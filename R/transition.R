#' Construct a transition
#'
#' @inheritParams transition
#' @returns a `transition` object
#' @noRd
new_transition <- function(
  from, to, fun, transition_type, mortality_type, predictors, parameters
) {

  checkmate::assert_string(from, min.chars = 1)
  checkmate::assert_string(to, min.chars = 1, null.ok = TRUE)
  checkmate::assert_class(fun, "transition_function")
  checkmate::assert_choice(transition_type, c("probability", "duration"))
  checkmate::assert_choice(
    mortality_type, c("per_day", "throughout_transition"), null.ok = TRUE
  )
  checkmate::assert_list(
    predictors, types = "predictor_spec", unique = TRUE, names = "unique",
    null.ok = TRUE
  )
  checkmate::assert_class(parameters, "parameters")

  transition <- structure(
    list(
      from = from,
      to = to,
      transition_type = transition_type,
      mortality_type = mortality_type,
      fun = fun,
      predictors = predictors,
      parameters = parameters
    ),
    class = "transition"
  )

  return(transition)
}

#' Check that a `transition` object is valid
#'
#' @param transition input to validate
#' @returns the input if it passes all checks
#' @noRd
validate_transition <- function(transition) {

  if (
    !is.null(transition$mortality_type) &&
    transition$mortality_type == "throughout_transition" &&
    transition$transition_type == "probability"
  ) {
    stop(
      "`probability` transitions only support `per_day` mortality",
      call. = FALSE
    )
  }

  if (is.null(transition$to) == is.null(transition$mortality_type)) {
    stop(
      "exactly 1 of `to` or `mortality_type` must be non-NULL",
      call. = FALSE
    )
  }

  # there is no overlap between parameter and predictor names
  checkmate::assert_disjunct(
    names(transition$parameters), names(transition$predictors)
  )

  # each argument to the function corresponds to a parameter or predictor
  checkmate::assert_set_equal(
    c(names(transition$parameters), names(transition$predictors)),
    names(formals(transition$fun))
  )

  if (transition$transition_type == "probability") {
    invalid <- get_preds_where_first_day_only_is_false(transition$predictors)
    if (length(invalid) > 0) {
      stop(
        "Probability type transitions cannot have any predictors where the ",
        "`first_day_only` attribute is `FALSE`. Found these exceptions:\n",
        paste0(names(invalid), ":\n", lapply(invalid, format)),
        call. = FALSE
      )
    }
  }

  return(transition)
}


#' Create a `transition` object
#'
#' A `transition` object represents a single transition between two tick life
#' stages, or the mortality rate from a life stage.
#'
#' @param from The name of the life stage a tick is transitioning from.
#' @param to The name of the life stage a tick is transitioning to, or NULL if
#'   the transition is representing mortality.
#' @param fun The transition function to evaluate. TODO further document or link
#' to documentation in class definition
#' @param transition_type One of:
#'   `"probability"`: the evaluated transition is interpreted as the daily
#'     fraction of ticks that complete the transition. Ticks remain in the
#'     original life stage if they do not complete a transition or undergo
#'     mortality.
#'   `"duration"`: the transition is complete on the first day that the
#'     cumulative sum of the evaluated transition is greater than or equal to 1.
#'     No ticks remain in the original life stage at the end of a transition -
#'     they either complete the transition or die.
#' @param mortality_type One of:
#'   `NULL`: the default, indicating that the transition is not mortality.
#'   `"per_day"`: indicates that the evaluated transition is the fraction of
#'     ticks that dies each day.
#'   `"throughout_transition"`: only valid for `"duration"` type transitions,
#'     where it indicates that the evaluated transition is the fraction of
#'     ticks that die throughout the entire transition.
#' @param predictors Optional, a named list of \code{\link{predictor_spec}} objects
#'   that specify how any predictor data should be used in evaluating `fun`. The names are
#'   matched with the formal args to  `fun` to determine which input in `fun`
#'   each predictor will be passed to.
#' @param parameters Optional, a \code{\link{parameters}} object, or a named
#'   list of numeric vectors.
#'
#' @export
#'
#' @returns a `transition` object
transition <- function(
  from, to, fun, transition_type, mortality_type = NULL, predictors = NULL,
  parameters = list()
) {

  fun <- new_transition_function(fun)

  # TODO note conflict between parameters() the function and parameters the
  # formal argument for this function. A few design notes:
  # - Could not export parameters(), just use new_parameters(). Might make
  #   sense since parameters() currently adds no additional functionality.
  # - More broadly, can think about whether it makes sense to have arguments
  #   like life_cycle in config, parameters here in transition, named the same
  #   thing as the s3 helpers that create them. Could differentiate using
  #   IxPopDyMod:: like here, but maybe that's extra complication?
  parameters <- do.call(new_parameters, as.list(parameters))

  validate_transition(new_transition(
    from = from,
    to = to,
    transition_type = transition_type,
    mortality_type = mortality_type,
    fun = fun,
    predictors = predictors,
    parameters = parameters
  ))
}



#' Return whether a transition is mortality or a transition between life stages
#'
#' @param transition a `transition` object
#' @returns a boolean
#' @noRd
transition_is_mortality <- function(transition) {
  !is.null(transition$mortality_type)
}


# This is just a very simple option for use within a life_cycle
format.transition <- function(x, ...) {
  to <- ifelse(transition_is_mortality(x), "mortality", x[["to"]])
  paste(x[["from"]], "->", to, "\n")
}

#' Print a transition
#' @export
#' @param ... not used
#' @param x A `transition`

print.transition <- function(x, ...) {
  param_names <- names(x$parameters)
  param_string <- ""
  for (i in param_names) param_string <- paste(param_string, i, " = ",  x$parameters[i], ", ", sep = "")
  param_string <- sub(", $", "", param_string)

  pred_names <- names(x$predictors)
  pred_string <- ""
  for (i in pred_names) pred_string <- paste(pred_string, i, " = ",  x$predictors[i], ", ", sep = "")
  pred_string <- sub(", $", "", pred_string)

  function_string <- sub('structure\\(function ',"", deparse(x$fun))
  function_string <- sub(', class = "(.+)"\\)$',"",function_string)

  cat(
    "** A transition",
    "\n** ", format.transition(x),
    "Transition type: ", x$transition_type,
    ifelse(transition_is_mortality(x), "\nMortality type: ", ""),
    ifelse(transition_is_mortality(x), x$mortality_type, ""),
    ifelse(!is.null(x$predictors),"\nPredictors: ",""),
    ifelse(!is.null(x$predictors),pred_string,""),
    "\nParameters: ", param_string,
    "\nFunction: ",
    function_string,
    sep = ""
  )
}


#' Helper function for validation. It's an issue if this case is met.
#'
#' @param transition a `transition` object
#' @param stages character vector of life stage names
#' @returns a boolean
#' @noRd
transition_uses_tick_den_predictor_with_first_day_only_false <- function(
  transition, stages
) {
  preds_with_errors <- lapply(
    transition$predictors,
    function(x) pred_is_life_stage(x, stages = stages) && !x[["first_day_only"]]
  )
  any(unlist(preds_with_errors))
}
