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
  checkmate::assert_character(predictors, null.ok = TRUE)
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

  if (
    (is.null(transition$to) && is.null(transition$mortality_type)) ||
    (!is.null(transition$to) && !is.null(transition$mortality_type))
  ) {
    stop(
      "exactly 1 of `to` or `mortality_type` must be non-NULL",
      call. = FALSE
    )
  }

  # arguments to transition function align with parameters and predictors
  checkmate::assert_set_equal(
    names(transition$parameters), get_parameter_names(transition$fun)
  )
  checkmate::assert_set_equal(
    transition$predictors, get_predictor_names(transition$fun)
  )

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
#' @param fun The \code{\link{transition_function}} to evaluate.
#' @param transition_type One of:
#'   `"probability"`: the evaluated transition is interpreted as the daily
#'     fraction of ticks that complete the transition.
#'   `"duration"`: the transition is complete on the first day that the
#'     cumulative sum of the evaluated transition is greater than or equal to 1.
#' @param mortality_type One of:
#'   `NULL`: the default, indicating that the transition is not mortality.
#'   `"per_day"`: indicates that the evaluated transition is the fraction of
#'     ticks that dies each day.
#'   `"throughout_transition"`: only valid for `"duration"` type transitions,
#'     where it indicates that the evaluated transition is the fraction of
#'     ticks that die throughout the entire transition.
#' @param predictors Character vector of predictor names to use in evaluating
#'   `fun`. Predictors are the data inputs to transition functions. Each element
#'     can be one of:
#'     - A string in the `"pred"` column in the \code{\link{predictors}} table.
#'       The predictor value passed to `fun` is the corresponding value of that
#'       predictor in the table.
#'     - A string that matches at least one life stage name via regex. The value
#'       passed to `fun` is the sum of the population sizes of all matched life
#'       stages.
#' @param parameters Optional, a \code{\link{parameters}} object, or a named
#'   list of numeric vectors.
#'
#' TODO clarify how passing works and order matters for predictors/parameters
#' IDEA for predictor passing - use a named vector, match args based on names
#' of vector and names of formals in transition funs, and determine what to pass
#' based on values of vector.
#'
#' TODO need to actually create the helper functions linked in these docs.
#'
#' @export
#'
#' @returns a `transition` object
transition <- function(
  from, to, fun, transition_type, mortality_type = NULL,
  predictors = get_predictor_names(fun),
  parameters = new_parameters()
) {

  # TODO these should be helpers that do any coercion
  fun <- new_transition_function(fun)

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
