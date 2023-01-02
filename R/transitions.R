new_transition <- function(
  from, to, transition_type, mortality_type, fun, predictors, parameters
) {

  checkmate::assert_string(from, min.chars = 1)
  checkmate::assert_string(to, min.chars = 1, null.ok = TRUE)
  checkmate::assert_choice(transition_type, c("probability", "duration"))
  checkmate::assert_choice(
    mortality_type, c("per_day", "throughout_transition"), null.ok = TRUE
  )
  checkmate::assert_class(fun, "transition_function")
  # a vector of predictor names - TODO should it be named?
  checkmate::assert_character(predictors)
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

  return(transition)
}



#' Return whether a transition is mortality or a transition between life stages
transition_is_mortality <- function(transition) {
  !is.null(transition$mortality_type)
}
