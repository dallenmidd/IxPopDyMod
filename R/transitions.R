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

  # TODO move to helper?
  if (
    !is.null(mortality_type) &&
    mortality_type == "throughout_transition" &&
    transition_type == "probability"
  ) {
    stop(
      "`probability` transitions only support `per_day` mortality",
      call. = FALSE
    )
  }

  # TODO move to helper?
  if (
    (is.null(to) && is.null(mortality_type)) ||
    (!is.null(to) && !is.null(mortality_type))
  ) {
    stop(
      "exactly 1 of `to` or `mortality_type` must be non-NULL",
      call. = FALSE
    )
  }

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


#' Return whether a transition is mortality or a transition between life stages
transition_is_mortality <- function(transition) {
  !is.null(transition$mortality_type)
}
