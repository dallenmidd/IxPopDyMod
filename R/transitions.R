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
transition_is_mortality <- function(transition) {
  !is.null(transition$mortality_type)
}
