new_transition <- function(
  from, to, transition_type, mortality_type, fun, predictors, parameters
) {

  # TODO should to and from be factors?
  stopifnot(is.character(from))
  stopifnot(length(from) == 1)
  stopifnot(nchar(from) > 0)

  stopifnot(is.na(to) || is.character(to))
  stopifnot(length(to) == 1)
  if (is.character(to)) {
    stopifnot(nchar(to) > 0)
  }

  stopifnot(inherits(transition_type, "transition_type"))
  stopifnot(inherits(mortality_type, "mortality_type"))

  if (
    !is.na(mortality_type) &&
    mortality_type == "throughout_transition" &&
    transition_type == "probability"
  ) {
    stop(
      "`probability` transitions only support `per_day` mortality",
      .call = FALSE
    )
  }

  # TODO move to helper?
  stopifnot(
    (!is.na(to) && is.na(mortality_type)) ||
      (is.na(to) && !is.na(mortality_type))
  )

  # TODO use a custom transition function class?
  stopifnot(is.function(fun))

  # a vector of predictor names
  # TODO should it be named?
  stopifnot(is.character(predictors))

  stopifnot(inherits(parameters, "parameters"))

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
  !is.na(transition$mortality_type)
}

#' Construct a transition type
new_transition_type <- function(type_name) {
  options <- c("probability", "duration")

  stopifnot(is.character(type_name))
  stopifnot(type_name %in% options)
  stopifnot(length(type_name) == 1)

  result <- structure(
    type_name,
    class = "transition_type"
  )

  return(result)
}


#' Construct a mortality type
new_mortality_type <- function(type_name) {
  options <- c("per_day", "throughout_transition")

  stopifnot(is.na(type_name) || is.character(type_name))
  stopifnot(is.na(type_name) || (type_name %in% options))
  stopifnot(length(type_name) == 1)

  result <- structure(
    type_name,
    class = "mortality_type"
  )
  return(result)
}
