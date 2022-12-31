new_transition <- function(
  from, to, type, mortality_type, fun, predictors, parameters
) {

  # TODO should to and from be factors?
  stopifnot(is.character(from))
  stopifnot(length(from) == 1)

  stopifnot(is.character(to))
  stopifnot(length(to) == 1)

  stopifnot(is.factor(type))
  stopifnot(length(type) == 1)
  stopifnot(levels(type) == c("probability", "duration"))

  stopifnot(is.factor(mortality_type))
  stopifnot(length(mortality_type) == 1)
  stopifnot(levels(mortality_type) == c("per_day", "throughout_transition"))

  # TODO use a custom transition function class?
  stopifnot(is.function(fun))

  # a vector of predictor names
  # TODO should it be named?
  stopifnot(is.character(predictors))

  stopifnot(class(parameters) == "parameters")

  transition <- structure(
    list(
      from = from,
      to = to,
      type = type,
      mortality_type = mortality_type,
      fun = fun,
      predictors = predictors,
      parameters = parameters
    ),
    class = "transition"
  )

  return(transition)
}


transition_is_mortality <- function(transition) {
  !is.na(transition$mortality_type)
}
