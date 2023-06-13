#' `config` constructor
#'
#' @description
#' Quickly create a new `config` object with minimal checks
#'
#' @inheritParams config
#'
#' @returns a `config` object
#'
#' @noRd
new_config <- function(
    cycle, initial_population, preds, steps, max_duration
  ) {

  # check that all types are correct
  checkmate::assert_class(cycle, "life_cycle")
  checkmate::assert_integer(
    initial_population,
    lower = 0,
    min.len = 1,
    names = "unique",
    any.missing = FALSE
  )
  checkmate::assert_class(preds, "predictors", null.ok = TRUE)
  checkmate::assert_integer(steps, lower = 0, len = 1, any.missing = FALSE)
  checkmate::assert_integer(
    max_duration, lower = 1, len = 1, any.missing = FALSE
  )

  structure(
    list(
      cycle = cycle,
      initial_population = initial_population,
      preds = preds,
      steps = steps,
      max_duration = max_duration
    ),
    class = "config"
  )
}

#' Create a `config` object
#'
#' @description
#' Make a `config` object from the input parameters, and ensure that the inputs
#' meet the requirements for the model. The returned object is a complete
#' description of a model run scenario.
#'
#' @param cycle A tick's \code{\link{life_cycle}} test
#' @param preds Optional input \code{\link{predictors}} data
#' @param steps Numeric vector of length one indicating the duration to run the
#'   model over in days.
#' @param max_duration Numeric vector of length one. Determines the maximum
#'   number of days that a duration-based transition can last, after which ticks
#'   are removed from the model/die. Default of 365 is likely sensible for most
#'   cases.
#' @param initial_population Named numeric vector indicating starting population
#'   for each life stage. Life stages not specified are assumed to be 0.
#' @param verbose Boolean; whether to warn about coercion to inputs
#'
#' @returns A `config` object
#'
#' @export
#'
#' @examples
#' # We build a simple example config
#' my_config <- config(
#'   cycle = life_cycle(
#'     transition("a", "b", function() 0.1, "probability"),
#'     transition("b", "a", function() 10, "probability")
#'   ),
#'   initial_population = c(a = 1)
#' )
#'
#' # If we make a change to an existing `config`, it is a good idea to check
#' # whether it is still valid by calling `config()` on it again. For example,
#' # here we set the initial_population of a life stage that is not included in
#' # the life cycle.
#' my_config$initial_population <- c(a = 1, c = 1)
#'
#' \dontrun{
#' # Now, we re-run the validations, which will throw an error
#' do.call(config, my_config)
#' }
#'
config <- function(
    cycle, initial_population, preds = NULL, steps = 365L, max_duration = 365L,
    verbose = TRUE
  ) {

  # coerce input types

  # life_cycle() will attempt to coerce each input to a valid `transition`
  cycle <- do.call(life_cycle, as.list(cycle))

  if (!is.null(preds)) {
    preds <- predictors(preds, verbose = verbose)
  }

  initial_population <- ensure_int(initial_population)
  steps <- ensure_int(steps)
  max_duration <- ensure_int(max_duration)

  # return validated config
  validate_config(
    new_config(
      cycle = cycle,
      initial_population = initial_population,
      preds = preds,
      steps = steps,
      max_duration = max_duration
    )
  )
}
