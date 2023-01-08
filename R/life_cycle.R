#' `life_cycle` constructor
#'
#' @inheritParams life_cycle
#' @returns a `life_cycle`
#' @noRd
new_life_cycle <- function(...) {

  cycle <- structure(
    list(...),
    class = "life_cycle"
  )

  checkmate::assert_list(cycle, types = "transition", unique = TRUE)

  return(cycle)

}

#' Validate a `life_cycle`
#'
#' @param cycle a `life_cycle` to validate
#' @returns the input if it passes checks
#' @noRd
validate_life_cycle <- function(cycle) {
  return(cycle)
}

#' Create a `life_cycle` from a collection of `transition`s
#'
#' @param ... A set of `transition`s
#' @returns a `life_cycle`
#' @export
life_cycle <- function(...) {

  transitions <- list(...)

  # attempt to coerce each input to a transition
  transitions <- lapply(transitions, function(x) do.call(transition, x))

  # convert list to `life_cycle` class
  cycle <- do.call(new_life_cycle, transitions)

  validate_life_cycle(cycle)
}
