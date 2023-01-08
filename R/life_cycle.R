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
  transitions <- lapply(
    seq_along(transitions),
    function(i) coerce_transition(index = i, transitions = transitions)
  )

  # convert list to `life_cycle` class
  cycle <- do.call(new_life_cycle, transitions)

  validate_life_cycle(cycle)
}


#' Attempt to coerce an input to a `transition`, first ensuring that the
#' required elements of a transition are provided.
coerce_transition <- function(index, transitions) {
  each_transition <- transitions[[index]]
  expected_args <- names(formals(transition))
  actual_args <- as.character(names(each_transition))
  valid <- checkmate::assert_set_equal(
    actual_args,
    expected_args,
    .var.name = paste("elements of transition at index:", index)
  )
  do.call(transition, each_transition)
}
