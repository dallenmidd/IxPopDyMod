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
  # TODO `life_cycle` is simply a list of transitions. This is where we'd add
  # the specific logic that should apply to a group of transitions/a life cycle
  #
  # 1. Do transitions form an actual cycle?
  assert_transitions_form_a_cycle(cycle)
  # 2. There can only be 1 or 0 mortality type transitions from each stage
  assert_max_one_mortality_from_each_stage(cycle)
  # 3. Corresponding transitions and mortality transitions must have same
  #    `transition_type`
  assert_consistent_transition_types(cycle)

  return(cycle)
}

assert_transitions_form_a_cycle <- function(cycle) {}
assert_max_one_mortality_from_each_stage <- function(cycle) {}
assert_consistent_transition_types <- function(cycle) {}

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

#' Attempt to coerce an input to a `transition`
#'
#' First ensures that the required elements of a transition are provided, and
#' throws a more informative error than the missing argument error that would
#' otherwise be thrown by `do.call()`.
#'
#' @param index which item in the list to validate
#' @param transitions a list of (not yet validated) transitions
#' @returns a validated `transition`, if checks pass
#' @noRd
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
