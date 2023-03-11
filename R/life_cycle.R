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
#' These checks ensure that the group of `transition`s in a `life_cycle` make
#' sense together.
#'
#' @param cycle a `life_cycle` to validate
#' @returns the input if it passes checks
#' @noRd
validate_life_cycle <- function(cycle) {
  assert_no_duplicate_transitions(cycle)
  assert_transition_accompanies_each_mortality(cycle)
  assert_only_one_transition_type_from_each_stage(cycle)
  assert_max_one_duration_transition_from_each_stage(cycle)
  # assert_transitions_form_a_cycle(cycle)
  return(cycle)
}

assert_no_duplicate_transitions <- function(cycle) {

  # create a list of (from, to) for each stage
  from_to <- lapply(cycle, function(x) x[c("from", "to")])

  # check if any elements are duplicated
  if (any(duplicated(from_to))) {
    stop(
      "There cannot be more than one transition between a pair of life stages",
      call. = FALSE
    )
  }
}

# TODO implement
# assert_transitions_form_a_cycle <- function(cycle) {}

assert_transition_accompanies_each_mortality <- function(cycle) {
  for (stage in life_stages(cycle)) {
    transitions <- query_transitions(cycle, field = "from", value = stage)
    non_mortality <- query_transitions_by_mortality(transitions, FALSE)
    if (length(non_mortality) == 0) {
      stop(
        "Life cycles must have at least one non-mortality transition from ",
        "each stage, but found only mortality transition(s) from stage ", stage,
        call. = FALSE
      )
    }
  }
}

assert_only_one_transition_type_from_each_stage <- function(cycle) {
  for (stage in life_stages(cycle)) {
    transitions <- query_transitions(cycle, field = "from", value = stage)
    transition_types <- vapply(
      transitions, function(x) x[["transition_type"]], FUN.VALUE = character(1)
    )
    if (length(unique(transition_types)) > 1) {
      stop(
        "All transitions from a given stage must have the same transition_type",
        call. = FALSE
      )
    }
  }
}

assert_max_one_duration_transition_from_each_stage <- function(cycle) {
  for (stage in life_stages(cycle)) {
    transitions <- cycle %>%
      query_transitions_by_mortality(mortality = FALSE) %>%
      query_transitions(field = "from", value = stage) %>%
      query_transitions(field = "transition_type", value = "duration")
    if (length(transitions) > 1) {
      stop(
        "Can only have one duration type transition from each life stage",
        call. = FALSE
      )
    }
  }
}

#' Create a `life_cycle` from a collection of `transition`s
#'
#' @param ... A set of \code{\link{transition}}s
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

# TODO life cycle is the longest printed part of a config, could implement an
# abbreviated printing method (print.life_cycle)

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
  checkmate::assert_set_equal(
    actual_args,
    expected_args,
    .var.name = paste("elements of transition at index:", index)
  )
  do.call(transition, each_transition)
}

#' Print a life cycle
#' @export
#' @param x A `life_cycle`
#' @param abbreviated logical, indicating whether to print an abbreviated
#'   representation or the entire list-based hierarchy
#' @param max number of transitions to print, or NULL to print all transitions
print.life_cycle <- function(x, ..., abbreviated = TRUE, max = 10L) {
  if (abbreviated) {
    transitions <- x %>%
      vapply(format.transition, character(1L)) %>%
      number_each_element() %>%
      to_short_string(max = max, collapse = "", item_name = "transitions")
    cat(
      "** A life cycle",
      "\n** Number of transitions: ", length(x),
      "\n** Unique life stages: ", paste(life_stages(x), collapse = ", "), "\n",
      transitions,
      sep = ""
    )
  } else {
    # print the default list representation
    print.default(x)
  }
}

# TODO move this to transition.R and combine with any print method Dave is
# working on. This is just a very simple option for use within a life_cycle
format.transition <- function(x, ...) {
  to <- ifelse(transition_is_mortality(x), "mortality", x[["to"]])
  paste(x[["from"]], "->", to, "\n")
}

number_each_element <- function(x) {
  nums <- seq_along(x)
  paste0(nums, ". ", x)
}



###########################################
# helpers for working with a `life_cycle` #
###########################################

#' Get `transition`s in a list where the `field` is `value`
#'
#' @param cycle a list of transitions
#' @param field name of the field to search in each `transition`
#' @param value value to search for in the specified field
#' @returns a subset of the input cycle
#' @noRd
query_transitions <- function(cycle, field, value) {
  search_fun <- function(each_transition) each_transition[[field]] == value
  Filter(search_fun, cycle)
}

#' Get either mortality or non-mortality `transition`s
#'
#' @param cycle a list of transitions
#' @param mortality boolean
#' @returns a subset of the input life cycle
#' @noRd
query_transitions_by_mortality <- function(cycle, mortality) {
  cycle[vapply(cycle, transition_is_mortality, logical(1)) == mortality]
}

#' Get the names of unique life stages
#'
#' @param cycle a list of transitions
#' @returns a character vector
#' @noRd
life_stages <- function(cycle) {
  from_fields <- vapply(cycle, function(x) x$from, FUN.VALUE = character(1))
  unique(from_fields)
}
