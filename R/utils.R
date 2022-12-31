#' Return whether all elements of an object are named
#' @returns bool
#' @noRd
all_elements_are_named <- function(obj) {
  !("" %in% allNames(obj))
}

#' Return whether an object has any duplicate names
#' @returns bool
#' @noRd
has_duplicate_names <- function(obj) {
  any(duplicated(names(obj)))
}
