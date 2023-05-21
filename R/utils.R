#' Helper for formatting vectors or lists in error messages
#' @param v A vector-like input
#' @param max Maximum number of elements of `v` to include in output
#' @param collapse a character string to separate the elements of `v`
#' @param item_name a character string with the name of what the elements
#'    of `v` are
#' @returns Input formatted as a string
#' @noRd
to_short_string <- function(v, max = 3, collapse = ", ", item_name = "values") {
  l <- length(v)
  string <- paste(v[1:min(max, l)], collapse = collapse)
  paste0(
    string,
    ifelse(l > max,
           paste("... and", l - max, "more", item_name),
           ""
    )
  )
}

#' Convert doubles to integers and preserve names
#'
#' @param x A double
#' @returns If x is a double whose value is equal to an integer, return the
#' equivalent integer. Otherwise, return x
#' @noRd
ensure_int <- function(x) {
  if (is.double(x) && all((x == as.integer(x)) | (is.na(x)))) {
    return(stats::setNames(as.integer(x), names(x)))
  }
  x
}

#' Attempt to coerce a list-based element within a parent list to the correct type
#'
#' First ensures that the element has the required names, and throws a more
#' informative error than the missing argument error that would otherwise be
#' thrown by `do.call()`.
#'
#' @param index which element in the list to validate
#' @param list_of_element a list of (not yet validated) elements
#' @param element_fun function to use to validate the provided element
#' @returns a validated element of type `element_fun`, if checks pass
#' @noRd
coerce_element <- function(index, list_of_element, element_fun) {
  each_elt <- list_of_element[[index]]
  expected_args <- names(formals(element_fun))
  actual_args <- as.character(names(each_elt))
  checkmate::assert_set_equal(
    actual_args,
    expected_args,
    .var.name = paste("names of element at index:", index)
  )
  do.call(element_fun, each_elt)
}
