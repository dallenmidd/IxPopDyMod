#' Helper for formatting vectors or lists in error messages
#' @param v A vector-like input
#' @param max Maximum number of elements of `v` to include in output
#' TODO document other params
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
#' @return If x is a double whose value is equal to an integer, return the
#' equivalent integer. Otherwise, return x
#' @noRd
ensure_int <- function(x) {
  if (is.double(x) && all((x == as.integer(x)) | (is.na(x)))) {
    return(stats::setNames(as.integer(x), names(x)))
  }
  x
}
