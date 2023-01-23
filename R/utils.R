#' Helper for formatting vectors or lists in error messages
#' @param v A vector-like input
#' @param max Maximum number of elements of `v` to include in output
#' @returns Input formatted as a string
#' @noRd
to_short_string <- function(v, max = 3) {
  l <- length(v)
  string <- paste(v[1:min(max, l)], collapse = ", ")
  paste0(
    string,
    ifelse(l > max,
           paste("... and", l - max, "more values"),
           ""
    )
  )
}
