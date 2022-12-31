#' Parameters constructor
#' @noRd
#' @returns a parameters object
new_parameters <- function(...) {

  parameters <- structure(list(...), class="parameters")

  stopifnot(all_elements_are_named(parameters))
  stopifnot(!has_duplicate_names(parameters))

  # Each parameter can be either a numeric vector of length 1, or a named
  # numeric vector of length > 1.
  # TODO need to figure out how to use vector names for matching parameters
  # to host species density predictor values
  for (param in parameters) {
    stopifnot(is.numeric(param))
    stopifnot(length(param) >= 1)
    if (length(param) > 1) {
      stopifnot(all_elements_are_named(param))
      stopifnot(!has_duplicate_names(param))
    }
  }

  return(parameters)

}
