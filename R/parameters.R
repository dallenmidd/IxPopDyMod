# TODO do we need a public interface to creating parameters?
#' Parameters constructor
#' @param ... A set of named numeric vectors
#' @noRd
#' @returns a parameters object
new_parameters <- function(...) {

  parameters <- structure(list(...), class="parameters")

  if (length(parameters) > 0) {
    checkmate::assert_names(names(parameters), type = "unique")
  }

  # Each parameter can be either a numeric vector of length 1, or a named
  # numeric vector of length > 1.
  # TODO need to figure out how to use vector names for matching parameters
  # to host species density predictor values
  for (param in parameters) {
    checkmate::assert_numeric(param, min.len = 1L)
    if (length(param) > 1) {
      checkmate::assert_names(names(param), type = "unique")
    }
  }

  return(parameters)

}
