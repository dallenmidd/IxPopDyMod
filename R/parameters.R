#' Parameters constructor
#' @inheritParams parameters
#' @noRd
#' @returns a parameters object
new_parameters <- function(...) {

  parameters <- structure(list(...), class="parameters")

  if (length(parameters) > 0) {
    checkmate::assert_names(names(parameters), type = "unique")
  }

  # TODO need to figure out how to use vector names for matching parameters
  # to host species density predictor values
  for (param in parameters) {
    checkmate::assert_numeric(param, min.len = 1L)
    if (length(param) > 1) {
      checkmate::assert_names(names(param), type = "unique")
      if (!all(names(param) == sort(names(param)))) {
        stop("Vector parameters must be in alphabetical order", call. = FALSE)
      }
    } else if (!is.null(names(param))) {
      stop(
        "Scalar parameters must not have names. Found name: `", names(param), "`",
        call. = FALSE
      )
    }
  }

  return(parameters)

}

#' Create a set of parameters
#'
#' @param ... A set of named numeric vectors, each corresponding to a parameter.
#'   If a parameter is of length > 1, each element must be named.
#' @returns a `parameters` object
#' @export
#' @examples
#' # create a set of scalar parameters
#' parameters(a = 1, b = 2)
#'
#' # parameters of length > 1 may be useful for host-related parameters that
#' # differ between host species, for example tick feeding success
#' parameters(a = 1, feeding_success = c(deer = 0.49, squirrel = 0.17))
parameters <- function(...) {
  # Sort any vector parameters by their names in alphabetical order
  params <- lapply(list(...), sort_vector_by_names)
  do.call(new_parameters, params)
}

sort_vector_by_names <- function(v) {
  if (is.null(names(v))) {
    return(v)
  }
  v[sort(names(v))]
}
