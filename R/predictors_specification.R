#' Specify what predictors to use in a transition and how to use that data
#'
#' @param named list of `predictor_specification_node`s
new_predictors_specification <- function(...) {
  l <- structure(
    list(...),
    class = "predictors_specification"
  )

  checkmate::assert_list(l, types = "predictors_specification_node", unique = TRUE, names = "unique")

  return(l)
}



#' Specify how a single predictor should be used
new_predictors_specification_node <- function(name, first_day_only) {

  checkmate::assert_string(name, min.chars = 1)
  checkmate::assert_logical(first_day_only, len = 1, any.missing = FALSE)

  structure(
    list(name = name, first_day_only = first_day_only),
    class = "predictors_specification_node"
  )
}
