new_predictors_spec <- function(...) {
  l <- structure(list(...), class = "predictors_spec")
  checkmate::assert_list(
    l, types = "predictors_spec_node", unique = TRUE, names = "unique"
  )
  return(l)
}

#' Specify what predictors to use in a transition and how to use that data
#'
#' @param ... named sequence of `predictor_specification_node`s. The names are
#'   matched with the formal args to the containing \code{\link{transition}}'s
#'   `fun` to determine which input in `fun` each predictor will be passed to.
#' @returns a `predictors_spec` object
#' @export
predictors_spec <- function(...) {
  nodes <- list(...)

  # Allow passing elements that are missing the `predictor_specification_node`
  # class attribute, but are otherwise valid
  node_names <- names(nodes)
  nodes <- lapply(
    seq_along(nodes),
    function(i) coerce_element(
      index = i,
      list_of_element = nodes,
      element_fun = new_predictors_spec_node
    )
  )
  names(nodes) <- node_names

  do.call(new_predictors_spec, nodes)
}

# TODO implement
# print.predictors_spec <- function()

new_predictors_spec_node <- function(name, first_day_only) {
  checkmate::assert_string(name, min.chars = 1)
  checkmate::assert_logical(first_day_only, len = 1, any.missing = FALSE)
  structure(
    list(name = name, first_day_only = first_day_only),
    class = "predictors_spec_node"
  )
}

#' Specify how a single predictor should be used
#' @param name String indicating where to get predictor data. Can be one of:
#'  - A string in the `"pred"` column in the \code{\link{predictors}} table.
#'    In this case, the predictor value passed to the containing
#'    \code{\link{transition}}'s `fun` is the corresponding value of that
#'    predictor in the table.
#'  - A string that matches at least one life stage name via regex. In this
#'    case, the value passed to the containing \code{\link{transition}}'s `fun`
#'    is the sum of the population sizes of all matched life stages.
#' @param first_day_only Boolean indicating whether to repeat the predictor data
#'   value from the first day of a \code{\link{transition}} when evaluating it
#'   (`TRUE` case), or to use the range of predictor data over the duration of a
#'   transition (`FALSE` case). `FALSE` is only valid for \code{\link{transition}}s
#'   with `"duration"` as the `transition_type`, because `"probability"` type
#'   transitions only last one day. A value of `FALSE` also requires the `name`
#'   parameter to be a value in the \code{\link{predictors}} table `"pred"`
#'   column, not a tick life stage.
#'
#' @export
#'
#' @returns a `predictors_spec_node` list-based object
predictors_spec_node <- function(name, first_day_only) {
  new_predictors_spec_node(name = name, first_day_only = first_day_only)
}
