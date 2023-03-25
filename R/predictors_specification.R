#' Format a `predictor_spec`
#' @inheritParams print.predictor_spec
#' @returns string representation of input
#' @export
format.predictor_spec <- function(x, ...) {
  paste0(
    "** A predictor_spec",
    "\n** pred:           \"", x$pred, "\"",
    "\n** first_day_only: ", x$first_day_only, "\n"
  )
}

#' Print a `predictor_spec`
#' @param x a predictor_spec
#' @param ... not used
#' @export
print.predictor_spec <- function(x, ...) {
  cat(format(x))
}

new_predictor_spec <- function(pred, first_day_only) {
  checkmate::assert_string(pred, min.chars = 1)
  checkmate::assert_logical(first_day_only, len = 1, any.missing = FALSE)
  structure(
    list(pred = pred, first_day_only = first_day_only),
    class = "predictor_spec"
  )
}

#' Specify how a single predictor should be used
#' @param pred String indicating where to get predictor data. Can be one of:
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
#' @returns a `predictor_spec` list-based object
predictor_spec <- function(pred, first_day_only = TRUE) {
  new_predictor_spec(pred = pred, first_day_only = first_day_only)
}

get_preds_where_first_day_only_is_false <- function(spec) {
  vals <- lapply(spec, function(x) x[["first_day_only"]])
  spec[vals == FALSE]
}


pred_is_life_stage <- function(a_predictor_spec, stages) {
  any(grepl(a_predictor_spec$pred, stages))
}
