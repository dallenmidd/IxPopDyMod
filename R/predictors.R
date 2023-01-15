#' `predictors` constructor
#' @inheritParams predictors
#' @returns a `predictors` object
#' @noRd
new_predictors <- function(df) {
  checkmate::assert_data_frame(df)

  # check column names
  checkmate::assert_names(
    colnames(df),
    permutation.of = c("pred", "pred_subcategory", "j_day", "value"),
    what = "colnames"
  )

  # check column types
  checkmate::assert_character(df$pred, any.missing = FALSE, min.chars = 1)
  checkmate::assert_character(df$pred_subcategory, min.chars = 1)
  checkmate::assert_integer(df$j_day, lower = 0)
  checkmate::assert_numeric(df$value, any.missing = FALSE)

  structure(df, class = c("predictors", class(df)))
}

#' Validate `predictors`
#'
#'
validate_predictors <- function(preds) {

  # TODO if a pred has subcategories, it must have the same subcategories for each day

  # TODO each pred/pred_subcategory group must either
  # - have a NA jday and have exactly one row
  # - have all the jdays in the range

  return(preds)
}

#' Create a table of `predictors`
#'
#' A data frame of input data to be used in as predictor values in transition
#' functions.
#'
#' @param df input data frame, with columns:
#' \describe{
#'   \item{pred}{String specifying the name of the predictor, e.g. "temp" or
#'   "host_den"}
#'   # TODO use of `pred_subcategory` is messy, especially the dependence on order
#'   \item{pred_subcategory}{This column allows specifying predictors for which
#'   there are multiple values for a given j_day. Predictor values are sorted by
#'   this column in the config set up. This ensures that when accessing a
#'   predictor with multiple values for the same j_day, we get a vector of
#'   predictor values ordered by this column. A typical use for this column is
#'   to specify the host density of each host species.}
#'   \item{j_day}{Integer specifying the Julian day, or NA for predictors with
#'   constant value over time}
#'   \item{value}{Numeric value of predictor}
#' }
#'
#' @returns a `predictors` object
#' @export
predictors <- function(df) {

  # coerce j_day column to integer (from double)
  if (is.data.frame(df) && utils::hasName(df, "j_day")) {
    df$j_day <- ensure_int(df$j_day)
  }

  validate_predictors(new_predictors(df))
}
