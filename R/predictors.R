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
#' @param df the `predictors` object to validate
#' @returns the input, if checks pass
#' @noRd
validate_predictors <- function(df) {

  # get the maximum day for any predictor
  if (!all(is.na(df$j_day))) {
    max_day <- max(df$j_day, na.rm = TRUE)
    expected_days <- 1:max_day
  }

  for (pred in unique(df$pred)) {

    subset <- dplyr::filter(df, pred == pred)
    na_days <- is.na(subset$j_day)

    if (any(na_days)) {
      if (!(all(na_days))) {
        # constant predictor
        stop(
          "The `j_day` column for each `pred` must be entirely NA (indicating ",
          "a constant value) or entirely non-NA (variable value), but ",
          "the `pred` '", pred, "' has a mix of NA and non-NA `j_day` values.",
          call. = FALSE
        )
      }
    } else {
      # condition with no NA j_days, meaning predictor is variable over time
      # each subcategory's j_day range must be set equal to 1:max_day
      for (subcategory in unique(subset$pred_subcategory)) {
        subcategory_subset <- dplyr::filter(
          df,
          (pred_subcategory == subcategory) |
            (is.na(pred_subcategory) & is.na(subcategory))
        )
        actual_days <- subcategory_subset$j_day
        if (!setequal(expected_days, actual_days)) {
          missing_days <- setdiff(expected_days, actual_days)
          stop(
            "Variable predictors must have a row for each `j_day` between 1 ",
            "and the final day when there is any predictor data, but ",
            "the `pred` '", pred, "' and the subcategory '", subcategory,
            "' is missing day(s): ", to_short_string(missing_days),
            call. = FALSE
          )
        }
      }
    }
  }

  return(df)
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