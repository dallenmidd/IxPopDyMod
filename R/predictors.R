#' `predictors` constructor
#' @inheritParams predictors
#' @returns a `predictors` object
#' @noRd
new_predictors <- function(df) {
  checkmate::assert_data_frame(df, min.rows = 1)

  # check column names
  checkmate::assert_names(
    colnames(df),
    must.include = c("pred", "pred_subcategory", "j_day", "value"),
    what = "colnames"
  )

  # check column types
  checkmate::assert_character(df$pred, any.missing = FALSE, min.chars = 1)
  checkmate::assert_character(df$pred_subcategory, min.chars = 1)
  checkmate::assert_integer(df$j_day, lower = 0)
  checkmate::assert_numeric(df$value, any.missing = FALSE)

  structure(df, class = unique(c("predictors", class(df))))
}

#' Validate `predictors`
#'
#' @param df the `predictors` object to validate
#' @returns the input, if checks pass
#' @noRd
validate_predictors <- function(df) {
  assert_predictors_are_ordered(df)
  for (pred in unique(df$pred)) {
    subset <- df[df[["pred"]] == pred, ]
    assert_jday_entirely_na_or_numeric(subset = subset, pred = pred)
    if (all(!is.na(subset$j_day))) {
      # condition with no NA j_days, meaning predictor is variable over time
      max_day <- max(df$j_day, na.rm = TRUE)
      assert_predictor_extends_up_to_max_day(
        subset = subset, pred = pred, max_day = max_day
      )
    }
  }
  df
}

assert_predictors_are_ordered <- function(df) {
  actual_order <- order(df$j_day, df$pred, df$pred_subcategory, na.last = FALSE)
  expected_order <- seq_len(nrow(df))
  if (!all(actual_order == expected_order)) {
    stop(
      "predictors must be ordered by `j_day`, `pred`, then `pred_subcategory` columns",
      call. = FALSE
    )
  }
}

assert_jday_entirely_na_or_numeric <- function(subset, pred) {
  na_days <- is.na(subset$j_day)
  if (any(na_days)) {
    # condition with constant predictor
    if (!(all(na_days))) {
      stop(
        "The `j_day` column for each `pred` must be entirely NA (indicating ",
        "a constant value) or entirely non-NA (variable value), but ",
        "the `pred` '", pred, "' has a mix of NA and non-NA `j_day` values.",
        call. = FALSE
      )
    }
  }
}

assert_predictor_extends_up_to_max_day <- function(subset, pred, max_day) {
  # each subcategory's j_day range must reach to the max day for any predictor
  for (subcategory in unique(subset$pred_subcategory)) {
    subcategory_subset <- subset[
      (subset[["pred_subcategory"]] == subcategory) |
        (is.na(subset[["pred_subcategory"]]) & is.na(subcategory)),
    ]
    actual_days <- subcategory_subset$j_day
    expected_days <- 1:max_day
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



#' Create a table of `predictors`
#'
#' A data frame of input data to be used in as predictor values in transition
#' functions.
#'
#' @param df input data frame, with columns:
#' \describe{
#'   \item{pred}{String specifying the name of the predictor, e.g. "temp" or
#'   "host_den"}
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
#' @param verbose Boolean; whether to warn about reordering the `df`
#' @returns a `predictors` object
#' @export
predictors <- function(df, verbose = FALSE) {

  # coerce j_day column to integer (from double)
  if (is.data.frame(df) && utils::hasName(df, "j_day")) {
    df$j_day <- ensure_int(df$j_day)
  }

  # sort predictors by j_day, pred, then pred_subcategory
  if (is.data.frame(df) && all(utils::hasName(df, c("j_day", "pred", "pred_subcategory")))) {
    if (verbose) {
      message("Ordering predictors by `j_day`, `pred`, then `pred_subcategory` columns")
    }
    df <- df[order(df$j_day, df$pred, df$pred_subcategory, na.last = FALSE), ]
    rownames(df) <- seq_len(nrow(df))
  }

  validate_predictors(new_predictors(df))
}


#' Get the predictor options for a `transition` based on a `predictors` table
#'
#' In addition to the options returned by this function, `transition`s can use
#' a string that matches one or more tick life stage names via regex, in order
#' to use tick density as a predictor value.
#'
#' @param df A `predictors` object
#' @returns A character vector
#' @noRd
valid_predictors_from_table <- function(df) {
  unique(df$pred)
}

#' Return whether input contains any predictors that are variable over time
#'
#' @param df A `predictors` object
#' @returns boolean
#' @noRd
predictor_data_varies_over_time <- function(df) {
  !all(is.na(df$j_day))
}

#' Return the max `j_day` in a `predictors` table
#'
#' @param df A `predictors` object
#' @returns An integer, or `-Inf` if the `j_day` column is entirely NA
#' @noRd
max_day_in_predictors_table <- function(df) {
  max(df$j_day, na.rm = TRUE)
}
