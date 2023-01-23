to_short_string <- function(v, max = 3) {
  l <- length(v)
  string <- paste(v[1:min(max, l)],
    collapse = ", "
  )
  paste0(
    string,
    ifelse(l > max,
      paste("... and", l - max, "more values"),
      ""
    )
  )
}

#' Check that a `config` object is valid
#' @return Returns the input config object if it passes all the checks
#' @noRd
validate_config <- function(cfg) {

  # Note that we only test that max day in predictors >= steps + max_duration,
  # because the predictors class handles the check that days form a continuous
  # sequence starting at day 1, and that all predictors extend to the same day.
  # We add max_duration out of caution, to ensure that there is predictors data
  # for any day between steps:(steps + max_duration), when we might evaluate a
  # transition, even though the model output only extends to `steps` days.
  if (!is.null(cfg$preds) && predictor_data_varies_over_time(cfg$preds)) {
    # the days that predictor data should, and actually, extend to
    expected_max_day <- cfg$steps + cfg$max_duration
    actual_max_day <- max_day_in_predictors_table(cfg$preds)
    if (actual_max_day < expected_max_day) {
      stop(
        "Predictor data should extend to at least ", expected_max_day,
        ", the sum of `max_duration` and `steps`, but the final day in the ",
        "predictor data is ", actual_max_day,
        call. = FALSE
      )
    }
  }

  # # TODO need some checks on the relationships between transitions and
  # # predictors data. And that each argument to each transition_function has a
  # # corresponding parameter or predictor.
  # test_predictors(cfg$transitions, cfg$predictors)

  valid_life_stages <- life_stages(cfg$cycle)
  initial_life_stages <- names(cfg$initial_population)

  if (!all(initial_life_stages %in% valid_life_stages)) {
    stop(
      "`initial_population` had names that are not valid life stages: ",
      paste(
        setdiff(initial_life_stages, valid_life_stages),
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  if (!any(cfg$initial_population > 0)) {
    stop(
      "`initial_population` must be greater than 0 for at least one life stage",
      call. = FALSE
    )
  }

  # test_transition_values(cfg)

  cfg
}
