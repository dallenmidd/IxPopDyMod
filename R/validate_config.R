set_all_names <- function(name) {
  function(v) {
    if (!is.null(v)) {
      names(v) <- rep(name, length(v))
      v
    }
  }
}

set_all_names_x <- set_all_names("x")
set_all_names_i <- set_all_names("i")


#' @param error_header String
#' @param problem_list List of strings describing each problem
#' @noRd
print_errors <- function(error_header, problem_list, hints = NULL,
                         max_problems = 3L) {
  n_problems <- length(problem_list)

  if (n_problems > 0) {
    error_header <- paste(
      stringr::str_squish(error_header),
      "\n"
    )

    problems_to_print <-
      problem_list[seq_len(min(n_problems, max_problems))] %>%
      set_all_names_x() %>%
      rlang::format_error_bullets()

    error_footer <- ifelse(
      n_problems > max_problems,
      paste("\n... and", n_problems - max_problems, "more problems\n"),
      "\n"
    )

    hints <- hints %>%
      stringr::str_squish() %>%
      set_all_names_i() %>%
      rlang::format_error_bullets()

    stop(
      error_header,
      problems_to_print,
      error_footer,
      hints,
      call. = FALSE
    )
  }
}

error_df_to_list <- function(error_df, row_to_string) {
  if (nrow(error_df) > 0) {
    apply(error_df, 1, row_to_string)
  }
}

test_predictors <- function(transitions, predictors) {
  # return whether a predictor value is supported by get_pred()
  predictor_is_valid <- function(pred) {
    valid_strings <- unique(predictors$pred)

    life_stages <- get_life_stages(transitions)

    is.na(pred) ||
      pred %in% valid_strings ||
      any(stringr::str_detect(life_stages, pred))
  }

  error_tibble <- transitions[c("pred1", "pred2")] %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    tidyr::pivot_longer(cols = c("pred1", "pred2"))

  error_tibble$valid <- unlist(lapply(error_tibble$value, predictor_is_valid))

  error_tibble <- error_tibble %>%
    filter(!(.data$valid))

  row_to_string <- function(row) {
    paste0(
      "\"", row["value"], "\" in row ", row["row"],
      ", column `", row["name"], "`"
    )
  }

  errors <- error_df_to_list(error_tibble, row_to_string)
  print_errors(
    "Unsupported predictor values found in `transitions`:", errors
  )
}


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


test_transition_values <- function(cfg) {
  life_stages <- get_life_stages(cfg$transitions)

  # initialize a population matrix with 10 of each tick life_stage on day 1
  # and an empty N_developing matrix
  population <- matrix(
    nrow = length(life_stages),
    ncol = cfg$steps + cfg$max_delay,
    data = 0
  )
  rownames(population) <- life_stages
  developing_population <- population
  population[, 1] <- 10

  funs <- add_params_list(cfg$transitions, cfg$parameters)

  is_valid <- function(transition_value) {
    all(!is.na(transition_value) &
      is.numeric(transition_value) &
      transition_value >= 0)
  }

  transition_values <-
    sapply(
      seq_len(nrow(funs)),
      function(row_index) {
        get_transition_val(
          time = 1,
          transition_row_with_parameters = funs[row_index, ],
          population = population,
          developing_population = developing_population,
          max_delay = cfg$max_delay,
          life_stages = life_stages,
          predictors = cfg$predictors
        )
      }
    )

  transitions_are_valid <-
    sapply(
      transition_values,
      is_valid
    )

  if (!all(transitions_are_valid)) {
    funs$valid <- transitions_are_valid
    funs$value <- transition_values

    invalid_funs <- funs %>%
      dplyr::mutate(row_number = dplyr::row_number()) %>%
      dplyr::filter(!.data$valid)

    row_to_string <- function(row) {
      paste0(
        "`", row[["transition_fun"]], "` in row ", row[["row_number"]],
        " evaluates to ", to_short_string(row[["value"]])
      )
    }

    errors <- error_df_to_list(invalid_funs, row_to_string)
    print_errors(
      "Transitions must evaluate to numeric values greater than zero. Found
        exceptions:",
      errors
    )
  }
}


#' Check that a `config` object is valid
#' @return Returns the input config object if it passes all the checks
#' @noRd
validate_config <- function(cfg) {

  # Note that we only need to test that max day in predictors is >= to
  # steps + max_delay, because predictors() handles the check that days
  # form a continuous sequence starting at day 1, and that all predictors
  # extend to the same day.

  # TODO check this logic... does it matter if cumsum doesn't reach 1 if it's
  # after the steps are complete?
  # We add max_delay because if input data is missing for time in between
  # steps and steps+max_delay, we could incorrectly get delay transitions that
  # do not cumsum to 1.
  if (!is.null(cfg$preds) && predictor_data_varies_over_time(cfg$preds)) {
    # the days that predictor data should and actually extends to
    expected_max_day <- cfg$steps + cfg$max_delay
    actual_max_day <- max_day_in_predictors_table(cfg$preds)
    if (actual_max_day < expected_max_day) {
      stop(
        "Predictor data should extend to at least", expected_max_day,
        ", the sum of `max_delay` and `steps`, but the final day in the ",
        "predictor data was ", actual_max_day,
        call. = FALSE
      )
    }
  }

  #
  # # TODO need some checks on the relationships between transitions and
  # # predictors data. And that each argument to each transition_function has a
  # # corresponding parameter or predictor.
  # test_predictors(cfg$transitions, cfg$predictors)
  #
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
