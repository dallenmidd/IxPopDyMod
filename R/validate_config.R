#' Check that a `config` object is valid
#' @return Returns the input config object if it passes all the checks
#' @noRd
validate_config <- function(cfg) {

  assert_predictor_data_extends_over_required_days(cfg)
  assert_initial_population_has_valid_life_stages(cfg)
  assert_initial_population_non_zero(cfg)
  assert_predictors_strings_in_transitions_are_valid(cfg)
  assert_inputs_to_each_transition_function_are_valid(cfg)
  assert_no_tick_density_predictors_have_first_day_only_false(cfg)

  cfg
}

assert_inputs_to_each_transition_function_are_valid <- function(cfg) {
  # For each transition, get the parameters and predictors

  # To actually get predictor values, we need a couple inputs that are variable
  # over time and thus not in the config. We mock them here.
  time <- 1
  pop <- empty_population_matrix(life_stages = life_stages(cfg$cycle), steps = cfg$steps)

  for (transition in cfg$cycle) {

    inputs <- get_transition_inputs_unevaluated(
      time = time,
      transition = transition,
      predictors = cfg$preds,
      max_duration = cfg$max_duration,
      population = pop,
      developing_population = pop
    )

    parameters <- inputs[["parameters"]]
    predictors <- inputs[["predictors"]]

    # Named, non-scalar values
    named_parameters <- Filter(function(x) !is.null(names(x)), parameters)
    named_predictors <- Filter(function(x) !is.null(names(x)), predictors)

    # If there are any parameters or predictors that are named vectors, they
    # must all have the same names.
    if (length(named_parameters) > 0 || length(named_predictors) > 0) {

      parameter_names <- unname(lapply(named_parameters, names))
      predictor_names <- unname(lapply(named_predictors, names))

      all_names <- c(parameter_names, predictor_names)

      # each vector of names should be identical
      if (length(unique(all_names)) != 1L) {
        stop(
          "For each transition, any named parameters and predictors must have ",
          "identical names. Found these parameter names: ", parameter_names,
          ", and these predictor names: ", predictor_names,
          call. = FALSE
        )
      }
    }
  }
}

assert_no_tick_density_predictors_have_first_day_only_false <- function(cfg) {
  # Any `predictor_spec`s that are for tick density must have a value of
  # TRUE in the field `first_day_only`. This is because we only know tick
  # density at the current time step during modeling - there's no way to know
  # future tick density. This validation is implemented here, at the config
  # level, because we need the tick life stages to know which predictors are
  # specifying tick density.

  stages <- life_stages(cfg$cycle)

  problematic_transitions <- vapply(
    cfg$cycle,
    function(each_transition) {
      transition_uses_tick_den_predictor_with_first_day_only_false(
        transition = each_transition, stages = stages
      )
    },
    FUN.VALUE = logical(1L)
  )

  if (any(problematic_transitions) > 0) {
    stop(
      "Predictors using tick density must have the `first_day_only` field set ",
      "to `TRUE`. The `transition`s at these indices violated this expectation: ",
      to_short_string(which(problematic_transitions), item_name = "cases"),
      call. = FALSE
    )
  }
}

assert_predictor_data_extends_over_required_days <- function(cfg) {
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
}

assert_initial_population_has_valid_life_stages <- function(cfg) {
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
}

assert_initial_population_non_zero <- function(cfg) {
  if (!any(cfg$initial_population > 0)) {
    stop(
      "`initial_population` must be greater than 0 for at least one life stage",
      call. = FALSE
    )
  }
}

assert_predictors_strings_in_transitions_are_valid <- function(cfg) {
  # for each transition, each value in the predictors vector must either be
  # a value in the predictors table or a pattern that matches 1 or more
  # life stages via regex

  # so, the names of each life stage must be disjoint with the names of
  # predictors in the predictors table
  stages <- life_stages(cfg$cycle)
  predictors_in_table <- valid_predictors_from_table(cfg$preds)
  checkmate::assert_disjunct(stages, predictors_in_table)

  # and so, if a value in the predictors vector is not a value in the predictors
  # table, then it should pattern match with at least one life stage
  predictors_in_transitions <- unlist(lapply(
    cfg$cycle, function(transition) {
      lapply(transition$predictors, function(x) x$pred)
    }
  ))

  predictors_not_in_table <- setdiff(
    predictors_in_transitions, predictors_in_table
  )

  if (length(predictors_not_in_table) == 0) {
    return()
  }

  # whether each predictor that's not in the table matches >= 1 life stage
  matches <- vapply(
    predictors_not_in_table,
    function(predictor) any(grepl(pattern = predictor, x = stages)),
    FUN.VALUE = logical(1)
  )

  if (!all(matches)) {
    unmatched_predictors <- names(matches[!matches])
    stop(
      "All predictors in transitions must be either a value in the `pred` ",
      "column of the predictors table, or a pattern that matches the name of ",
      "at least one life stage. Found these invalid predictor names: ",
      paste(unmatched_predictors, collapse = ", "),
      call. = FALSE
    )
  }
}
