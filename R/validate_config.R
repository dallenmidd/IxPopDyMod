has_required_cols <- function(cfg, df_name, required_colnames) {
  if (!all(rlang::has_name(cfg[[df_name]], required_colnames))) {
    missing_cols <- required_colnames[!rlang::has_name(cfg[[df_name]],
                                                required_colnames)]
    stop("`", df_name, "` is missing required columns: ",
         paste(missing_cols, collapse = ', '),
         call. = FALSE
    )
  }
}

has_required_types <- function(cfg, df_name, required_coltypes) {

  actual_coltypes <- sapply(cfg[[df_name]], class)

  for (col in names(required_coltypes)) {

    if (!(required_coltypes[[col]] == actual_coltypes[[col]] |
          (required_coltypes[[col]] == 'numeric' &
           actual_coltypes[[col]] %in% c('double', 'integer')) |
          # if missing values were interpreted as type logical
          (actual_coltypes[[col]] == 'logical' &
           all(is.na(cfg[[df_name]][, col])))))

      stop(
        "Expected type \"", required_coltypes[col], "\" for column `", col,
        "` in `", df_name,
        "` but found type \"", actual_coltypes[col], "\"",
        call. = FALSE
      )
  }
}

# return whether parameters are sorted by host_spp column
parameters_are_sorted <- function(parameters) {
  actual <- parameters$host_spp[!is.na(parameters$host_spp)]
  all(actual == sort(actual)) | all(actual == sort(actual, decreasing = TRUE))
}

set_all_names <- function(name) {
  function(v) {
    if (!is.null(v)) {
      names(v) <- rep(name, length(v))
      v
    }
  }
}

set_all_names_x <- set_all_names('x')
set_all_names_i <- set_all_names('i')


#' @param error_header String
#' @param problem_list List of strings describing each problem
print_errors <- function(error_header, problem_list, hints = NULL,
                         max_problems = 3L) {

  n_problems <- length(problem_list)

  if (n_problems > 0) {

    error_header <- paste(
      stringr::str_squish(error_header),
      '\n')

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


parameter_pattern_matching_is_valid <- function(parameters, life_stages) {

  is_valid <- function(s) {
    any(stringr::str_detect(life_stages, s) |
          s %in% c('m', 'per_capita_m'))}

  # get name and index of invalid `from` or `to` fields
  invalid_from <- which(!sapply(parameters$from, is_valid))
  invalid_to <- which(!sapply(parameters$to, is_valid))

  invalid <- tibble::tibble(
    string = c(names(invalid_from),
               names(invalid_to)),
    row = c(invalid_from,
            invalid_to),
    col = c(rep('from', length(invalid_from)),
            rep('to', length(invalid_to))))

  row_to_string <- function(row) {
    paste0('"', row[['string']], '" in col "', row[['col']], '", row ',
           row[['row']])
  }

  problem_list <- error_df_to_list(invalid, row_to_string)
  print_errors(
    error_header = "Strings in `parameters` `from` and `to` columns must
      either be regex patterns that match life stages, or strings \"m\" or
      \"per_capita_m\" indicating mortality. Found exceptions:",
    problem_list = problem_list
  )
}


transition_fun_exists <- function(row_index, transitions) {
  tryCatch(
    {
      function_name <- transitions[[row_index, 'transition_fun']]
      get(function_name)
      NULL # indicate that there was no error
    },

    error = function(e) {
      paste0("Can't find function `", function_name, "` from row ", row_index,
             " of `transitions`")
    }
  )
}

# return a string with the following information for a problematic transition:
# function name and row in transition table (to locate the error)
# list of missing parameters
# list of extra parameters
missing_and_extra_params <- function(row_index, transitions_with_parameters) {

  transition_row <- transitions_with_parameters[row_index,]
  expected <- as.list(args(get(transition_row[['transition_fun']])))
  expected <- names(expected[!(names(expected) %in% c('x', 'y', ''))])
  # need empty string to get rid of unnamed NULL value at end of list
  actual <- names(transition_row$params_list[[1]])
  missing <- setdiff(expected, actual)
  extra <- setdiff(actual, expected)

  param_to_string <- function(param, problem) {
    paste0(problem, ' param "', param,
           '" in function "', transition_row[['transition_fun']],
           '" in row ', row_index, ' of `transitions`')
  }

  params_to_string_list <- function(params, problem) {
    if (length(params) > 0) {
      lapply(params,
             function(p) param_to_string(p, problem))
    }
  }

  c(params_to_string_list(missing, 'Missing'),
    params_to_string_list(extra, 'Extra'))
}

test_predictors <- function(transitions) {

  # return whether a predictor value is supported by get_pred()
  predictor_is_valid <- function(pred) {

    valid_strings <- c('temp', 'vpd', 'host_den')

    life_stages <- get_life_stages(transitions)

    is.na(pred) ||
      pred %in% valid_strings ||
      any(stringr::str_detect(life_stages, pred))
  }

  error_tibble <- transitions[c('pred1', 'pred2')] %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    tidyr::pivot_longer(cols = c('pred1', 'pred2'))

  error_tibble$valid <- unlist(lapply(error_tibble$value, predictor_is_valid))

  error_tibble <- error_tibble %>%
    filter(!(.data$valid))

  row_to_string <- function(row) {
    paste0("\"", row['value'], "\" in row ", row['row'],
           ", column `", row['name'], "`")
  }

  errors <- error_df_to_list(error_tibble, row_to_string)
  print_errors(
    'Unsupported predictor values found in `transitions`:', errors)
}

# determine whether transitions depend on given predictors
depends_on <- function(predictors) {
  function(transitions) {
    any(predictors %in% unlist(transitions[, c('pred1', 'pred2')]))
  }
}

depends_on_weather <- depends_on(c('temp', 'vpd'))
depends_on_hosts <- depends_on('host_den')

to_short_string <- function(v, max = 3) {
  l <- length(v)
  string <- paste(v[1:min(max, l)],
                  collapse = ', ')
  paste0(
    string,
    ifelse(l > max,
           paste('... and', l - max, 'more values'),
           ''))
}

# ensure that there is weather or host_comm data for the entire j_day range
# that we are running the model, plus the max_delay
# We add max_delay because if input data is missing for time in between
# steps and steps+max_delay, we could incorrectly get delay transitions that
# do not cumsum to 1
test_missing_days <- function(tbl, tbl_name, steps, max_delay) {

  missing_days <- setdiff(1:(steps + max_delay),
                          intersect(1:(steps + max_delay), tbl$j_day))

  if (length(missing_days) > 0) {
    stop(
      stringr::str_squish(paste0(
        "`", tbl_name, "` must have a row for each `j_day` from 1 to `steps`
          + `max_delay`. Missing `j_day` values: ")), ' ',
      to_short_string(missing_days),
      call. = FALSE
    )
  }
}

# ensure that there is host density data for the same days for all host spp
test_host_spp_days <- function(host_comm) {

  hosts <- unique(host_comm$host_spp)

  days_for_each_host <- unname(sapply(hosts, function(h) {
    host_comm[(host_comm$host_spp == h), 'j_day']
  }))

  # get days in same order since we do not test set equality
  days_for_each_host <- lapply(days_for_each_host, sort)

  is_same_day_range <- isTRUE(do.call(all.equal, days_for_each_host))

  if(!(is_same_day_range)) {
    stop(
      "`host_comm` must have the same `j_day` range for each `host_spp`",
      call. = FALSE
    )
  }
}

test_transition_values <- function(cfg) {

  life_stages <- get_life_stages(cfg$transitions)

  # initialize a population matrix with 10 of each tick life_stage on day 1
  # and an empty N_developing matrix
  N <- matrix(nrow = length(life_stages),
              ncol = cfg$steps + cfg$max_delay,
              data = 0)
  rownames(N) <- life_stages
  N_developing <- N
  N[,1] <- 10

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
          N = N,
          N_developing = N_developing,
          max_delay = cfg$max_delay,
          life_stages = life_stages,
          host_comm = cfg$host_comm,
          weather = cfg$weather)})

  transitions_are_valid <-
    sapply(transition_values,
           is_valid)

  if (!all(transitions_are_valid)) {

    funs$valid = transitions_are_valid
    funs$value = transition_values

    invalid_funs <- funs %>%
      dplyr::mutate(row_number = dplyr::row_number()) %>%
      dplyr::filter(!.data$valid)

    n_invalid <- nrow(invalid_funs)

    row_to_string <- function(row) {
      paste0('`', row[['transition_fun']], '` in row ', row[['row_number']],
             ' evaluates to ', to_short_string(row[['value']]))
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

  if (length(cfg$steps) != 1) {
    stop(
      "`steps` must have length 1",
      call. = FALSE
    )
  }

  if (cfg$steps < 1) {
    stop(
      "`steps` must be at least 1",
      call. = FALSE
    )
  }

  if (length(cfg$max_delay) != 1) {
    stop(
      "`max_delay` must have length 1",
      call. = FALSE
    )
  }

  if (cfg$max_delay < 365L) {
    stop(stringr::str_squish("`max_delay` should be at least 365 or developing
                             ticks may not emerge from delay transitions"),
      call. = FALSE
    )
  }

  transitions_coltypes <- c(
    from = 'character', to = 'character', transition_fun = 'character',
    delay = 'logical', pred1 = 'character', pred2 = 'character'
  )
  has_required_cols(cfg, 'transitions', names(transitions_coltypes))
  has_required_types(cfg, 'transitions', transitions_coltypes)

  test_predictors(cfg$transitions)

  if (depends_on_weather(cfg$transitions)) {
    weather_coltypes <- c(tmean = 'numeric', j_day = 'numeric')
    has_required_cols(cfg, 'weather', names(weather_coltypes))
    test_missing_days(cfg$weather, 'weather', cfg$steps, cfg$max_delay)
    has_required_types(cfg, 'weather', weather_coltypes)
  }


  parameters_coltypes <- c(
    from = 'character', to = 'character', param_name = 'character',
    param_value = 'numeric')

  if (depends_on_hosts(cfg$transitions)) {

    # add host spp column requirement
    parameters_coltypes['host_spp'] <- 'character'

    host_comm_coltypes <- c(
      j_day = 'numeric', host_spp = 'character', host_den = 'numeric')
    has_required_cols(cfg, 'host_comm', names(host_comm_coltypes))
    has_required_types(cfg, 'host_comm', host_comm_coltypes)
    test_missing_days(cfg$host_comm, 'host_comm', cfg$steps, cfg$max_delay)
    test_host_spp_days(cfg$host_comm)
  }

  has_required_cols(cfg, 'parameters', names(parameters_coltypes))
  has_required_types(cfg, 'parameters', parameters_coltypes)

  if (depends_on_hosts(cfg$transitions)) {
    if (!parameters_are_sorted(cfg$parameters)) {
      stop(
        "`parameters` must be sorted by the column `host_spp`",
        call. = FALSE
      )
    }
  }

  if (is.null(names(cfg$initial_population)) |
      any(names(cfg$initial_population) == '')) {
    stop(
      "all values of `initial_population` must be named",
      call. = FALSE
    )
  }

  life_stages <- get_life_stages(cfg$transitions)
  life_stages_found <- names(cfg$initial_population)

  if (!all(life_stages_found %in% life_stages)) {
    stop(
      "`initial_population` had names that are not valid life stages: ",
      paste(life_stages_found[!(life_stages_found %in% life_stages)],
            collapse = ", "),
      call. = FALSE
    )
  }

  if (!any(cfg$initial_population > 0)) {
    stop(
      "`initial_population` must be greater than 0 for at least one life stage",
      call. = FALSE
    )
  }

  if (any(cfg$initial_population < 0)) {
    stop(
      "`initial_population` may not be negative for any life stage",
      call. = FALSE
    )
  }

  parameter_pattern_matching_is_valid(cfg$parameters, life_stages)

  transition_fun_errors <-
    seq_len(nrow(cfg$transitions)) %>%
    lapply(function(x) transition_fun_exists(x, cfg$transitions)) %>%
    unlist()

  print_errors(
    "Strings in the `transition_fun` column of `transitions` must be the names
     of functions:",
    transition_fun_errors,
    "If you're using custom functions, make sure you've loaded your
      functions into the environment, e.g. by sourcing a file with
      function definitions."
  )

  transitions_with_parameters <- add_params_list(cfg$transitions,
                                                 cfg$parameters)

  errors <- unlist(lapply(
    seq_len(nrow(transitions_with_parameters)),
    function(i) missing_and_extra_params(i, transitions_with_parameters)))

  print_errors( 'Extra and/or missing parameters found: \n', errors)

  test_transition_values(cfg)

  # TODO Myles
  # transitions must form a closed loop (borrow testing functions code)

  cfg
}
