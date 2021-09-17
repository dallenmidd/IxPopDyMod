#' `config` constructor
#' Quickly create a new `config` object with minimal checks. Backend use only.
new_config <- function(initial_population, transitions, parameters,
                       host_comm, weather, steps, max_delay) {


  # check that all types are correct
  stopifnot(is.integer(initial_population))
  stopifnot(is.data.frame(transitions))
  stopifnot(is.data.frame(parameters))
  stopifnot(is.data.frame(host_comm))
  stopifnot(is.data.frame(weather))
  stopifnot(is.integer(steps))
  stopifnot(is.integer(max_delay))

  structure(list(steps = steps,
                 initial_population = initial_population,
                 transitions = transitions,
                 parameters = parameters,
                 host_comm = host_comm,
                 weather = weather,
                 max_delay = max_delay),
            class = 'config')
}

#' check that a config object is valid
#' @importFrom rlang has_name
#' @return Returns the input config object if it passes all the checks
validate_config <- function(cfg) {

  if (length(cfg$steps) != 1) {
    stop(
      "`steps` must have length 1",
      call. = FALSE
    )
  }

  if (cfg$steps < 0) {
    stop(
      "`steps` must be positive",
      .call = FALSE
    )
  }

  if (length(cfg$max_delay) != 1) {
    stop(
      "`max_delay` must have length 1",
      call. = FALSE
    )
  }

  # TODO this may be too strict...
  if (cfg$max_delay < 365L) {
    stop(
      "`max_delay` should be at least 365 or developing ticks may not emerge
       from delay transitions",
      call. = FALSE
    )
  }

  weather_colnames <- c('tmean', 'j_day')
  host_comm_colnames <- c('j_day', 'host_spp', 'host_den')
  parameters_colnames <- c('from', 'to', 'param_name', 'host_spp',
                           'param_value')
  transitions_colnames <- c('from', 'to', 'transition_fun', 'delay', 'pred1',
                            'pred2')

  has_required_cols <- function(df_name, required_colnames) {
    if (!all(has_name(cfg[[df_name]], required_colnames))) {
      missing_cols <- required_colnames[!has_name(cfg[[df_name]],
                                                  required_colnames)]
      stop("`", df_name, "` is missing required columns: ",
           paste(missing_cols, collapse = ', '),
           call. = FALSE
           )
    }
  }

  has_required_cols('weather', weather_colnames)
  has_required_cols('host_comm', host_comm_colnames)
  has_required_cols('parameters', parameters_colnames)
  has_required_cols('transitions', transitions_colnames)

  weather_coltypes <- c(tmean = 'numeric', j_day = 'numeric')
  host_comm_coltypes <- c(
    j_day = 'numeric', host_spp = 'character', host_den = 'numeric')
  parameters_coltypes <- c(
    from = 'character', to = 'character', param_name = 'character',
    host_spp = 'character', param_value = 'numeric')

  # TODO delay should probably be logical TRUE or FALSE rather than 0 or 1
  transitions_coltypes <- c(
    from = 'character', to = 'character', transition_fun = 'character',
    delay = 'numeric', pred1 = 'character', pred2 = 'character'
  )

  has_required_types <- function(df_name, required_coltypes) {

    actual_coltypes <- sapply(cfg[[df_name]], class)

    for (col in names(required_coltypes)) {

      if (!(required_coltypes[[col]] == actual_coltypes[[col]] |
            (required_coltypes[[col]] == 'numeric' &
             actual_coltypes[[col]] %in% c('double', 'integer'))))

        # TODO technically model could work without weather or host community
        # inputs, it's just a very simple model that way. This would be okay if
        # the pred1 and pred2 columns are not host_den or temp or vpd...
        # Should we be less strict for that case? E.g. this might throw an error
        # if a column like host_spp in the parameters table is empty and defaults
        # to logical type... even though that should be okay

      stop(
        "Expected type ", required_coltypes[col], " for column ", col,
        " in ", df_name,
        " but found type ", actual_coltypes[col],
        call. = FALSE
      )
    }
  }

  has_required_types('weather', weather_coltypes)
  has_required_types('host_comm', host_comm_coltypes)
  has_required_types('parameters', parameters_coltypes)
  has_required_types('transitions', transitions_coltypes)


  # return whether parameters are sorted by host_spp column
  parameters_are_sorted <- function(parameters) {
    actual <- parameters$host_spp[!is.na(parameters$host_spp)]
    all(actual == sort(actual) | actual == sort(actual, decreasing = TRUE))
  }

  if (!parameters_are_sorted(cfg$parameters)) {
    stop(
      "`parameters` must be sorted by the column `host_spp`",
      call. = FALSE
    )
  }

  if (is.null(names(cfg$initial_population))) {
    stop(
      "`initial_population` must have names",
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

  set_all_names_x <- function(v) {
    if (!is.null(v)) {
      names(v) <- rep('x', length(v))
      v
    }
  }

  parameter_pattern_matching_is_valid <- function(parameters) {

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

    n_invalid <- nrow(invalid)

    row_to_string <- function(row) {
      paste0('"', row[['string']], '" in col "', row[['col']], '", row ',
             row[['row']])
    }

    if (n_invalid > 0) {

      problem_list <- set_all_names_x(apply(invalid[1:min(3, n_invalid), ],
                                            1,
                                            row_to_string))

      stop(
        "Strings in `parameters` `from` and `to` columns must either be regex
        patterns that match life stages, or strings \"m\" or \"per_capita_m\"
        indicating mortality. Found exceptions: \n",
        rlang::format_error_bullets(problem_list),
        ifelse(n_invalid > 3,
               paste('\n... and', n_invalid - 3, 'more problems'),
               ''),
        call. = FALSE
      )
    }
  }

  parameter_pattern_matching_is_valid(cfg$parameters)


  transition_fun_exists <- function(row_index, transitions = cfg$transitions) {
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

  transition_fun_errors <-
    seq_len(nrow(cfg$transitions)) %>%
    lapply(transition_fun_exists) %>%
    unlist() %>%
    set_all_names_x() %>%
    rlang::format_error_bullets()

  n_errors <- length(transition_fun_errors)

  if ((n_errors) > 0) {
    stop(
      "Strings in the `transition_fun` column of `transitions` must be the names
       of functions: \n",
      transition_fun_errors[1:min(3, n_errors)],
      '\n',
      rlang::format_error_bullets(c(
        i = "If you're using custom functions, make sure you've loaded your
        functions into the environment, e.g. by sourcing a file with
        function definitions.")),
      ifelse(n_errors > 3,
             paste('\n... and', n_errors - 3, 'more problems'),
             ''),
      call. = FALSE
    )
  }



  transitions_with_parameters <- add_params_list(cfg$transitions,
                                                 cfg$parameters)

  # return a string with the following information for a problematic transition:
  # function name and row in transition table (to locate the error)
  # list of missing parameters
  # list of extra parameters
  missing_and_extra_params <- function(row_index) {

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

  errors <- unlist(lapply(
    seq_len(nrow(transitions_with_parameters)),
    missing_and_extra_params))

  n_errors <- length(errors)

  if (n_errors > 0) {

    error_bullets <- rlang::format_error_bullets(
      set_all_names_x(errors[1:min(3, n_errors)]))

    stop('Extra and/or missing parameters found: \n',
         error_bullets,
         ifelse(n_errors > 3,
                paste('\n... and', n_errors - 3, 'more problems'),
                ''),
         call. = FALSE
    )
  }

  # return whether a predictor value is supported by get_pred()
  predictor_is_valid <- function(pred) {

    valid_strings <- c('temp', 'vpd', 'host_den')

    life_stages <- get_life_stages(cfg$transitions)

    is.na(pred) ||
      pred %in% valid_strings ||
      any(stringr::str_detect(life_stages, pred))
  }

  test_predictors <- function(transitions) {

    error_tibble <- transitions[c('pred1', 'pred2')] %>%
      dplyr::mutate(row = dplyr::row_number()) %>%
      tidyr::pivot_longer(cols = c('pred1', 'pred2'))

    error_tibble$valid <- unlist(lapply(error_tibble$value, predictor_is_valid))

    error_tibble <- error_tibble %>%
      filter(!valid)

    row_to_string <- function(row) {
      paste0("\"", row['value'], "\" in row ", row['row'],
             ", column `", row['name'], "`")
    }

    n_errors <- nrow(error_tibble)

    if (n_errors > 0) {

      error_bullets <- rlang::format_error_bullets(
        set_all_names_x(apply(error_tibble[1:min(3, n_errors),],
                               1,
                               row_to_string))
      )

      stop('Unsupported predictor values found in `transitions`: \n',
           error_bullets,
           ifelse(n_errors > 3,
                  paste('\n... and', n_errors - 3, 'more problems'),
                  ''),
           call. = FALSE
      )
    }

  }


  # TODO change run() behavior so it takes an initial_population named vector
  # with only some life stages. Currently we pass a vector of length
  # life_stages, and I'm not sure whether it uses the order or names of the
  # vector initial_population to set the starting population in the population
  # matrix `N`

  # TODO Myles
  # transitions must form a closed loop (borrow testing functions code)
  #
  # weather and host_comm inputs have values for each j_day
  #   in seq_len(cfg$steps)
  #   host_spp must have a row for each host_spp for each j_day
  #   the inputs do not need to be sorted by j_day
  #
  # use something like test_transitions() to test that transition functions
  #   return reasonable answers.
  #   is.numeric() ensures results are not NULL or NA
  #   should also test that values are >= 0, because a negative probability doesn't make sense
  #   should use all() for delay outputs that are vectors with length > 1
  # ...

  cfg
}

#' Helper for users to create a `config` object
#' This function should make it easy for users to create a new `config`, e.g. by
#' allowing doubles like 365 instead of the integer 365L for steps, max_delay
#' and initial_population.
#' @param initial_population Named???? numeric vector indicating the starting
#'   population for each life stage. Length should be equal to the number of
#'   life stages.
#' @param transitions Tick transitions tibble
#' @param parameters Tick parameters tibble
#' @param host_comm Host community tibble.
#' @param weather Weather tibble.
#' @param steps Numeric vector of length one indicating the duration to run the
#'   model over in days.
#' @param max_delay Numeric vector of length one. Determines the maximum
#' number of days that a delayed transition can last.
#' @return A `config` object
#' @export
config <- function(initial_population, transitions, parameters,
                   host_comm, weather, steps = 365L, max_delay = 365L) {

  # convert doubles to integers
  ensure_int <- function(x) {
    if (is.double(x) && all(x == as.integer(x))) {
      stats::setNames(as.integer(x), names(x))
    }
    x
  }

  initial_population <- ensure_int(initial_population)
  steps <- ensure_int(steps)
  max_delay <- ensure_int(max_delay)

  # sort parameters by host_spp column
  parameters <- dplyr::arrange(parameters, .data$host_spp)

  # return validated config
  validate_config(new_config(initial_population, transitions, parameters,
                             host_comm, weather, steps, max_delay))
}

#' create a config object from a yaml file
#' @importFrom yaml read_yaml
#' @importFrom readr read_csv
#' @return A `config` object
#' @export
read_config <- function(file) {

  # parse the input config file as a named list
  cfg <- yaml::read_yaml(file)

  # reshape initial_population from list to named vector
  cfg$initial_population <- unlist(cfg$initial_population)

  # convert from paths to dfs
  cfg$transitions <- read_csv(cfg$transitions, show_col_types = FALSE)
  cfg$parameters <- read_csv(cfg$parameters, show_col_types = FALSE)
  cfg$host_comm <- read_csv(cfg$host_comm, show_col_types = FALSE)
  cfg$weather <- read_csv(cfg$weather, show_col_types = FALSE)

  # use this named list as the arguments to constructing a config object
  do.call(config, cfg)
}

#' convert a config object back to a YAML file
#' TODO Behavior for this would probably be weird and low priority.
#' How would we handle the dfs that are part of the config object? Have
#' param for this function for path to each csv? Write those csvs from the dfs
#' in the config object, or assume they are already there?
#' @param file Path to the output YAML config file
write_config <- function(file) {}


##########################

#' Run each config and return a df for comparing the outputs
#' @return df like the output from run, but with a column each for each config
#' TODO Dave - not sure but you may also want to change run() in core_functions
#' so it just takes a config object
run_all_configs <- function() {}

#' Generate copies of a config with a modified parameter
#' TODO locate parameter by row in parameters table or filtering
#' with from, to, param_name, and host_spp?
#' TODO Dave
#' @param config base configuration to make modified copies of
#' @param param_row row number of parameter to vary
#' @param values Numeric vector of values to use for parameter
#' @return List of configs?????
vary_param <- function(config, param_row, values)


#' Generate an array/grid of configs modifying each parameter along its own
#' sequence of values
#' TODO Dave - no idea about the feasability of this one!
vary_many_params <- function() {}

# We agreed that any changes we'd want to do to tick_transitions would be manual
# I think it is similar for host_comm and weather. For weather, we might want to
# compare weather between a few different IUCN climate scenarios - which would
# require manually downloading that data and formatting weather csvs for each
# scenario - I don't think there's a standardized way that we'd want to vary
# the weather input. Same goes for host_den. For example we might want to compare
# a few different rodent communities, but it would make more sense to manually
# create these different host_comm dfs

# example use:
if (FALSE) {
  ex_config <- read_config('inputs/config.yml')
  ex_config
}

