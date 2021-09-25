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

#' Save a `config` object as files
#'
#' @description
#' Write a `config` object as a YAML file, and write all dataframe
#' components (transitions, parameters, weather, host_comm) as csv files. All
#' paths must be explicitly specified as arguments. This function will not
#' allow overwriting files.
#'
#' @param cfg A `config` object
#' @param config_path Path to the output YAML config file
#' @param transitions_path Path to output transitions csv
#' @param parameters_path Path to output parameters csv
#' @param weather_path Path to output weather csv
#' @param host_comm_path Path to output host_comm csv

#' @param transtions
write_config <- function(cfg, config_path, transitions_path, parameters_path,
                         weather_path, host_comm_path) {

  for (f in c(config_path, transitions_path, parameters_path, weather_path,
              host_comm_path)) {
    if (file.exists(f)) {
      stop(
        paste0('Cannot write "', f, '", file already exists'),
        call. = FALSE
      )
    }
  }

  yaml::write_yaml(
    x = list(
      steps = cfg$steps,
      max_delay = cfg$max_delay,
      initial_population = as.list(cfg$initial_population),
      transitions = transitions_path,
      parameters = parameters_path,
      weather = weather_path,
      host_comm = host_comm_path
      ),
    file = config_path
  )

  readr::write_csv(cfg$transitions, transitions_path)
  readr::write_csv(cfg$parameters, parameters_path)
  readr::write_csv(cfg$weather, weather_path)
  readr::write_csv(cfg$host_comm, host_comm_path)
}


#' Run the model for each config
#' @param configs List of `config` objects
#' @param parallel Logical; if TRUE, run on all cores using `parallel` package.
#' @return A stacked data frame of the model outputs for each `config`. Return
#'   value is like `run()`, with an additional column "config" identifying
#'   the `config` object that results were generated from.
run_all_configs <- function(configs, parallel = FALSE) {

  if (parallel) {
    n_cores <- parallel::detectCores()
    l <- parallel::mcmapply(run, configs, mc.cores = n_cores,
                            SIMPLIFY = FALSE)
  } else {
    l <- sapply(configs, run, simplify = FALSE)
  }

  dplyr::bind_rows(l, .id = "config")
}

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

