#' `config` constructor
#' @description
#' Quickly create a new `config` object with minimal checks. See `config()` for
#' explanation of parameters.
#' @noRd
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

#' Create a `config` object
#' @param initial_population Named numeric vector indicating starting population
#'   for each life stage. Life stages not specified are assumed to be 0.
#' @param transitions Tick transitions data frame. See readme for details.
#' @param parameters Tick parameters tibble. See readme for details.
#' @param host_comm Host community tibble. See readme for details.
#' @param weather Weather tibble. See readme for details.
#' @param steps Numeric vector of length one indicating the duration to run the
#'   model over in days.
#' @param max_delay Numeric vector of length one. Determines the maximum
#'   number of days that a delayed transition can last.
#' @return A `config` object
#' @export
config <- function(initial_population, transitions, parameters,
                   host_comm, weather, steps, max_delay = 365L) {

  # convert doubles to integers
  ensure_int <- function(x) {
    if (is.double(x) && all(x == as.integer(x))) {
      stats::setNames(as.integer(x), names(x))
    }
    x
  }

  if ('host_spp' %in% names(parameters)) {
    parameters <- dplyr::arrange(parameters, .data$host_spp)
  }

  initial_population <- ensure_int(initial_population)
  steps <- ensure_int(steps)
  max_delay <- ensure_int(max_delay)

  # return validated config
  validate_config(new_config(initial_population, transitions, parameters,
                             host_comm, weather, steps, max_delay))
}

#' create a config object from a yaml file
#' @importFrom yaml read_yaml
#' @importFrom readr read_csv
#' @param file YAML file to read
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
#'
#' @export
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
#'
#' @export
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



#' Generate copies of a `config` with a modified parameter
#'
#' @description
#' Create copies of a `config` with a modified parameter. These new
#' `configs`s can be used to see how that parameter affects the model
#'
#' @param cfg A `config` object
#' @param config Base configuration to make modified copies of
#' @param param_row Row number of parameter to vary,
#'   if this is specified arguments from, to, and param_name are unneeded
#' @param from The from life stage from of the parameter to change. If this is given
#'   to and param_name are also needed.
#' @param to The to life stage of the parameter to change.
#' @param param_name The name of the parameter to change
#' @param values Numeric vector of values to use for parameter
#' @return A list of `config`s
#'
#' @export
vary_param <- function(cfg, param_row= NA, to = NA, from = NA , param_name =NA , values) {

  if (!is.na(to))
  {
    param_row <- which(cfg[['parameters']]$to == to &
                       cfg[['parameters']]$from == from &
                       cfg[['parameters']]$param_name == param_name)
  }

  list_cfg <- list()
  counter <- 1
  for (v in values){
    new_parameters <- cfg[['parameters']]
    new_parameters[param_row, 'param_value'] <- v

    new_cfg <- config(cfg[['initial_population']],
                         cfg[['transitions']],
                         new_parameters,
                         cfg[['host_comm']],
                         cfg[['weather']],
                         cfg[['steps']],
                         cfg[['max_delay']] )

    list_cfg[[counter]] <-  new_cfg
    counter <- counter + 1
  }
  list_cfg
}


#' Generate an array/grid of configs modifying each parameter along its own
#' sequence of values
#' TODO Dave - no idea about the feasability of this one!
#' @export
vary_many_params <- function() {}
