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
                 intial_population = initial_population,
                 transitions = transitions,
                 parameters = parameters,
                 host_comm = host_comm,
                 weather = weather,
                 max_delay = max_delay),
            class = 'config')
}

#' check that a config object is valid
#' @return Returns the input config object if it passes all the checks
validate_config <- function(config) {

  if (steps < 0) {
    stop(
      "`steps` must be positive",
      .call = FALSE
    )
  }

  # TODO
  # max_delay must be positive, and length(max_delay) == 1
  # initial_population must be a named vector with the same names and
  #   length as get_life_stages(transitions)
  # make sure there is an initial_population greater than 0 for some life stage?
  #   and/or that all the names of the intial_population vector are valid life
  #   stages
  # transitions must form a closed loop (borrow testing functions code)
  # functions (e.g. expo_fun) in transitions table are accessible/exist
  # each function in the transition df has the parameters it needs in the
  #   parameters df
  # pred1 and pred2 values are supported by get_pred()
  # all tibbles must have required columns, with correct types
  # ...

  config
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
  configuration <- yaml::read_yaml(file)

  # reshape initial_population from list to named vector
  configuration$initial_population <- unlist(configuration$initial_population)

  # convert from paths to dfs
  configuration$transitions <- read_csv(configuration$transitions, show_col_types = FALSE)
  configuration$parameters <- read_csv(configuration$parameters, show_col_types = FALSE)
  configuration$host_comm <- read_csv(configuration$host_comm, show_col_types = FALSE)
  configuration$weather <- read_csv(configuration$weather, show_col_types = FALSE)

  # use this named list as the arguments to constructing a config object
  do.call(config, configuration)
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
run_all_configs <- function() {}

#' Generate copies of a config with a modified parameter
#' TODO locate parameter by row in parameters table or filtering
#' with from, to, param_name, and host_spp?
#' @param config base configuration to make modified copies of
#' @param param_row row number of parameter to vary
#' @param values Numeric vector of values to use for parameter
vary_param <- function(config, param_row, values)


#' Generate an array/grid of configs modifying each parameter along its own
#' sequence of values
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

