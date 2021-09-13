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
validate_config <- function(config) { # TODO should probably change argument name bc conflicts with config()

  if (length(config$steps) != 1) {
    stop(
      "`steps` must have length 1"
    )
  }

  if (config$steps < 0) {
    stop(
      "`steps` must be positive",
      .call = FALSE
    )
  }

  if (length(config$max_delay) != 1) {
    stop(
      "`max_delay` must have length 1"
    )
  }

  # TODO this may be too strict...
  if (config$max_delay < 365L) {
    stop(
      "`max_delay` should be at least 365 or developing ticks may not emerge
       from delay transitions"
    )
  }

  weather_colnames <- c('tmean', 'j_day')
  host_comm_colnames <- c('j_day', 'host_spp', 'host_den')
  parameters_colnames <- c('from', 'to', 'param_name', 'host_spp',
                           'param_value')
  transitions_colnames <- c('from', 'to', 'transition_fun', 'delay', 'pred1',
                            'pred2')

  has_required_cols <- function(df_name, required_colnames) {
    if (!all(has_name(config[[df_name]], required_colnames))) {
      missing_cols <- required_colnames[!has_name(config[[df_name]],
                                                  required_colnames)]
      stop("`", df_name, "` is missing required columns: ",
           paste(missing_cols, collapse = ', '))
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
  transitions_coltypes <- c(
    from = 'character', to = 'character', transition_fun = 'character',
    delay = 'numeric', pred1 = 'character', pred2 = 'character'
  )

  has_required_types <- function(df_name, required_coltypes) {

    actual_coltypes <- sapply(config[[df_name]], class)

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
        " but found type ", actual_coltypes[col]
      )
    }
  }

  has_required_types('weather', weather_coltypes)
  has_required_types('host_comm', host_comm_coltypes)
  has_required_types('parameters', parameters_coltypes)
  has_required_types('transitions', transitions_coltypes)

  if (is.null(names(config$initial_population))) {
    stop(
      "`initial_population` must have names"
    )
  }

  life_stages <- get_life_stages(config$transitions)
  life_stages_found <- names(config$initial_population)

  if (!all(life_stages_found %in% life_stages)) {
    stop(
      "`initial_population` had names that are not valid life stages: ",
      paste(life_stages_found[!(life_stages_found %in% life_stages)],
            collapse = ", ")
    )
  }

  if (!any(config$initial_population > 0)) {
    stop(
      "`initial_population` must be greater than 0 for at least one life stage"
    )
  }

  if (any(config$initial_population < 0)) {
    stop(
      "`initial_population` may not be negative for any life stage"
    )
  }

  # TODO change run() behavior so it takes an initial_population named vector
  # with only some life stages. Currently we pass a vector of length
  # life_stages, and I'm not sure whether it uses the order or names of the
  # vector initial_population to set the starting population in the population
  # matrix `N`

  # TODO Myles
  # transitions must form a closed loop (borrow testing functions code)
  # functions (e.g. expo_fun) in transitions table are accessible/exist
  # each function in the transition df has the parameters it needs in the
  #   parameters df
  # pred1 and pred2 values are supported by get_pred()
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

