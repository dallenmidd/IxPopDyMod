#' `config` constructor
#'
#' @description
#' Quickly create a new `config` object with minimal checks
#'
#' @inheritParams config
#'
#' @return a `config` object
#'
#' @noRd
new_config <- function(
    cycle, initial_population, preds, steps, max_delay
  ) {

  # check that all types are correct
  checkmate::assert_class(cycle, "life_cycle")
  checkmate::assert_integer(
    initial_population, lower = 0, min.len = 1, names = "unique"
  )
  checkmate::assert_class(preds, "predictors", null.ok = TRUE)
  checkmate::assert_int(steps, lower = 0)
  checkmate::assert_int(max_delay, lower = 1)

  structure(
    list(
      cycle = cycle,
      initial_population = initial_population,
      preds = preds,
      steps = steps,
      max_delay = max_delay
    ),
    class = "config"
  )
}

#' Create a `config` object
#'
#' @description
#' Make a `config` object from the input parameters, and ensure that the inputs
#' meet the requirements for the model. The returned object is a complete
#' description of a model run scenario.
#'
#' @param life_cycle A tick \link{\code{life_cycle}}
#'
#' @param predictors Optional input \link{\code{predictors}} data
#'
#' @param steps Numeric vector of length one indicating the duration to run the
#'   model over in days.
#'
#' @param max_delay Numeric vector of length one. Determines the maximum
#'   number of days that a duration-based transition can last, after which ticks
#'   are removed from the model/die. Default of 365 is likely sensible for most
#'   cases.
#'
#' @param initial_population Named numeric vector indicating starting population
#'   for each life stage. Life stages not specified are assumed to be 0.
#'
#' @return A `config` object
#'
#' @details TODO move to `life_cycle`?
#' The delay column affects how a transition row is used in the model. In all
#' cases, a transition row is evaluated with any parameters and predictors,
#' resulting in a transition value, `t`. If there is another row with the same
#' "from", but either "m" or "per_capita_m" for the "to" stage, this row
#' will be evaluated as well, resulting in a mortality transition value, `m`.
#' Only delay transitions support "per_capita_m".
#'
#' In non-delay transitions (where `delay == FALSE`), ticks can either advance
#' to the "to" stage, die, or remain in the "from" stage. In this case, `t`
#' is interpreted as the probability that a tick in the "from" stage will
#' advance to the "to" stage at the next time step. The survival rate, or the
#' probability that a tick will remain in the same "from" life stage, is
#' calculated as `1 - (t + m)`.
#'
#' In delay transitions (where `delay == TRUE`), ticks can either advance to the
#' "to" stage, or die - there is no survival. In this case, `t` is used to
#' determine the number of days until ticks in the "from" stage will emerge as
#' ticks in the "to" stage. `t` will be vectorized over each day from the
#' current time step to `max_delay` days ahead. The duration of the transition
#' (in days) will be the index `i` of the first element in `t` where the
#' cumulative sum of `t[1:i]` is greater than or equal to 1.
#'
#' Delay transitions support two modes of mortality, "m" and "per_capita_m".
#' For transitions to "m", the mortality value `m` is interpreted as a daily
#' probability of mortality for each day in the delay transition. This differs
#' from transitions to "per_capita_m", where `m` is the total probability of
#' mortality over the entire duration of the delay transition.'
#'
#' @export
#'
#' @examples TODO update
#'
#' # We rebuild an example config from its constituent parts. This is successful
#' # as expected, because we're just making a config that's identical to an
#' # example.
#' do.call(config, config_ex_1)
#'
#' # If we modify the config to something unsuitable, the function will
#' # complain. For example, if we modify the egg to larvae transition to use a
#' # different function that requires an additional parameter.
#'
#' \dontrun{
#' # We define a super simple function that takes two parameters.
#' prod_fun <- function(x, y, a, b) a * b
#'
#' my_config <- config_ex_1
#' my_config$transitions[1, 3] <- "prod_fun"
#'
#' # this will throw an error, because a parameter is missing
#' do.call(config, my_config)
#' # config() will report that parameter "b" is missing for the exponential
#' # function.
#'
#' # Adding the parameter should fix the config
#' my_config$parameters[9, ] <- list(
#'   from = "__e", to = "__l", param_name = "b",
#'   param_value = 1
#' )
#'
#' # Now, this should run without issues
#' do.call(config, my_config)
#' }
#'
#' # TODO this only works currently with new_config(), should be config()
#' config(
#'   cycle = life_cycle(
#'     transition("a", "b", function() 0.1, "probability"),
#'     transition("b", "a", function() 10, "probability")
#'   ),
#'   preds = NULL,
#'   initial_population = c(a = 1L, b = 0L),
#'   steps = 10,
#'   max_delay = 365L
#' )
#'
config <- function(
    cycle, initial_population, preds = NULL, steps, max_delay = 365L
  ) {

  # coerce input types

  # TODO check if do.call() introduces a new way to fail... see transition()
  cycle <- do.call(life_cycle, cycle)
  preds <- do.call(predictors, preds)

  initial_population <- ensure_int(initial_population)
  steps <- ensure_int(steps)
  max_delay <- ensure_int(max_delay)

  # return validated config
  validate_config(
    new_config(
      cycle = cycle,
      initial_population = initial_population,
      predictors = predictors,
      steps = steps,
      max_delay = max_delay
    )
  )
}

#' create a config object from a YAML file
#' @importFrom yaml read_yaml
#' @importFrom readr read_csv cols
#' @param file YAML file to read
#' @return A `config` object
#'
#' @examples
#' \dontrun{
#' read_config("cfg.yml")
#' }
#'
#' @export
read_config <- function(file) {
  # parse the input config file as a named list
  cfg <- yaml::read_yaml(file)

  # reshape initial_population from list to named vector
  cfg$initial_population <- unlist(cfg$initial_population)

  # convert from paths to dfs
  cfg$transitions <- read_csv(cfg$transitions, col_types = cols())
  cfg$parameters <- read_csv(cfg$parameters, col_types = cols())

  if (!is.null(cfg$predictors)) {
    cfg$predictors <- read_csv(cfg$predictors, col_types = cols())
  }

  # use this named list as the arguments to constructing a config object
  do.call(config, cfg)
}

#' Save a `config` object as files
#'
#' @description
#' Write a `config` object as a YAML file, and write all dataframe
#' components (transitions, parameters, predictors) as csv files. All
#' paths must be explicitly specified as arguments. This function will not
#' allow overwriting files.
#'
#' @param cfg A `config` object
#' @param config_path Path to the output YAML config file
#' @param transitions_path Path to output transitions csv
#' @param parameters_path Path to output parameters csv
#' @param predictors_path Path to output predictors csv
#'
#' @return None, writes config components to disk
#'
#' @examples
#' \dontrun{
#' write_config(
#'   config_ex_1, "cfg.yml", "trans.csv", "params.csv",
#'   "predictors.csv"
#' )
#' }
#'
#' @export
write_config <- function(cfg, config_path, transitions_path, parameters_path,
                         predictors_path) {
  paths <- c(config_path, transitions_path, parameters_path, predictors_path)
  for (f in paths) {
    if (file.exists(f)) {
      stop(
        paste0('Cannot write "', f, '", file already exists'),
        call. = FALSE
      )
    }
  }

  yaml::write_yaml(
    x = list(
      steps = ensure_int(cfg$steps),
      max_delay = cfg$max_delay,
      initial_population = as.list(ensure_int(cfg$initial_population)),
      transitions = transitions_path,
      parameters = parameters_path,
      predictors = predictors_path
    ),
    file = config_path
  )

  readr::write_csv(cfg$transitions, transitions_path)
  readr::write_csv(cfg$parameters, parameters_path)
  readr::write_csv(cfg$predictors, predictors_path)

  return(NULL)
}


#' Run the model for each config
#'
#' @description
#' Simple convenience wrapper for calling run on each `config` in a list
#'
#' @param configs List of `config` objects
#' @param parallel Logical; if TRUE, run on all cores using `parallel` package.
#'
#' @return A list of data frame model outputs like those returned by `run()`
#'
#' @export
#'
#' @examples
#' # run two example configs and save results
#'
#' \dontrun{
#' outputs <- run_all_configs(list(config_ex_1, config_ex_2))
#' }
run_all_configs <- function(configs, parallel = FALSE) {
  if (parallel) {
    n_cores <- parallel::detectCores()
    parallel::mcmapply(run, configs,
      mc.cores = n_cores,
      SIMPLIFY = FALSE
    )
  } else {
    sapply(configs, run, simplify = FALSE)
  }
}



#' Generate copies of a `config` with a modified parameter
#'
#' @description
#' Create copies of a `config` with a modified parameter. These new
#' `config`s can be used to see how that parameter affects the model
#'
#' @param cfg Base `config` to make modified copies of
#' @param param_row Row number of parameter to vary, if this is specified
#'   arguments from, to, param_name, and host_spp are unneeded
#' @param from The from life stage from of the parameter to change. If this is
#'   given, to and param_name are also needed.
#' @param to The to life stage of the parameter to change.
#' @param param_name The name of the parameter to change
#' @param host_spp The host_spp identifying the parameter to change. Needed only
#'   if there are multiple rows in the parameter table with the same from, to
#'   and param_name, but different host_spp.
#' @param values Numeric vector of values to use for parameter
#' @return A list of `config`s
#'
#' @examples
#'
#' # create new configs with different values for the parameter determining
#' # mortality of eggs (which is found in row 2)
#' cfgs <- vary_param(config_ex_1, param_row = 2, values = c(0, 0.1, 0.2))
#'
#' # inspect parameter row 2 in each of the new configs to verify that we have
#' # the new values
#' lapply(cfgs, function(cfg) cfg$parameters[[2, "param_value"]])
#'
#' @export
vary_param <- function(cfg, param_row = NA, to = NA, from = NA, param_name = NA,
                       host_spp = NA, values) {
  p <- cfg$parameters

  if ((!is.na(param_row) && !all(is.na(c(to, from, param_name, host_spp)))) ||
    (is.na(param_row) && any(is.na(c(to, from, param_name))))) {
    stop(
      "vary_param should be called with either param_row; or with to, from,
      param_name and (optionally) host_spp",
      call. = FALSE
    )
  }

  if (!is.na(to)) {
    if ("host_spp" %in% names(p) &&
      depends_on_hosts(cfg$transitions) &&
      !is.na(host_spp)) {
      param_row <- which(p$to == to &
        p$from == from &
        p$param_name == param_name &
        p$host_spp == host_spp)
    } else {
      param_row <- which(
        p$to == to &
          p$from == from &
          p$param_name == param_name
      )
    }

    if (length(param_row) != 1) {
      stop(
        "to, from, param_name and (optionally) host_spp must identify exactly 1
        parameter row. Found rows: ", paste(param_row, collapse = ", "),
        call. = FALSE
      )
    }
  }

  list_cfg <- list()
  counter <- 1
  for (v in values) {
    list_cfg[[counter]] <- set_param(cfg, param_row, v)
    counter <- counter + 1
  }
  list_cfg
}

set_param <- function(cfg, param_row, value) {
  cfg$parameters[param_row, "param_value"] <- value

  # The only validation check that changing a parameter value might break
  # is the value of an evaluated transition row. Therefore, we run this check
  # rather than all the checks in validate_config()
  tryCatch(
    # This test identifies the errors by the *transitions* row that caused the
    # error, which is not very helpful when we are changing *parameters*. We
    # add to that behavior by also identifying the offending parameter
    test_transition_values(cfg),
    error = function(e) {
      e$message <- paste(
        "Setting parameter in row", param_row, "to value", value,
        "resulted in an invalid transition value.", e$message
      )
      stop(e)
    }
  )

  cfg
}


#' Generate copies of a `config` with all combinations of modified parameters
#'
#' @inheritParams vary_param
#'
#' @param param_rows Numeric vector indicating the rows in the parameters
#'   table where parameter values should be modified. Length must equal length
#'   of values_list
#'
#' @param values_list List of numeric vectors. The values of a vector
#'   `values_list[[i]]` are the parameter values to use for the parameter
#'   identified by `param_rows[[i]]`
#'
#' @return A list of `config`s
#'
#' @examples
#'
#' # create new configs with different values for the parameter determining
#' # mortality of eggs (which is found in row 2) and that determining
#' # mortality of larvae (which is found in row 4)
#' cfgs <- vary_many_params(config_ex_1,
#'   param_rows = c(2, 4),
#'   values_list = list(c(0, 0.1), c(.99, .98))
#' )
#'
#' # inspect parameter rows 2 and 4 in each of the new configs to verify that we
#' # have the new values
#' lapply(cfgs, function(cfg) cfg$parameters[c(2, 4), "param_value"])
#'
#' @export
vary_many_params <- function(cfg, param_rows, values_list) {
  if ((l <- length(param_rows)) != length(values_list)) {
    stop(
      "param_rows and values_list must have equal lengths",
      call. = FALSE
    )
  }

  i <- 1
  cfgs <- list(cfg)

  while (i <= l) {
    cfgs <- lapply(cfgs, function(x) {
      vary_param(
        cfg = x,
        param_row = param_rows[[i]],
        values = values_list[[i]]
      )
    })

    cfgs <- unlist(cfgs, recursive = FALSE)

    i <- i + 1
  }

  cfgs
}


#' Visualize transitions as a life cycle graph
#'
#' @details
#' This function could be used to visually confirm that a custom `config` has
#' all the transitions intended
#'
#' @param transitions Tick transitions tibble
#'
#' @importFrom dplyr pull filter select
#' @importFrom igraph graph_from_data_frame
#' @export
#'
#' @return None, plots a life cycle graph
#'
#' @examples
#' graph_lifecycle(config_ex_1$transitions)
graph_lifecycle <- function(transitions) {
  transitions %>%
    select(.data$from, .data$to) %>%
    filter(.data$to %in% get_life_stages(transitions)) %>%
    graph_from_data_frame() %>%
    plot(edge.arrow.size = 0.5)
}

#' Convert doubles to integers and preserve names
#'
#' @param x A double
#' @return If x is a double whose value is equal to an integer, return the
#' equivalent integer. Otherwise, return x
#' @noRd
ensure_int <- function(x) {
  if (is.double(x) && all((x == as.integer(x)) | (is.na(x)))) {
    return(stats::setNames(as.integer(x), names(x)))
  }
  x
}
