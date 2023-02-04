#' Get tick age based on life stage
#'
#' @param life_stage Three-character string representing tick life stage
#' @importFrom stringr str_length
#' @return Character representing tick age, where "l" indicates larvae, "n"
#'   indicates nymph, and "a" indicates adult
#' @noRd
age <- function(life_stage) {
  substr(life_stage, str_length(life_stage), str_length(life_stage))
}

#' Get tick current process
#'
#' @param life_stage Three-character string representing tick life stage
#' @return Character representing current process tick is undergoing, where "a"
#'   indicates attached, "e" indicates engorged, "h" indicates hardening, "q"
#'   indicates questing, "r" indicates reproductive", and "f" indicates feeding
#' @noRd
process <- function(life_stage) {
  ifelse(substr(life_stage, 0, 1) != "", substr(life_stage, 0, 1), "")
}

#' Get tick infection status
#'
#' @param life_stage Three-character string representing tick life stage
#' @return Boolean indicating whether a life_stage is infected
#' @importFrom stringr str_detect
#' @noRd
infected <- function(life_stage) {
  str_detect(life_stage, "i")
}

#' Get all life stages
#' @param transitions Tick transitions data frame.
#' @return Character vector of life stage names
#' @noRd
get_life_stages <- function(transitions) {
  unique(pull(transitions, .data$from))
}

#' Get a predictor from input data
#'
#' @param time Numeric vector of days to get data. Ignored if input is constant
#'   over time (as indicated by NA value in 'j_day' column)
#' @param table input predictors table
#' @param pred string specifying the name of the predictor, e.g. "host_den"
#'
#' @returns a numeric vector of predictor values, TODO named by `pred_subcategory`
get_pred_from_table <- function(time, pred, table) {
  # NA entry in the j_day column indicates that the predictor does not vary
  # over time
  rows <- (is.na(table$j_day) | (table$j_day %in% time)) &
          (table$pred == pred)

  subset <- table[rows, "value"]

  # convert subsetted tibble to a vector # TODO should be named
  unname(unlist(subset))
}

#' Get tick density for specified time and life stages
#'
#' @param time Numeric vector of length one indicating day to get tick density
#' @param pred Character vector indicating life stages to get tick density of
#' @param population Matrix of number of ticks not currently undergoing
#'   development per life stage per day
#' @param developing_population Matrix of number of currently developing ticks
#'   per life stage per day
#' @returns Numeric vector of length one indicating current number of ticks in
#'   given life stages
#' @noRd
get_tick_den <- function(time, pred, population, developing_population) {
  stopifnot(length(time) == 1)
  life_stages <- rownames(population)
  life_stages_to_sum <- stringr::str_which(life_stages, pred)
  total_population <- population + developing_population
  sum(total_population[life_stages_to_sum, time])
}

get_pred_time_period <- function(time, pred, is_delay, max_delay, life_stages) {

  # if is_delay, we want a long vector so the cumsum will reach 1
  # otherwise, we want a vector of length 1
  if (is_delay) {
    time <- time:(time + max_delay)
  }

  # If the predictor is the density of ticks or hosts,
  # we only want the predictor value(s) at the starting time of the transition.
  # This is a vector of length 1 for tick density - indicating the total number
  # of ticks of the given life stages.
  # This is a vector of length equal to the number of host species for host
  # density - indicating the density of each host species.
  # TODO "host_den" is hardcoded here as the only predictor from the predictors
  # table for which we only use the predictor value at the first day of the
  # transition. Should this be part of the configuration for each predictor?
  # The current behavior is indeed likely what we want for tick density as a pred.
  if (!is.na(pred)) {
    if ((pred == "host_den") || any(stringr::str_detect(life_stages, pred))) {
      time <- time[1]
    }
  }
  time
}

#' Get the value of a predictor
#'
#' TODO docs are out of date
#' @param time Numeric vector indicating span of days to get predictor values
#' @param pred String indicating which predictor, one of: 'temp', 'vpd',
#'   'host_den' or NA
#' @param is_delay Boolean indicating whether the predictor is for a transition
#'   involving a delay
#' @param population Tick population matrix. See get_tick_den for details.
#' @param developing_population Matrix of currently developing ticks.
#'   See get_tick_den for details.
#' @param max_delay Numeric vector of length one. Determines the maximum
#' number of days that a delayed transition can last.
#' @param predictors Table of predictor values
#' @noRd
#'
#' @returns a vector of a predictor at time time. The vector's length is based
#' on whether the transition is_delay.
get_pred <- function(
    time, pred, is_delay, population, developing_population, max_delay,
    predictors
  ) {
  life_stages <- rownames(population)
  time <- get_pred_time_period(
    time = time,
    pred = pred,
    is_delay = is_delay,
    max_delay = max_delay,
    life_stages = life_stages
  )

  if (is.na(pred)) {
    NULL
  } else if (pred %in% valid_predictors_from_table(predictors)) {
    get_pred_from_table(time, pred, predictors)
  } else if (any(stringr::str_detect(life_stages, pred))) {
    get_tick_den(time, pred, population, developing_population)
  } else {
    stop("failed to match predictor: \"", pred, "\"")
  }
}

#' Get the value determining probability or duration of a transition
#'
#' @details
#' This generic function pulls out the functional form and parameters needed to
#' make the transition function, then evaluates it using supplied predictors
#' (pred1, pred2) like temperature or host density. To identify a transition,
#' this approach takes an entire transition_row, since there can be multiple
#' rows with the same 'from' and 'to'.
#'
#' @param time Numeric vector indicating span of days to get predictor values
#' # TODO seems like at this point, time can only be a numeric vector of length
#' # 1; it grows in length for certain delay transitions in get_pred().
#' @param transition_row_with_parameters A row from the tick_transitions tibble
#'   with parameters added.
#' @param population Tick population matrix. See get_tick_den for details.
#' @param developing_population Matrix of currently developing ticks. See
#'   get_tick_den for details.
#' @param max_delay Numeric vector of length one. Determines the maximum
#' number of days that a delayed transition can last.
#' @param predictors Table of predictor values
#' @noRd
#'
#' @return Numeric vector indicating probability or duration of a transition.
get_transition_val <- function(time, transition_row_with_parameters, population,
                               developing_population, max_delay, predictors) {
  # get the function
  f <- get(transition_row_with_parameters[["transition_fun"]])

  # get the parameters for the function
  params <- transition_row_with_parameters[["params_list"]]

  # get the value of the predictors for the function
  pred1 <- get_pred(
    time, transition_row_with_parameters[["pred1"]],
    transition_row_with_parameters[["delay"]], population,
    developing_population, max_delay, predictors
  )
  pred2 <- get_pred(
    time, transition_row_with_parameters[["pred2"]],
    transition_row_with_parameters[["delay"]], population,
    developing_population, max_delay, predictors
  )

  # evaluate the function
  do.call(f, c(list(pred1, pred2), params[[1]]))
}


#' Generate a matrix of transition probabilities between tick life stages
#'
#' @param time Numeric vector indicating day to get transition probabilities
#' @param population Tick population matrix. See get_tick_den for details.
#' @param developing_population Matrix of currently developing ticks. See
#'   get_tick_den for details.
#' @param tick_transitions Tick transitions tibble
#' @param predictors Table of predictor values
#'
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @return Matrix of transition probabilities, indicating the probabilities of
#'   transitioning from each stage (axis 1) to each stage (axis 2).
#'
#' @noRd
gen_trans_matrix <- function(
    time, population, developing_population, tick_transitions, predictors
  ) {
  # initialize the transition matrix with 0s
  life_stages <- rownames(population)
  n_life_stages <- length(life_stages)
  trans_matrix <- matrix(0,
    ncol = n_life_stages, nrow = n_life_stages,
    dimnames = list(life_stages, life_stages)
  )


  transitions <- tick_transitions %>%
    filter(
      !.data$delay,
      .data$to %in% life_stages
    ) # exclude mortality

  mort <- tick_transitions %>%
    filter(
      !.data$delay,
      !(.data$to %in% life_stages)
    )

  if (nrow(transitions) > 0) {
    for (t in seq_len(nrow(transitions))) {
      trans_matrix[transitions[t, ]$from, transitions[t, ]$to] <-
        get_transition_val(
          time, transitions[t, ], population, developing_population, NULL, predictors
        )
    }
  }

  if (nrow(mort) > 0) {
    for (m in seq_len(nrow(mort))) {
      from_stage <- mort[m, ]$from

      mortality <- get_transition_val(
        time, mort[m, ], population, developing_population, NULL, predictors
      )

      # The max(0, 1- ...) structure should ensure that survival is between 0
      # and 1. This line also ensures that when a reproductive tick lays
      # (multiple) eggs, the reproductive tick will not survive to the next
      # stage. This is the desired behavior because all ticks are semelparous.
      trans_matrix[from_stage, from_stage] <-
        max(0, 1 - sum(trans_matrix[from_stage, ]) - mortality)
    }
  }

  return(trans_matrix)
}

#' Update the delay array for the current time
#'
#' @details
#'
#' @param time Numeric vector indicating day to get transition probabilities
#' @param delay_arr Delay array from previous time step
#' @param population Tick population matrix. See get_tick_den for details.
#' @param developing_population Matrix of currently developing ticks. See
#'   get_tick_den for details.
#' @param tick_transitions Tick transitions tibble
#' @param predictors Table of predictor values
#' @param max_delay Numeric vector of length one. Determines the maximum
#' number of days that a delayed transition can last.
#'
#' @importFrom dplyr pull
#'
#' @return Delay array indicating the number of ticks currently undergoing delay
#'   transitions. Axis 1 is the from_stage, axis 2 is the to_stage, and axis 3
#'   is the day on which the ticks will emerge from the transition. The value
#'   at a given cell is the number of ticks emerging from the transition.
#' @noRd
update_delay_arr <- function(
    time, delay_arr, population, developing_population, tick_transitions,
    max_delay, predictors
  ) {
  life_stages <- rownames(population)
  # select all delay transition functions, including mortality
  transitions <- tick_transitions[tick_transitions$delay, ]

  # loop through these transitions by from_stage
  from_stages <- unique(pull(transitions, .data$from)) # TODO should use $ rather than pull
  for (from_stage in from_stages) {
    # for a given delay transition, every "from" stage has a unique "to" stage
    trans <- transitions[transitions$from == from_stage &
      transitions$to %in% life_stages, ]

    to_stage <- trans[["to"]]

    # daily probability of transitioning to the next stage
    val <- get_transition_val(
      time, trans, population, developing_population, max_delay, predictors
    )

    # daily or per capita mortality during the delayed transition
    # each "from" stage has either 1 or 0 corresponding mortality transitions

    mort_tibble <- transitions[transitions$from == from_stage &
      !(transitions$to %in% life_stages), ]

    if (nrow(mort_tibble) == 1) {
      mort <- get_transition_val(
        time, mort_tibble[1, ], population, developing_population,
        max_delay, predictors
      )
    } else if (nrow(mort_tibble) == 0) {
      mort <- 0
    } else {
      stop(
        nrow(mort_tibble),
        " mortality transitions found from a single stage, should be 1 or 0"
      )
    }

    # Constant functions (for a fixed delay transition) return a single value
    # We increase the length so that we can do a cumsum over the vector
    # We add 1 for consistency with output vector length from non-constant fxns,
    # which is determined by time:(time + max_delay) in get_pred()
    if (length(val) == 1) val <- rep(val, max_delay + 1)

    days <- cumsum(val) >= 1

    if (any(days)) {
      # delay duration is the number of days until the first day when the sum
      # of the daily probabilities >= 1
      days_to_next <- min(which(days))

      if (length(mort) > 1) {
        # Non-constant mortality
        # We might ultimately want density_fun() to return a vector of length >
        # 1, where the value for each day is calculated based on that day's
        # feeding tick population. But currently, all mortality transitions are
        # either constant_fun() or density_fun(), both of which return a vector
        # of length 1, so we shouldn't ever get to this case.
        stop("Found non-constant mortality for a delay transition. This
             functionality has not been tested",
          call. = FALSE
        )

        # in this case, mort is a vector of length max_delay. we subset it for
        # the duration of the delay then elementwise subtract (1 - each element)
        # to get vector of daily survival rate, and take product to get the
        # overall survival rate throughout the delay
        surv_to_next <- prod(1 - mort[1:days_to_next])
      } else if (nrow(mort_tibble) == 1 &&
        mort_tibble["to"] == "per_capita_m") {
        # Apply per capita mortality once during transition, rather than every
        # day
        surv_to_next <- 1 - mort
      } else {
        # Constant mortality
        surv_to_next <- (1 - mort)^days_to_next
      }

      # number of ticks emerging from from_stage to to_stage at time +
      # days_to_next is the number of ticks that were already going to emerge
      # then plus the current number of ticks in the from_stage * survival
      delay_arr[from_stage, to_stage, time + days_to_next] <-
        delay_arr[from_stage, to_stage, time + days_to_next] +
        population[from_stage, time] * surv_to_next
    } else {
      stop(
        "cumsum of daily transition probabilities never reached 1,",
        "max_delay may be too small"
      )
    }
  }
  return(delay_arr)
}

#' Return list of parameters for a given transition
#'
#' @details
#' This function is used for an optimization where we preprocess the model
#' inputs by joining the transitions and parameters into a tibble once rather
#' than searching for parameters for each function each time the function is
#' called.
#'
#' @param from String indicating life stage a transition is from
#' @param to String indicating life stage a transition is to
#' @param parameters Tick parameters tibble
#'
#' @importFrom stringr str_which
#'
#' @return Named list of parameters needed for the transition function from the
#'   `from` life stage to the `to` life stage.
#'
#' @noRd
get_params <- function(from, to, parameters) {
  string <- paste0(from, to)
  patterns <- paste0(parameters$from, parameters$to)

  params_tbl <- parameters[str_which(string, patterns), ]

  params <- params_tbl$param_value
  names(params) <- params_tbl$param_name
  params <- split(unname(params), names(params))

  params
}

#' Add parameters to a tick_transitions tibble
#'
#' @param tick_transitions Tick transitions tibble
#' @param parameters Tick parameters tibble
#' @importFrom dplyr mutate
#'
#' @return A new tick_transitions tibble with a column, params_list
#'   that is a named list of the parameters for each transition.
#'
#' @noRd
add_params_list <- function(tick_transitions, parameters) {
  params_list <- apply(tick_transitions, 1, function(x) {
    get_params(x[["from"]], x[["to"]], parameters)
  })

  mutate(tick_transitions, params_list = params_list)
}

#' Generate an empty delay array
#' TODO dimensions are one of...
#' dimensions: to, from, time
#' dimensions: from, to, time
empty_delay_array <- function(life_stages, steps, max_duration) {
  array(
    dim = c(length(life_stages), length(life_stages), steps + max_duration),
    dimnames = list(life_stages, life_stages, NULL),
    data = 0
  )
}

#' Create an empty (zero population) population matrix
empty_population_matrix <- function(life_stages, steps) {
  matrix(
    data = 0,
    nrow = length(life_stages),
    ncol = steps,
    dimnames = list(life_stages)
  )
}

#' Set initial population for each life stage - zero if not specified in cfg
set_initial_population <- function(population, initial_population) {
  population[, 1] <- vapply(
    rownames(population),
    function(stage) {
      ifelse(stage %in% names(initial_population), initial_population[[stage]], 0)
    },
    FUN.VALUE = numeric(1L)
  )
  population
}

#' Run the model
#'
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr row_number
#'
#' @param cfg An `IxPopDyMod::config` object
#'
#' @return Data frame of population of ticks of each life stage each day
#'
#' @examples
#' run(config_ex_1)
#'
#' @export
run <- function(cfg) {

  # 00 get valid life stages
  life_stages <- get_life_stages(cfg$transitions)

  # 01 combine transitions and parameters
  # TODO no longer relevant with new (these are already combined in new config)
  transitions_with_params <- add_params_list(cfg$transitions, cfg$parameters)

  # 02 initialize a delay array of all zeros
  delay_arr <- empty_delay_array(
    life_stages = life_stages,
    steps = cfg$steps,
    max_duration = cfg$max_delay
  )

  # 03 initialize a population matrix with initial_population
  population <- empty_population_matrix(life_stages = life_stages, steps = cfg$steps)
  population <- set_initial_population(
    population = population, initial_population = cfg$initial_population
  )

  # Initialize a population matrix to keep track of the number of individuals of
  # each stage that are currently developing (currently undergoing a delay)
  developing_population <- empty_population_matrix(life_stages = life_stages, steps = cfg$steps)

  # at each time step:
  # (1) generate a new trans_matrix based on conditions at "time"
  # (2) update the delay_arr based on conditions at "time"
  # (3) update the population matrix "N" for "time + 1"

  # at each time step
  for (time in 1:(cfg$steps - 1)) {
    if (time %% 100 == 0) print(paste("day", time))

    # Calculate the number of ticks currently in delayed development NOT
    # INCLUDING those added on current day, because that would be double
    # counting ticks in the population matrix, N. We exclude those added on
    # current day by calculating developing_population before updating
    # delay_arr. We slice the delay array from the current time + 1 to the end.
    # We add 1 because ticks that emerge from delay at current time would have
    # been added to population on the previous iteration when we update
    # N[, time + 1] by adding delay_mat[, time + 1]. We sum across to_stage and
    # days to get a vector of the number of ticks currently developing FROM each
    # life stage.
    developing_population[, time] <- rowSums(
      delay_arr[, , (time + 1):dim(delay_arr)[3]]
    )

    # calculate transition probabilities
    trans_matrix <- gen_trans_matrix(
      time, population, developing_population,
      transitions_with_params, cfg$predictors
    )

    # calculate the number of ticks entering delayed development
    delay_arr <- update_delay_arr(
      time, delay_arr, population, developing_population,
      transitions_with_params, cfg$max_delay, cfg$predictors
    )

    # collapse the delay_arr by summing across 'from', giving a matrix with
    # dims = (to, days)
    delay_mat <- colSums(delay_arr)

    # calculate the number of ticks at the next time step, which is
    # current population * transition probabilities + ticks emerging from
    # delayed development
    population[, time + 1] <-
      population[, time] %*% trans_matrix + delay_mat[, time + 1]
  }

  # Return the total population of ticks each day. Developing ticks are counted
  # in the FROM stage.
  (population + developing_population) %>%
    t() %>%
    as.data.frame() %>%
    mutate(day = row_number()) %>%
    pivot_longer(-c(.data$day), names_to = "stage", values_to = "pop") %>%
    mutate(
      age_group = age(.data$stage),
      process = process(.data$stage),
      infected = infected(.data$stage)
    )
}
