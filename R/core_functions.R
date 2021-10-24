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
  ifelse(substr(life_stage, 0, 1) != '', substr(life_stage, 0, 1), '')
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

#' Get temperature from input data
#'
#' @param time Numeric vector of days to get data
#' @param weather weather df from `config` object
#' @return Numeric vector of temperature
#' @noRd
get_temp <- function(time, weather) {
  weather[which(weather$j_day %in% time), ]$tmean
}

#' Get vapour-pressure deficit from input data
#'
#' @param time Numeric vector of days to get data
#' @param weather weather df from `config` object
#' @return Numeric vector of vpd
#' @noRd
get_vpd <- function(time, weather) {
  weather[which(weather$j_day %in% time), ]$vpdmean
}

#' Get host density from input data
#'
#' @param time Numeric vector of days to get data
#' @param host_comm host_comm df from `config` object
#' @return Numeric vector of host density
#' @noRd
get_host_den <- function(time, host_comm) {
  host_comm[which(host_comm$j_day %in% time), ]$host_den
}


#' Get tick density for specified time and life stages
#'
#' @param time Numeric vector of length one indicating day to get tick density
#' @param pred Character vector indicating life stages to get tick
#'   density of
#' @param N Matrix of number of ticks not currently undergoing development
#'   per life stage per day
#' @param N_developing Matrix of number of currently developing ticks per life
#'   stage per day
#' @param life_stages Character vector of life stages.
#' @importFrom stringr str_which
#' @return Numeric vector of length one indicating current number of ticks in
#'   given life stages
#' @noRd
get_tick_den <- function(time, N, N_developing, pred, life_stages) {
  sum((N + N_developing)[str_which(life_stages, pred), time])
}

#' Get the value of a predictor
#'
#' @param time Numeric vector indicating span of days to get predictor values
#' @param pred String indicating which predictor, one of: 'temp', 'vpd',
#'   'host_den' or NA
#' @param is_delay Boolean indicating whether the predictor is for a transition
#'   involving a delay
#' @param N Tick population matrix. See get_tick_den for details.
#' @param N_developing Matrix of currently developing ticks. See get_tick_den
#'   for details.
#' @param max_delay Numeric vector of length one. Determines the maximum
#' number of days that a delayed transition can last.
#' @param life_stages Character vector of life stages.
#' @param host_comm Host community tibble.
#' @param weather Weather tibble.
#' @noRd
#'
# Return a vector of a predictor at time time.
# The vector's length is based on whether the transition is_delay.
get_pred <- function(time, pred, is_delay, N, N_developing, max_delay,
                     life_stages, host_comm, weather) {

  # if is_delay, we want a long vector so the cumsum will reach 1
  # otherwise, we want a vector of length 1
  if (is_delay) {time <- time:(time + max_delay)}

  if (is.na(pred)) {
    return(NULL)
  } else if (pred == "temp") {
    return(get_temp(time, weather))
  } else if (pred == "vpd") {
    return(get_vpd(time, weather))
  } else if (pred == "host_den") {
    return(get_host_den(time[1], host_comm)) # length == n_host_spp
  } else if (any(str_detect(life_stages, pred))) {
    return(get_tick_den(time[1], N, N_developing, pred, life_stages)) # length == 1
  } else {
    stop("error: couldn't match pred")
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
#' @param transition_row_with_parameters A row from the tick_transitions tibble
#'   with parameters added.
#' @param N Tick population matrix. See get_tick_den for details.
#' @param N_developing Matrix of currently developing ticks. See get_tick_den
#'   for details.
#' @param max_delay Numeric vector of length one. Determines the maximum
#' number of days that a delayed transition can last.
#' @param life_stages Character vector of life stages.
#' @param host_comm Host community tibble.
#' @param weather Weather tibble.
#' @noRd
#'
#' @return Numeric vector indicating probability or duration of a transition.
get_transition_val <- function(time, transition_row_with_parameters, N,
                               N_developing, max_delay, life_stages,
                               host_comm, weather) {

  # get the function
  f <- get(transition_row_with_parameters[['transition_fun']])

  # get the parameters for the function
  params <- transition_row_with_parameters[['params_list']]

  # get the value of the predictors for the function
  pred1 <- get_pred(time, transition_row_with_parameters[['pred1']],
                    transition_row_with_parameters[['delay']], N, N_developing,
                    max_delay, life_stages, host_comm, weather)
  pred2 <- get_pred(time, transition_row_with_parameters[['pred2']],
                    transition_row_with_parameters[['delay']], N, N_developing,
                    max_delay, life_stages, host_comm, weather)

  # evaluate the function
  do.call(f, c(list(pred1, pred2), params[[1]]))
}


#' Generate a matrix of transition probabilities between tick life stages
#'
#' @param time Numeric vector indicating day to get transition probabilities
#' @param N Tick population matrix. See get_tick_den for details.
#' @param N_developing Matrix of currently developing ticks. See get_tick_den
#'   for details.
#' @param life_stages Character vector of life stages.
#' @param tick_transitions Tick transitions tibble
#' @param host_comm Host community tibble.
#' @param weather Weather tibble.
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
gen_trans_matrix <- function(time, N, N_developing, life_stages,
                             tick_transitions, host_comm, weather) {

  # initialize the transition matrix with 0s
  n_life_stages <- length(life_stages)
  trans_matrix <- matrix(0, ncol = n_life_stages, nrow = n_life_stages,
                         dimnames = list(life_stages, life_stages))


  transitions <- tick_transitions %>%
    filter(!.data$delay,
           .data$to %in% life_stages)  # exclude mortality

  mort <- tick_transitions %>%
    filter(!.data$delay,
           !(.data$to %in% life_stages))

  if (nrow(transitions) > 0) {
    for (t in seq_len(nrow(transitions))) {
      trans_matrix[transitions[t,]$from, transitions[t,]$to] <-
        get_transition_val(time, transitions[t, ], N, N_developing, NULL,
                           life_stages, host_comm, weather)
    }
  }

  if (nrow(mort) > 0) {
    for (m in seq_len(nrow(mort))) {
      from_stage <- mort[m,]$from

      mortality <- get_transition_val(
        time, mort[m,], N, N_developing, NULL, life_stages, host_comm, weather)

      # The max(0, 1- ...) structure should ensure that survival is between 0
      # and 1. This line also ensures that when a reproductive tick lays
      # (multiple) eggs, the reproductive tick will not survive to the next
      # stage. This is the desired behavior because all ticks are semelparous.
      trans_matrix[from_stage, from_stage] <-
        max(0, 1 - sum(trans_matrix[from_stage,]) - mortality)
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
#' @param N Tick population matrix. See get_tick_den for details.
#' @param N_developing Matrix of currently developing ticks. See get_tick_den
#'   for details.
#' @param life_stages Character vector of life stages.
#' @param tick_transitions Tick transitions tibble
#' @param host_comm Host community tibble.
#' @param weather Weather tibble.
#'
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
update_delay_arr <- function(time, delay_arr, N, N_developing, tick_transitions,
                             life_stages, max_delay, host_comm, weather) {

  # select all delay transition functions, including mortality
  transitions <- tick_transitions[tick_transitions$delay, ]

  # loop through these transitions by from_stage
  from_stages <- unique(pull(transitions, .data$from))
  for (from_stage in from_stages) {

    # for a given delay transition, every "from" stage has a unique "to" stage
    trans <- transitions[transitions$from == from_stage &
                           transitions$to %in% life_stages, ]

    to_stage <- trans[['to']]

    # daily probability of transitioning to the next stage
    val <- get_transition_val(time, trans, N, N_developing, max_delay,
                              life_stages, host_comm, weather)

    # daily or per capita mortality during the delayed transition
    # each "from" stage has either 1 or 0 corresponding mortality transitions

    mort_tibble <- transitions[transitions$from == from_stage &
                                 !(transitions$to %in% life_stages), ]

    if(nrow(mort_tibble) == 1) {
      mort <- get_transition_val(time, mort_tibble[1,], N, N_developing,
                                 max_delay, life_stages, host_comm, weather)
    } else if (nrow(mort_tibble) == 0) {
      mort <- 0
    } else {
      stop(nrow(mort_tibble),
           ' mortality transitions found from a single stage, should be 1 or 0')
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
        stop('Found non-constant mortality for a delay transition. This
             functionality has not been tested',
             call. = FALSE)

        # in this case, mort is a vector of length max_delay. we subset it for
        # the duration of the delay then elementwise subtract (1 - each element)
        # to get vector of daily survival rate, and take product to get the
        # overall survival rate throughout the delay
        surv_to_next <- prod(1 - mort[1:days_to_next])

      } else if (nrow(mort_tibble) == 1 &&
                 mort_tibble['to'] == 'per_capita_m') {
        # Apply per capita mortality once during transition, rather than every
        # day
        surv_to_next <- 1 - mort

      } else {
        # Constant mortality
        surv_to_next <- (1 - mort) ^ days_to_next
      }

      # number of ticks emerging from from_stage to to_stage at time +
      # days_to_next is the number of ticks that were already going to emerge
      # then plus the current number of ticks in the from_stage * survival
      delay_arr[from_stage, to_stage, time + days_to_next] <-
        delay_arr[from_stage, to_stage, time + days_to_next] +
        N[from_stage, time] * surv_to_next
    } else {
      stop('cumsum of daily transition probabilities never reached 1,',
           'max_delay may be too small')
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

  params_list <- apply(tick_transitions, 1, function(x)
    get_params(x[["from"]], x[["to"]], parameters))

  mutate(tick_transitions, params_list = params_list)
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
#' @export
run <- function(cfg) {

  life_stages <- get_life_stages(cfg$transitions)

  # combine transitions and parameters
  transitions_with_params <- add_params_list(cfg$transitions, cfg$parameters)

  # initialize a delay array of all zeros
  # dimensions: to, from, time
  # dimensions: from, to, time
  delay_arr <- array(dim = c(length(life_stages),
                             length(life_stages),
                             cfg$steps + cfg$max_delay),
                     dimnames = list(life_stages, life_stages, NULL),
                     data = 0)

  # initialize a population matrix with initial_population
  N <- matrix(nrow = length(life_stages), ncol = cfg$steps, data = 0)
  N[,1] <-
    sapply(life_stages, function(x) {
      if (x %in% names(cfg$initial_population)) {
        cfg$initial_population[[x]]
      } else {
        0 # life stages not specified in cfg$initial_population
      }
    })
  rownames(N) <- life_stages

  # Initialize a population matrix to keep track of the number of individuals of
  # each stage that are currently developing (currently undergoing a delay)
  N_developing <- matrix(nrow = length(life_stages), ncol = cfg$steps, data = 0)
  rownames(N_developing) <- life_stages

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
    # current day by calculating N_developing before updating delay_arr. We
    # slice the delay array from the current time + 1 to the end. We add 1
    # because ticks that emerge from delay at current time would have been added
    # to N on the previous iteration when we update N[, time + 1] by adding
    # delay_mat[, time + 1]. We sum across to_stage and days to get a vector of
    # the number of ticks currently developing FROM each life stage.
    N_developing[, time] <- rowSums(delay_arr[,,(time + 1):dim(delay_arr)[3]])

    # calculate transition probabilities
    trans_matrix <- gen_trans_matrix(time, N, N_developing, life_stages,
                                     transitions_with_params, cfg$host_comm,
                                     cfg$weather)

    # calculate the number of ticks entering delayed development
    delay_arr <- update_delay_arr(time, delay_arr, N, N_developing,
                                  transitions_with_params, life_stages,
                                  cfg$max_delay, cfg$host_comm, cfg$weather)

    # collapse the delay_arr by summing across 'from', giving a matrix with
    # dims = (to, days)
    delay_mat <- colSums(delay_arr)

    # calculate the number of ticks at the next time step, which is
    # current population * transition probabilities + ticks emerging from
    # delayed development
    N[, time + 1] <- N[, time] %*% trans_matrix + delay_mat[, time + 1]
  }

  # Return the total population of ticks each day. Developing ticks are counted
  # in the FROM stage.
  (N + N_developing) %>%
    t() %>%
    as.data.frame() %>%
    mutate(day = row_number()) %>%
    pivot_longer(-c(.data$day), names_to = 'stage', values_to = 'pop') %>%
    mutate(age_group = age(.data$stage),
           process = process(.data$stage),
           infected = infected(.data$stage))
}
