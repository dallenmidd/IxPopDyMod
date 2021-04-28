# core_functions.R

# functions to extract tick age, process, and infection status from life_stage name
# currently these are just used for graphing

# return age of tick in life_stage
# e.g. age("fl") = "l"
age <- function(life_stage) {
  substr(life_stage, str_length(life_stage), str_length(life_stage))
}

# return current process of tick in life_stage
process <- function(life_stage) {
  ifelse(substr(life_stage, 0, 1) != '', substr(life_stage, 0, 1), '')
}

# return whether a life_stage is infected
# unlike the 3 previous and similar functions, infected() returns a boolean
infected <- function(life_stage) {
  str_detect(life_stage, "i")
  
}

# 01 functions to grab the predictors that determine the transition probabiltiies at a given time

# extract temperature from input data at time time 
get_temp <- function(time) {
  weather[which(weather$j_day %in% time), ]$tmean
}

# extract vapour-pressure deficit from input data at time time 
get_vpd <- function(time) {
  weather %>% 
    filter(j_day %in% time) %>%
    pull(vpdmean)
} 

get_host_den <- function(time) {
  host_comm[which(host_comm$j_day %in% time), ]$host_den
}

# Return a vector of a predictor at time time. 
# The vector's length is based on whether the transition is_delay.
get_pred <- function(time, pred, is_delay, N, N_developing) {
  
  # if is_delay, we want a long vector so the cumsum will reach 1
  # otherwise, we want a vector of length 1
  if (is_delay) {time <- time:(time + max_delay)}
  
  if (is.na(pred)) {
    return(NULL)
  } else if (pred == "temp") {
    return(get_temp(time))
  } else if (pred == "vpd") {
    return(get_vpd(time))
  } else if (pred == "host_den") {
    return(get_host_den(time[1])) # length == n_host_spp
  } else if (any(str_detect(life_stages, pred))) {
    return(sum((N + N_developing)[str_subset(life_stages, pred), time[1]])) # length == 1
  } else {
    print("error: couldn't match pred")
  }
}

# 03
# calculate individual transition probabilities for advancing to consecutive life stage

# This generic function will pull out the functional form and parameters needed to make the transition function, 
# then evaluate it using supplied predictors (pred1, pred2) like temperature or host density
# This approach takes an entire transition_row, since there can be multiple rows with the same 'from' and 'to'
# transition_row: a row from the tick_transitions tibble
get_transition_val <- function(time, transition_row_with_parameters, N, N_developing) {
  
  f <- get(transition_row_with_parameters[['transition_fun']])
  
  params <- transition_row_with_parameters[['params_list']]
  
  pred1 <- get_pred(time, transition_row_with_parameters[['pred1']], 
                    transition_row_with_parameters[['delay']], N, N_developing)
  pred2 <- get_pred(time, transition_row_with_parameters[['pred2']], 
                    transition_row_with_parameters[['delay']], N, N_developing)
  
  do.call(f, c(list(pred1, pred2), params[[1]]))
}


# 04
# at each step, we generate a new transition matrix whose transition probabilities
# are based on the input data (weather, host_community) at that time 
gen_trans_matrix <- function(time, N, N_developing) {
  
  # initialize the transition matrix with 0s
  n_life_stages <- length(life_stages)
  trans_matrix <- matrix(0, ncol = n_life_stages, nrow = n_life_stages, 
                         dimnames = list(life_stages, life_stages))
  
  transitions <- tick_transitions %>% 
    filter(delay == 0,
           to %in% life_stages)  # exclude mortality
  
  mort <- tick_transitions %>%
    filter(delay == 0, 
           !(to %in% life_stages))
  
  if (nrow(transitions) > 0) {
    for (t in seq_len(nrow(transitions))) {
      trans_matrix[transitions[t,]$from, transitions[t,]$to] <- 
        get_transition_val(time, transitions[t, ], N, N_developing)
    }
  }
  
  if (nrow(mort) > 0) {
    for (m in seq_len(nrow(mort))) {
      from_stage <- mort[m,]$from
      
      mortality <- get_transition_val(time, mort[m,], N, N_developing) 
      trans_prob_sum <- sum(trans_matrix[from_stage,], mortality)
      
      # sum of transition probabilities plus mortality should not exceed 1
      # unless we're coming from a reproductive stage
      if (trans_prob_sum > 1 && !str_detect(from_stage, 'r.a')) {
        stop('transition probability from ', from_stage, ' = ', trans_prob_sum, ', but should be <= 1')
      }
      
      # The max(0, 1- ...) structure should ensure that survival is between 0 and 1
      # This line also ensures that when a reproductive tick lays (multiple) eggs, the
      # reproductive tick will not survive to the next stage. This is the desired behavior because
      # all ticks are semelparous.
      trans_matrix[from_stage, from_stage] <- max(0, 1 - sum(trans_matrix[from_stage,]) - mortality)
    }
  }
  
  return(trans_matrix)
}


# based on the current time, delay_arr and N (population matrix), return a delay_arr for the next time step
update_delay_arr <- function(time, delay_arr, N, N_developing) {
  
  # select all delay transition functions, including mortality
  transitions <- tick_transitions[tick_transitions$delay == 1, ]
  
  # loop through these transitions by from_stage 
  from_stages <- unique(pull(transitions, from))
  for (from_stage in from_stages) {
    
    # for a given delay transition, every "from" stage has a unique "to" stage
    trans <- transitions[transitions$from == from_stage &
                           transitions$to %in% life_stages, ]
    
    to_stage <- trans[['to']]
    
    # daily probability of transitioning to the next stage
    val <- get_transition_val(time, trans, N, N_developing)
    
    # daily or per capita mortality during the delayed transition
    # each "from" stage has either 1 or 0 corresponding mortality transitions
    
    mort_tibble <- transitions[transitions$from == from_stage &
                                 !(transitions$to %in% life_stages), ]
    
    if(nrow(mort_tibble) == 1) {
      mort <- get_transition_val(time, mort_tibble[1,], N, N_developing)    
    } else if (nrow(mort_tibble) == 0) {
      mort <- 0 
    } else {
      stop(nrow(mort_tibble), ' mortality transitions found from a single stage, should be 1 or 0')
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
        # We might ultimately want density_fun() to return a vector of length > 1,
        # where the value for each day is calculated based on that day's feeding tick population
        # But currently, all mortality transitions are either constant_fun() or density_fun(), 
        # both of which return a vector of length 1, so we shouldn't ever get to this case.
        print('non-constant mortality')
        
        # in this case, mort is a vector of length max_delay?
        # we susbet it for the duration of the delay
        # then elementwise subtract (1 - each element) to get vector of daily survival rate,
        # and take product to get the overall survival rate throughout the delay
        surv_to_next <- prod(1 - mort[1:days_to_next])
        
      } else if (nrow(mort_tibble) == 1 && mort_tibble['to'] == 'per_capita_m') {
        # Apply per capita mortality once during transition, rather than every day
        surv_to_next <- 1 - mort
        
      } else {
        # Constant mortality
        surv_to_next <- (1 - mort) ^ days_to_next
      }
      
      # number of ticks emerging from from_stage to to_stage at time + days_to_next is the number of ticks 
      # that were already going to emerge then plus the current number of ticks in the from_stage * survival
      delay_arr[to_stage, from_stage, time + days_to_next] <- 
        delay_arr[to_stage, from_stage, time + days_to_next] +
        N[from_stage, time] * surv_to_next
    }
  }
  return(delay_arr)
}

# return list of parameters for a given transition_row
get_params <- function(from, to, parameters = tick_params) {
  
  string <- paste0(from, to)
  patterns <- paste0(parameters$from, parameters$to)
  
  params_tbl <- parameters[str_which(string, patterns), ]
  
  params <- params_tbl$param_value
  names(params) <- params_tbl$param_name
  params <- split(unname(params), names(params))
  
  params
}

# return a new tick_transitions tibble with a column, params_list
# that is a named list of the parameters for each transition
add_params_list <- function(tick_transitions, parameters = tick_params) {
  
  # TODO should be a vectorized approach
  params_list <- list()
  
  for (i in 1:nrow(tick_transitions)) {
    p <- get_params(tick_transitions[[i,'from']], tick_transitions[[i, 'to']])
    params_list[[i]] <- p
  }
  
  mutate(tick_transitions, params_list = params_list)
}


# 05 iteratively run model for steps iterations, starting with initial_population
run <- function(steps, initial_population) {
  
  # update the tick transitions global variable by adding parameters 
  # for each transition
  # TODO would be better not to update a global variable
  tick_transitions <<- add_params_list(tick_transitions)
  
  # initialize a delay array of all zeros
  # dimensions: to, from, time
  delay_arr <- array(dim = c(length(life_stages), length(life_stages), steps + max_delay),
                     dimnames = list(life_stages, life_stages, NULL),
                     data = 0)
  
  
  # intialize a population matrix with initial_population
  N <- matrix(nrow = length(life_stages), ncol = steps, data = 0)
  N[,1] <- initial_population 
  rownames(N) <- life_stages
  
  # Initialize a population matrix to keep track of the number of individuals of each stage
  # that are currently developing (currently undergoing a delay)
  N_developing <- matrix(nrow = length(life_stages), ncol = steps, data = 0)
  rownames(N_developing) <- life_stages
  
  # at each time step:
  # (1) generate a new trans_matrix based on conditions at "time"
  # (2) update the delay_arr based on conditions at "time" 
  # (3) update the population matrix "N" for "time + 1"
  
  # at each time step
  for (time in 1:(steps - 1)) {
    
    if (time %% 100 == 0) print(paste("day", time))
    
    # Calculate the number of ticks currently in delayed development NOT INCLUDING those 
    # added on current day, because that would be double counting ticks in the population matrix, N. 
    # We exclude those added on current day by calculating N_developing before updating delay_arr
    # Slice the delay array from the current time + 1 to the end. We add 1 because ticks that 
    # emerge from delay at current time would have been added to N on the previous iteration 
    # when we update N[, time + 1] by adding delay_mat[, time + 1]
    # sum across columns (to_stage)
    # sum across rows (days)
    # Result is a vector of the number of ticks currently developing FROM each life stage
    N_developing[, time] <- rowSums(colSums(delay_arr[,,(time + 1):dim(delay_arr)[3]]))
    
    # calculate transition probabilities
    trans_matrix <- gen_trans_matrix(time, N, N_developing)
    
    # calculate the number of ticks entering delayed development
    delay_arr <- update_delay_arr(time, delay_arr, N, N_developing)
    
    # collapse the delay_arr by summing across 'from', giving a matrix with dims = (to, days)
    delay_mat <- apply(delay_arr, 3, rowSums)
    
    # calculate the number of ticks at the next time step, which is 
    # current population * transition probabilities + ticks emerging from delayed development
    N[, time + 1] <- N[, time] %*% trans_matrix + delay_mat[, time + 1]
  }
  
  # the population matrix N and delay_mat are local variables
  # we return them here after running "steps" times
  return(list(N, N_developing, delay_mat))
}
