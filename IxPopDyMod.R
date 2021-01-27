## Ixodes population dynamics model
## Dave Allen and Myles Stokowski

library(tidyverse)


# set constant steps, which ensures that model doesn't try to run for longer than there is input data
steps <- 300

# At each time step, how many time steps should we look into the future to see if any
# ticks emerge from a time delay? This value is a somewhat arbitrarily high constant.
max_delay <- 300

# 00
# read inputs 
# weather <- read_csv('inputs/weather.csv')
# constant temperature for testing
weather <- tibble(tmean = seq(from = 20, to = 20, length.out = steps), j_day = seq(from = 1, to = steps))
host_comm <- tibble(
  j_day = rep(1:steps, each=3), 
  host_spp = rep(c('mouse', 'squirrel', 'deer'), steps), 
  host_den = rep(c(40, 8, 0.25), steps)) %>%
  # host_den = runif(steps * 3, .75, 1.25) * rep(c(40, 8, 0.25), steps)) %>% 
  arrange(j_day, host_spp)
n_host_spp <- host_comm %>% pull(host_spp) %>% unique() %>% length()


# option to run on simple inputs for testing
simple <- FALSE

if (simple) {
  tick_params <- read_csv('inputs/tick_parameters_simple_stable_delay2.csv') # %>% arrange(host_spp)
  tick_funs <- read_csv('inputs/tick_functions_simple_delay.csv')
  life_stages <- read_csv('inputs/tick_stages_simple.csv')[[1]]
} else {
  tick_params <- read_csv('inputs/tick_parameters.csv') %>% 
    arrange(host_spp) # sort so parameters are in same host_spp order for pairwise vector calculations
  tick_funs <- read_csv('inputs/tick_functions.csv')
  #life_stages <- read_csv('inputs/tick_stages.csv')[[1]]
  life_stages <- tick_funs %>% pull(from) %>% unique()
}

# set initial population
initial_population <- runif(length(life_stages), 0, 0)
names(initial_population) <- life_stages
initial_population['eua'] <- 10 # start with only one cohort (adults)

# 01 functions to grab the predictors that determine the transition probabiltiies at a given time

# extract temperature from input data at time time 
get_temp <- function(time) {
  weather %>%
    filter(j_day %in% time) %>%
    pull(tmean)
}

# extract vapour-pressure deficit from input data at time time 
get_vpd <- function(time) {
  weather %>% 
    filter(j_day %in% time) %>%
    pull(vpdmean)
} 

get_host_den <- function(time) {
  host_comm %>%
    filter(j_day %in% time) %>%
    pull(host_den)
}

# Return a vector of a predictor at time time. The vector's length is based on whether the transition is_delay.
get_pred <- function(time, pred, is_delay, N) {
  
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
    return(get_host_den(time))
  } else if (pred %in% life_stages) {
    #print('density_dep_mort')
    return(N[pred, time[1]] %>% unname())
    #return(N[pred, time[1]:time[max(which(time <= steps))]] %>% unname()) 
  } else {
    print("error: couldn't match pred")
  }
}

# vector subset: return a subset of named vector v where the names are in sub_names
v_sub <- function(v, sub_names) subset(v, names(v) %in% sub_names)

# 02 functional forms for transition probabilities

expo_fun <- function(x, y, p) ifelse(x>0,p['a']*x^p['b'],0)
briere_fun <- function(x, y, p) ifelse(x>p['tmin'] & x<p['tmax'],p['q']*x*(x-p['tmin'])*sqrt(p['tmax']-x),0) # https://doi.org/10.7554/eLife.58511
constant_fun <- function(x, y, p) p['a'] 
binomial_fun <- function(x, y, p) 1-(1-p['a'])^x

feed_fun <- function(x, y, p) {
  binomial_fun(
    x = sum(x * v_sub(p, 'pref')),
    y = NULL,
    p = p
  )}

engorge_fun <- function(x, y, p) sum(ifelse(rep(p['from_infected'], n_host_spp), 1, abs(ifelse(p['to_infected'], 0, 1) - v_sub(p, 'host_rc'))) * 
                                       (v_sub(p, 'feed_success') * ((x * v_sub(p, 'pref')) / sum(x * v_sub(p, 'pref')))))


# density dependent mortality
# x = host_den, y = number of feeding ticks
density_fun <- function(x, y, p) sum((v_sub(p, 'a') + (v_sub(p, 'b') * log((v_sub(p, 'c') + y * v_sub(p, 'pref') * x / sum(v_sub(p, 'pref') * x)) / x))) * 
                                                                                            v_sub(p, 'pref') * x / sum(v_sub(p, 'pref') * x))

# 03
# functions that calculate individual transition probabilities for advancing to consecutive life stage

# this generic function will pull out the functional form and parameters needed to make the transition function, 
# then evaluate it using supplied predictors (pred1, pred2) like temperature or some host community metric
get_transition_val <- function(which_from, which_to, pred1 = NULL, pred2 = NULL, functions = tick_funs, parameters = tick_params) {
  
  # this is problematic if we have multiple functions with the same from and to,
  # because it will always get() the first function 
  f <- functions %>%
    filter(from == which_from, to == which_to) %>%
    pull(transition_fun) %>%
    get()
  
  # I think parameter handling can still happen this way with multiple
  # functions with the same from and to, becuase parameters are selected by
  # name. Just CANNOT have parameters with same name for different functions
  # that with same from and to
  params <- parameters %>%
    filter(from == which_from, to == which_to) %>%
    pull(param_value)
  
  names(params) <- parameters %>%
    filter(from == which_from, to == which_to) %>%
    pull(param_name)
  
  # f(x = pred1, y = pred2, p = params, h = host_community) %>% unname()
  f(x = pred1, y = pred2, p = params) %>% unname()
}

# alternative approach that takes an entire transition row, which is a workaround for 
# having multiple rows with the same 'from' and 'to'
# transition_row: a row from the tick_funs tibble
get_transition_val2 <- function(time, transition_row, N, parameters = tick_params) {
  
  f <- transition_row[['transition_fun']] %>% get()
  
  params <- parameters %>%
    filter(from == transition_row[['from']], to == transition_row[['to']]) %>%
    pull(param_value)
  
  names(params) <- parameters %>%
    filter(from == transition_row[['from']], to == transition_row[['to']]) %>%
    pull(param_name)
  
  f(x = get_pred(time, transition_row[['pred1']], transition_row[['delay']], N), 
    y = get_pred(time, transition_row[['pred2']], transition_row[['delay']], N),
    p = params) %>% unname()
}

# 04
# at each step, we generate a new transition matrix whose transition probabilities
# are based on the input data (weather, host_community) at that time 
# Myles note: I think all the transitions we're handling this way are questing -> feeding
gen_trans_matrix <- function(time, N) {
  
  # initialize the transition matrix with zeros
  n_life_stages <- length(life_stages)
  trans_matrix <- matrix(0, ncol = n_life_stages, nrow = n_life_stages, 
                         dimnames = list(life_stages, life_stages))
  
  transitions <- tick_funs %>% 
    filter(delay == 0,
           from %in% life_stages, # need until all tick_funs lines are to and from life stage names (or m)
           to %in% life_stages,  # exclude mortality
           is.na(todo))          
  
  mort <- tick_funs %>%
    filter(delay == 0, 
           from %in% life_stages,
           to == 'm',
           is.na(todo))
  

  if (nrow(transitions) > 0) {
    for (t in seq_len(nrow(transitions))) {
      from <- transitions[t,]$from
      to <- transitions[t,]$to
      # If there are multiple lines in the tick_funs input for a given transition, we take the product
      # of these probabilities. Currently, this applies to the questing to feeding transitions, which are 
      # the product of P(active questing) and P(host finding). Not sure if we want to implement something
      # similar for delay transition 
      trans_matrix[from, to] <- ifelse((trans_matrix[from, to] == 0), 1, trans_matrix[from, to]) *
        get_transition_val2(time, transitions[t, ], N) * transitions[t, ]$fecundity
      # pretty printing of trans_matrix
      # print(ifelse(trans_matrix == 0, ".", trans_matrix %>% as.character() %>% substr(0, 4)), quote = FALSE)
    }
  }
  
  if (nrow(mort) > 0) {
    for (m in seq_len(nrow(mort))) {
      from_val <- mort[m,]$from
      # TODO temporary fix: fecundity is a multi-element vector if there are multiple (non-mort) 
      # transitions for the same "from". I think these should be all have the same fecundity, 
      # for now we'll just assume that is true and use the first one
      fecundity <- transitions %>% filter(from == from_val, to != 'm') %>% pull(fecundity) %>% .[1]
      trans_matrix[from_val, from_val] <- fecundity - sum(trans_matrix[from_val,]) - get_transition_val2(time, mort[m,], N)
    }
  }
  
  return(trans_matrix)
}

# based on the current time, delay_mat and N (population matrix), return a delay_mat for the next time step
update_delay_mat <- function(time, delay_mat, N) {
  
  transitions <- tick_funs %>% filter(delay == 1,
                                      is.na(todo), 
                                      from %in% life_stages,
                                      to %in% life_stages)
                                      
  
  for (t in seq_len(nrow(transitions))) {
    from_val <- transitions[t,]$from
    to <- transitions[t,]$to
    
    pred1 <- get_pred(time, transitions[t,]$pred1, transitions[t,]$delay, N)
    pred2 <- get_pred(time, transitions[t,]$pred2, transitions[t,]$delay, N)
    
    # calculate transition
    val <- get_transition_val2(time, transitions[t,], N)
    
    # constant function returns a single value, 
    # we need a vector with many entries for the cumsum
    if (length(val) == 1) {
      # we add 1 for consistency with output vector length from non-constant fxns
      # which is determined by time:(time + max_delay) in get_pred()
      val <- rep(val, max_delay + 1) 
    } 
    
    days <- cumsum(val) > 1
    
    if (TRUE %in% days) {
      days_to_next <- min(which(days))
      # I think this is a fix if mortality is not constant
      if (length(pred1))
      {
        daily_survival <- 1 - get_transition_val(from_val, 'm', pred1[1:days_to_next], pred2[1:days_to_next])
        surv_to_next <- prod(daily_survival)
      } else {  
        surv_to_next <- (1 - get_transition_val2(time, filter(tick_funs, from == from_val, to == 'm'), N)) ^ days_to_next
      }
      delay_mat[to, time + days_to_next] <- delay_mat[to, time + days_to_next] + 
        N[from_val, time] * surv_to_next * transitions[t, 'fecundity'][[1]]
    }
  }
  return(delay_mat)
}

# 05 iteratively run model for steps iterations, starting with initial_population
run <- function(steps, initial_population) {
  
  # initialize a delay matrix of all zeros
  delay_mat <- matrix(nrow = length(life_stages), ncol = steps + max_delay, data = 0)
  rownames(delay_mat) <- life_stages
  
  # host community array, hard coding three tick life stages
  # this keeps track of the number of ticks of each life stage on the average host of each host spp on each day
  hc_array <- array(dim = c(3, n_host_spp, steps + max_delay), 
                    data = 0, 
                    dimnames = list(c('l','n','a'), host_comm %>% pull(host_spp) %>% unique(), NULL)) 
  
  # intialize a population matrix with initial_population
  N <- matrix(nrow = length(life_stages), ncol = steps, data = 0)
  N[,1] <- initial_population 
  rownames(N) <- life_stages
  
  # Initialize a population matrix to keep track of the number of individuals of each stage
  # that are currently developing (currently undergoing a delay)
  N_developing <- matrix(nrow = length(life_stages), ncol = steps, data = 0)
  
  # at each time step:
  # (1) generate a new trans_matrix based on conditions at "time"
  # (2) update the delay_mat based on conditions at "time" 
  # (3) update the population matrix "N" for "time + 1"
  
  # at each time step
  for (time in 1:(steps - 1)) {
    
    if (time %% 100 == 0) print(time)
    
    # calculate transition probabilities
    trans_matrix <- gen_trans_matrix(time, N)
    
    # calculate the number of ticks entering delayed development
    delay_mat <- update_delay_mat(time, delay_mat, N)
    
    # calculate the number of ticks currently in delayed development
    # TODO wrong, this currently records the stage that the ticks are transitioning TO
    N_developing[, time] <- rowSums(delay_mat[, time:steps, drop=FALSE])
    
    # calculate the number of ticks at the next time step, which is 
    # current population * transition probabilities + ticks emerging from delayed development
    N[, time + 1] <- N[, time] %*% trans_matrix + delay_mat[, time + 1]
  }
  
  # the population matrix N and delay_mat are local variables
  # we return them here after running "steps" times
  return(list(N, N_developing, delay_mat))
}

# run the model and extract the output population matrix and delay_matrix
out <- run(steps, initial_population)
out_N <- out[[1]]
out_N_developing <- out[[2]]
out_delay_mat <- out[[3]]

# out_N <- out_N + out_N_developing

# inspect the outputs
# out_N[,1:50]
# out_delay_mat[,1:50]

# convert output population matrix to a friendly format for graphing
out_N_df <- out_N %>% t() %>% as.data.frame() %>% mutate(day = row_number()) %>% 
  pivot_longer(-c(day), names_to = 'stage', values_to = 'pop') %>%
  mutate(age_group = substr(stage, str_length(stage), str_length(stage)),
         sub_stage = substr(stage, 0, str_length(stage) - 1),
         process = substr(stage, 0, 1),
         infected = grepl("i", stage, fixed = TRUE))
  #filter(stage %in% c('ql', 'fl', 'hl'))

# graph population over time
ggplot(out_N_df, aes(x = day, y = pop, color = process, shape = age_group, group = stage)) + 
  geom_point(aes(size = infected)) + # , position = 'jitter') + 
  scale_size_manual(values = c(1, 3)) + 
  scale_shape_manual(values = c(15:18)) + 
  geom_line() + 
  scale_y_log10(limits = c(-1, 1e+05), breaks = c(10, 100, 1000, 10000, 100000, 1000000)) + 
  geom_hline(yintercept = 1000)

