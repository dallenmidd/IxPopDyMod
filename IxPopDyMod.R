## Ixodes population dynamics model
## Dave Allen and Myles Stokowski

library(tidyverse)

# 00
# read inputs 
# host_community <- read_csv('inputs/host_community.csv')
weather <- read_csv('inputs/weather.csv')
# constant temperature for testing
weather <- tibble(tmean = seq(from = 20, to = 20, length.out = 1000), j_day = seq(from = 1, to = 1000))

# option to run on simple inputs 
simple <- FALSE

if (simple) {
  tick_params <- read_csv('inputs/tick_parameters_simple_stable_delay2.csv')
  tick_funs <- read_csv('inputs/tick_functions_simple_delay.csv')
  life_stages <- read_csv('inputs/tick_stages_simple.csv')[[1]]
} else {
  tick_params <- read_csv('inputs/tick_parameters.csv')
  tick_funs <- read_csv('inputs/tick_functions.csv')
  life_stages <- read_csv('inputs/tick_stages.csv')[[1]]
}

initial_population <- runif(length(life_stages), 0, 0)
initial_population[length(initial_population)] <- 10 # start with only one cohort (adults)


# hard code in some values as placeholders until we have nicely formatted inputs
n_host_spp <- 3 # mouse, squirrel, deer
host_den <- c(40, 8, 0.25)
l_pref <- c(1, 0.75, 0.25)
n_pref <- c(1, 1, 0.25)
a_pref <- c(0, 0, 1)
l_feed_success <- c(0.49, 0.17, 0.49)
n_feed_success <- c(0.49, 0.17, 0.49)
a_feed_success <- c(0, 0, 0.49)
host_rc <- c(0.92, 0.147, 0.046)

max_delay <- 300


# 01 functions to grab the parameters that determine the transition matrix at a given time
get_temp <- function(time) {
  weather %>%
    filter(j_day %in% time) %>%
    pull(tmean)
}

get_vpd <- function(time) {
  weather %>% 
    filter(j_day %in% time) %>%
    pull(vpdmean)
} 

# 02 functional forms for transition probabilities

expo_fun <- function(x, y, p) ifelse(x>0,p['a']*x^p['b'],0)
briere_fun <- function(x, y, p) ifelse(x>p['tmin'] & x<p['tmax'],p['q']*x*(x-p['tmin'])*sqrt(p['tmax']-x),0) # https://doi.org/10.7554/eLife.58511
constant_fun <- function(x, y, p) p['a'] # because there is no pred (x or y) input in this function, it always outputs a scalar
                                         # which is problematic when we want to do a cumsum
binomial_fun <- function(x, y, p) 1-(1-p['a'])^x

# 03
# functions that calculate individual transition probabilities for advancing to consecutive life stage
# this generic function will pull out the functional form and parameters need to make the transition function
get_transition_fun <- function(which_from, which_to, pred1 = NULL, pred2 = NULL, functions = tick_funs, parameters = tick_params) {
  
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
  
  f(x = pred1, y = pred2, p =  params) %>% unname()
}

get_transition_fun2 <- function(time, transition_row, parameters = tick_params) {
  # alternative approach that takes an entire transition row, which is a workaround for 
  # having multiple rows with the same 'from' and 'to'
  # transition_row: a row from the transitions tibble
  
  f <- transition_row[['transition_fun']] %>% get()
  
  params <- parameters %>%
    filter(from == transition_row[['from']], to == transition_row[['to']]) %>%
    pull(param_value)
  
  names(params) <- parameters %>%
    filter(from == transition_row[['from']], to == transition_row[['to']]) %>%
    pull(param_name)
  
  f(x = get_pred(time, transition_row[['pred1']], transition_row[['delay']]), 
    y = get_pred(time, transition_row[['pred2']], transition_row[['delay']]),
    p =  params) %>% unname()
}

# Dave: We will get these transition probs from other studies
# Ogden et al 2004: https://doi.org/10.1603/0022-2585-41.4.622
# Ogden et al. 2005: https://doi.org/10.1016/j.ijpara.2004.12.013
# Dobson et al. 2011: https://doi.org/10.1111/j.1365-2664.2011.02003.x
# Wallace et al 2019: https://doi.org/10.1155/2019/9817930
# Randolph 1999 effect of Sat Def on questing, see FIg 3: http://doi.org/10.1093/jmedent/36.6.741

# 04
# at each step, we generate a new transition matrix whose transition probabilities
# are based on the input data (weather, host_community) at that time 
# Myles note: I think all the transitions we're handling this way are questing -> feeding
gen_trans_matrix <- function(time) {
  
  # get the parameters 
  #temp = get_temp(time)
  #vpd = get_vpd(time)
  
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
      pred1 <- get_pred(time, transitions[t,]$pred1, transitions[t,]$delay)
      pred2 <- get_pred(time, transitions[t,]$pred2, transitions[t,]$delay)
      # If there are multiple lines in the tick_funs input for a given transition, we take the product
      # of these probabilities. Currently, this applies to the questing to feeding transitions, which are 
      # the product of P(active questing) and P(host finding). Not sure if we want to implement something
      # similar for delay transition 
      trans_matrix[from, to] <- ifelse((trans_matrix[from, to] == 0), 1, trans_matrix[from, to]) * get_transition_fun2(time, transition_row = transitions[t,]) * transitions[t,]$fecundity
    }
  }
  
  # just turning this off for now for the simple run
  if (!simple)
  {
    # TODO!!! these should be implemented as time delay
    # density dependent feeding success? Yikes, will need to track how many of each life stage on each host every day?????
    # for now ignore density dependent feeding success
    # Myles note: not sure how to implement time delay here. What's different here is that one class goes to multiple, 
    # and the transition probability is not only describing the chance of advancing to next stage, but also the 
    # relative number of feeding ticks that become either infected or uninfected.
    # Idea: maybe we have to split up the processes of becoming infected (interpret this is a probability) and 
    # becoming engorged (interpret this as a time delay)
    trans_matrix['fl', 'eul'] <- sum((1-host_rc) * (l_feed_success * ( (host_den * l_pref)/sum(host_den * l_pref))))
    trans_matrix['fl', 'eil'] <- sum(host_rc* (l_feed_success * ( (host_den * l_pref)/sum(host_den * l_pref))))
    trans_matrix['fl', 'fl'] <- 1 - trans_matrix['fl', 'eul'] - trans_matrix['fl', 'eil'] - get_transition_fun('fl', 'm')
    
    trans_matrix['fun', 'eun'] <- sum((1-host_rc) * (n_feed_success * ( (host_den * n_pref)/sum(host_den * n_pref))))
    trans_matrix['fun', 'ein'] <- sum(host_rc* (n_feed_success * ( (host_den * n_pref)/sum(host_den * n_pref))))
    trans_matrix['fun', 'fun'] <- 1 - trans_matrix['fun', 'eun'] - trans_matrix['fun', 'ein'] - get_transition_fun('fun', 'm')
    trans_matrix['fin', 'ein'] <- sum(n_feed_success * ((host_den * n_pref)/sum(host_den * n_pref)))
    trans_matrix['fin', 'fin'] <- 1 - trans_matrix['fin', 'ein'] - get_transition_fun('fin', 'm')
    
    # TODO should we do just reproductive adults, or split into eua/eia?
    # trans_matrix['fua', 'eua'] <- sum((1-host_rc) * (a_feed_success * ( (host_den * a_pref)/sum(host_den * a_pref))))
    # trans_matrix['fua', 'eia'] <- sum(host_rc* (a_feed_success * ( (host_den * a_pref)/sum(host_den * a_pref))))
    # trans_matrix['fua', 'fua'] <- 1 - trans_matrix['fua', 'eua'] - trans_matrix['fua', 'eia'] - get_transition_fun('fua', 'm')
    # trans_matrix['fia', 'eia'] <- sum(a_feed_success * ((host_den * a_pref)/sum(host_den * a_pref)))
    # trans_matrix['fia', 'fia'] <- 1 - trans_matrix['fia', 'eia'] - get_transition_fun('fia', 'm')
  }
  
  if (nrow(mort) > 0) {
    for (m in seq_len(nrow(mort))) {
      from_val <- mort[m,]$from
      # TODO temporary fix: fecundity is a multi-element vector if there are multiple (non-mort) 
      # transitions for the same "from". I think these should be all have the same fecundity, 
      # for now we'll just assume that is true and use the first one
      fecundity <- transitions %>% filter(from == from_val, to != 'm') %>% pull(fecundity) %>% .[1]
      trans_matrix[from_val, from_val] <- fecundity - sum(trans_matrix[from_val,]) - get_transition_fun(from_val, 'm', pred1, pred2)
    }
  }
  
  return(trans_matrix)
}

# based on the current time, delay_mat and N, return a delay_mat for the next time step
update_delay_mat <- function(time, delay_mat, N) {
  
  transitions <- tick_funs %>% filter(delay == 1,
                                      is.na(todo), 
                                      from %in% life_stages,
                                      to %in% life_stages)
                                      
  
  for (t in seq_len(nrow(transitions))) {
    from <- transitions[t,]$from
    to <- transitions[t,]$to
    
    pred1 <- get_pred(time, transitions[t,]$pred1, transitions[t,]$delay)
    pred2 <- get_pred(time, transitions[t,]$pred2, transitions[t,]$delay)
    
    # calculate transition
    fun <- get_transition_fun(from, to, pred1, pred2)
    
    # constant functions return a single value, 
    # we need a vector with many entries for the cumsum
    if (length(fun) == 1) {
      fun <- seq(from=fun, to=fun, length.out = max_delay) # I think this is still 1 shorter than time:(time+max_delay), may not matter
    }
    
    days <- cumsum(fun) > 1
    
    if (TRUE %in% days) {
      days_to_next <- min(which(days))
      surv_to_next <- (1 - get_transition_fun(from, 'm', pred1, pred2)) ^ days_to_next
      delay_mat[to, time + days_to_next] <- delay_mat[to, time + days_to_next] + 
        N[from, time] * surv_to_next * transitions[t, 'fecundity'][[1]]
    }
  }
  return(delay_mat)
}

# return a vector of length max_delay of a predictor
get_pred <- function(time, pred, is_delay) {
  
  # if delay, we want a long vector so the cumsum will reach 1
  # otherwise, we want a vector of length 1
  if (is_delay) {time <- time:(time + max_delay)}
  
  if (is.na(pred)) {
    return(NULL)
  } else if (pred == "temp") {
    return(get_temp(time))
  } else if (pred == "sum(host_den * l_pref)") {
    return(sum(host_den * l_pref))
  } else if (pred == "sum(host_den * n_pref)") {
    return(sum(host_den * n_pref)) 
  } else if (pred == "sum(host_den * a_pref)") {
    return(sum(host_den * a_pref))
  } else {
    print("error: couldn't match pred")
  }
}

# 05 iteratively run model 
run <- function(steps, initial_population) {
  
  # initialize a delay matrix of all zeros
  delay_mat <- matrix(nrow = length(life_stages), ncol = steps + max_delay, data = 0)
  rownames(delay_mat) <- life_stages
  
  # intialize a population matrix with initial_population
  N <- matrix(nrow = length(life_stages), ncol = steps, data = 0)
  N[,1] <- initial_population 
  rownames(N) <- life_stages
  
  # at each time step:
  # (1) generate a new trans_matrix based on conditions at "time"
  # (2) update the delay_mat based on conditions at "time" 
  # (3) update the population matrix "N" for "time + 1"
  for (time in 1:(steps - 1)) {
    trans_matrix <- gen_trans_matrix(time)
    delay_mat <- update_delay_mat(time, delay_mat, N)
    N[, time + 1] <- N[, time] %*% trans_matrix + delay_mat[, time + 1]
  }
  
  # the population matrix N and delay_mat are local variables
  # we return them here after running "steps" times
  return(list(N, delay_mat))
}

# run the model and extract the output population matrix and delay_matrix
out <- run(steps=300, initial_population)
out_N <- out[[1]]
out_delay_mat <- out[[2]]

# inspect the outputs
# out_N[,1:50]
# out_delay_mat[,1:50]

# convert output population matrix to a friendly format for graphing
out_N_df <- out_N %>% t() %>% as.data.frame() %>% mutate(day = row_number()) %>% 
  pivot_longer(-c(day), names_to = 'stage', values_to = 'pop') %>%
  mutate(age_group = substr(stage, str_length(stage), str_length(stage)),
         sub_stage = substr(stage, 0, str_length(stage) - 1)) 
  #filter(stage %in% c('ql', 'fl', 'hl'))

# graph population over time
ggplot(out_N_df, aes(x = day, y = pop, color = sub_stage, shape = age_group)) + 
  geom_point(size = 2, position = 'jitter') + 
  geom_line() + 
  #ylim(0,3000) + 
  xlim(0, 300) + 
  scale_y_log10() + 
  geom_hline(yintercept = 1000)







