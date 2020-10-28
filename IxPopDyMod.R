## Ixodes population dynamics model
## Dave Allen and Myles Stokowski

library(tidyverse)

# 00
# read inputs 
# host_community <- read_csv('inputs/host_community.csv')
weather <- read_csv('inputs/weather.csv')
tick_params <- read_csv('inputs/tick_parameters.csv')
tick_funs <- read_csv('inputs/tick_functions.csv')
life_stages <- read_csv('inputs/tick_stages.csv')[[1]]

# hard code in some values as placeholders until we have nicely formatted inputs
n_host_spp <- 3 # mouse, squirrel, deer
host_den <- c(40, 8, 0.25)
l_pref <- c(1, 0.75, 0.25)
n_pref <- c(1, 1, 0.25)
a_pref <- c(0, 0, 1)
l_feed_success <- c(0.49, 0.17, 0.49)
host_rc <- c(0.92, 0.147, 0.046)

initial_population <- runif(length(life_stages), 1000, 1000)

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
constant_fun <- function(x, y, p) p['a']
binomial_fun <- function(x, y, p) 1-(1-p['a'])^x

# 03
# functions that calculate individual transition probabilities for advancing to consecutive life stage
# this generic function will pull out the functional form and parameters need to make the transition function
get_transition_fun <- function(which_trans, pred1 = NULL, pred2 = NULL, functions = tick_funs, parameters = tick_params) {
  f <- functions %>%
    filter(transition == which_trans) %>%
    pull(transition_fun) %>%
    get()
  
  params <- parameters %>%
    filter(transition == which_trans) %>%
    pull(param_value)
  
  names(params) <- parameters %>%
    filter(transition == which_trans) %>%
    pull(param_name)
  
  f(x = pred1, y = pred2, p =  params) %>% unname()
}

get_transition_fun2 <- function(which_from, which_to, pred1 = NULL, pred2 = NULL, functions = tick_funs, parameters = tick_params) {
  f <- functions %>%
    filter(from == which_from, to == which_to) %>%
    pull(transition_fun) %>%
    get()
  
  params <- parameters %>%
    filter(from == which_from, to == which_to) %>%
    pull(param_value)
  
  names(params) <- parameters %>%
    filter(from == which_from, to == which_to) %>%
    pull(param_name)
  
  f(x = pred1, y = pred2, p =  params) %>% unname()
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

gen_trans_matrix <- function(time) {
  
  # get the parameters 
  temp = get_temp(time)
  vpd = get_vpd(time)

  # initialize the transition matrix with zeros
  n_life_stages <- length(life_stages)
  trans_matrix <- matrix(0, ncol = n_life_stages, nrow = n_life_stages, 
                         dimnames = list(life_stages, life_stages))
  
  # calculate the transition probabilities for possible transitions
  trans_matrix['ql', 'fl'] <- get_transition_fun('ql_fl', pred1 = temp) * get_transition_fun('q_f', pred1 = sum(host_den * l_pref))
  trans_matrix['ql', 'ql'] <- 1 - trans_matrix['ql', 'fl'] - get_transition_fun('l_m')
  
  # density dependent feeding success? Yikes, will need to track how many of each life stage on each host every day?????
  # for now ignore density dependent feeding success
  # TODO!!! this doesn't have time delay 
  # Myles note: not sure how to implement time delay here. What's different here is that one class goes to multiple, 
  # and the transition probability is not only describing the chance of advancing to next stage, but also the 
  # relative number of feeding ticks that become either infected or uninfected.
  # Idea: maybe we have to split up the processes of becoming infected (interpret this is a probability) and 
  # becoming engorged (interpret this as a time delay)
  trans_matrix['fl', 'eul'] <- sum((1-host_rc) * (l_feed_success * ( (host_den * l_pref)/sum(host_den * l_pref))))
  trans_matrix['fl', 'eil'] <- sum(host_rc* (l_feed_success * ( (host_den * l_pref)/sum(host_den * l_pref))))
  trans_matrix['fl', 'fl'] <- 1 - trans_matrix['fl', 'eul'] - trans_matrix['fl', 'eil'] - get_transition_fun('el_m')
  
  # trans_matrix['qun', 'fun'] skipped because depends on host community
  # trans_matrix['qun', 'qun'] ...
  # trans_matrix['qin', 'fin'] ...
  # trans_matrix['qin', 'qin'] ...
  
  # trans_matrix['qua', 'fua'] skipped because depends on hosts
  # trans_matrix['qua', 'qua'] ...
  # trans_matrix['qia', 'fia'] ...
  # trans_matrix['qia', 'qia'] ...
  
  return(trans_matrix)
} 

# Myles note: I think all the transitions we're handling this way are questing -> feeding
gen_trans_matrix2 <- function(time) {
  
  # get the parameters 
  temp = get_temp(time)
  vpd = get_vpd(time)
  
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
  
  # TODO hardcoded 
  pred1 <- get_temp(time)
  pred2 <- NULL
  
  if (nrow(transitions) > 0) {
    for (t in seq_along(nrow(transitions))) {
      from <- transitions[t,]$from
      to <- transitions[t,]$to
      # print(str_c("delay: ", to, " ", from))
      print(from)
      print(to)
      trans_matrix[from, to] <- get_transition_fun2(from, to, pred1, pred2)
    }
  }
  
  if (nrow(mort) > 0 ) {
    for (m in seq_along(nrow(mort))) {
      from <- mort[m,]$from
      trans_matrix[from, from] <- 1 - sum(trans_matrix[from,]) - get_transition_fun2(from, 'm', pred1, pred2)
    }
  }
  
  return(trans_matrix)
}

# based on the current time, delay_mat and N, return a delay_mat for the next time step
update_delay_mat2 <- function(time, delay_mat, N) {
  
  transitions <- tick_funs %>% filter(delay == 1,
                                      is.na(todo), 
                                      from %in% life_stages)
  
  for (t in seq_along(nrow(transitions))) {
    from <- transitions[t,]$from
    to <- transitions[t,]$to

    pred1 <- get_temp(time:(time + max_delay))
    pred2 <- NULL
    
    days <- cumsum(get_transition_fun2(from, to, pred1, pred2 )) > 1
    if (TRUE %in% days) {
      days_to_next <- min(which(days))
      surv_to_next <- (1 - get_transition_fun2(from, 'm', pred1, pred2)) ^ days_to_next
      delay_mat[to, time + days_to_next] <- delay_mat[to, time + days_to_next] + 
        N[from, time] * surv_to_next * transitions[t, 'fecundity'][[1]]
    }
  }
  return(delay_mat)
}

# 05 iteratively run model  

run <- function(steps, initial_population) {
  
  # initialize a delay matrix of all zeros
  max_delay <- 300
  delay_mat <- matrix(nrow = length(life_stages), ncol = dim(weather)[1] + max_delay, data = 0)
  rownames(delay_mat) <- life_stages
  
  # intialize a population matrix with initial_population
  N <- matrix(nrow = length(life_stages), ncol = dim(weather)[1], data = 0)
  N[,1] <- initial_population 
  rownames(N) <- life_stages
  
  # at each time step:
  # (1) generate a new trans_matrix based on conditions at "time"
  # (2) update the delay_mat based on conditions at "time" 
  # (3) update the population matrix "N" for "time + 1"
  for (time in 1:(steps - 1)) {
    trans_matrix <- gen_trans_matrix2(time)
    delay_mat <- update_delay_mat2(time, delay_mat, N)
    N[, time + 1] <- N[, time] %*% trans_matrix + delay_mat[, time + 1]
  }
  
  # the population matrix N and delay_mat are local variables
  # we return them here after running "steps" times
  return(list(N, delay_mat))
}

# run the model and extract the output population matrix and delay_matrix
out <- run(steps=10, initial_population)
out_N <- out[[1]]
out_delay_mat <- out[[2]]

