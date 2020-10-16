## Ixodes population dynamics model
## Dave Allen and Myles Stokowski

require(tidyverse)

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

# initialize a delay matrix with a row for each of the tick life_stages and
# a column for each day we have weather data
max_delay = 300
delay_mat <- matrix(nrow = length(life_stages), ncol = dim(weather)[1] + max_delay, data = 0)
rownames(delay_mat) <- life_stages

# also initialize a population matrix N
N <- matrix(nrow = length(life_stages), ncol = dim(weather)[1], data = 0)
N[,1] <- runif(length(life_stages), min = 1000, max = 1000)
rownames(N) <- life_stages

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
  
  trans_matrix['hl', 'ql'] <- get_transition_fun('hl_ql')
  trans_matrix['hl', 'hl'] <- 1 - trans_matrix['hl', 'ql'] - get_transition_fun('l_m')
  
  trans_matrix['ql', 'fl'] <- get_transition_fun('ql_fl', pred1 = temp) * get_transition_fun('q_f', pred1 = sum(host_den * l_pref))
  trans_matrix['ql', 'ql'] <- 1 - trans_matrix['ql', 'fl'] - get_transition_fun('l_m')
  
  # density dependent feeding success? Yikes, will need to track how many of each life stage on each host every day?????
  # for now ignore density dependent feeding success
  # this doesn't have time delay
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
  
  trans_matrix['fun', 'eun'] <- get_transition_fun('fn_en') / 2 # placeholder, dependent on hosts 
  trans_matrix['fun', 'ein'] <- get_transition_fun('fn_en') / 2
  trans_matrix['fun', 'fun'] <- 1 - trans_matrix['fun', 'eun'] - trans_matrix['fun', 'ein'] - get_transition_fun('en_m')
  
  trans_matrix['fin', 'ein'] <- get_transition_fun('fn_en')
  trans_matrix['fin', 'fin'] <- 1 - trans_matrix['fin', 'ein'] - get_transition_fun('en_m')

  # trans_matrix['qua', 'fua'] skipped because depends on hosts
  # trans_matrix['qua', 'qua'] ...
  # trans_matrix['qia', 'fia'] ...
  # trans_matrix['qia', 'qia'] ...
  
  # trans_matrix['fua', 'ra'] skipped becuase depends on hosts
  # trans_matrix['fia', 'ra'] ...
  
  return(trans_matrix)
} 

# 05 iteratively run model  

run <- function(steps) {
  # steps: number of iterations (days) to run model
  
  # update N[, time + 1]
  for (time in 1:(steps - 1)) {

    # generate transition matrix for (time -> time + 1), which is based on 
    # conditions (weather and host community) at day "time"
    trans_matrix <- gen_trans_matrix(time)
    
    # update the delay matrix, also based on conditions at day "time"
    temp <- get_temp(time:(time + max_delay))
    
    # TODO initial thoughts on how to generalize
    # for (trans in c("e_hl", "eul_qun", "eil_qin", "eun_qua", "ein_qia", "ra_e")) { # development transitions only here
    #   days <- cumsum(get_transition_fun(trans, pred1 = temp)) > 1
    #   
    #   if (TRUE %in% days) {
    # 
    #     # TODO the issue here is that we would need some way to identify the transition to grab based on the name of the transition
    #     # options: (1) have a duplicate entry for each transition function and parameter each time it is used (e.g. duplicate for 
    #     # eul_qun and eil_qin), or (2) use some string pattern matching, e.g. to get the transition fun/params "el_qn" for the 
    #     # transitions "eul_qun" and "eil_qin"
    #     
    #     from <- str_split(trans, '_')[1]
    #     to <- str_split(trans, '_')[2]
    # 
    #     days_to_next <- min(which(days))
    #     surv_to_next <- 1 - get_transition_fun()
    # 
    #     # TODO need condition for multiplying by number of eggs
    #     delay_mat[to, time + days_to_next] <- N[from, time]*surv_to_next + delay_mat[to, time + days_to_next] 
    #   }
    # }

    # for each transition using a delay, generate a vector of whether the cumsum is > 1 on each day
    e_hl_days <- cumsum(get_transition_fun("e_hl", pred1 = temp)) > 1
    eul_qun_days <- cumsum(get_transition_fun('el_qn', pred1 = temp)) > 1
    eil_qin_days <- cumsum(get_transition_fun('el_qn', pred1 = temp)) > 1    
    eun_qua_days <- cumsum(get_transition_fun('en_qa', pred1 = temp)) > 1
    ein_qia_days <- cumsum(get_transition_fun('en_qa', pred1 = temp)) > 1
    ra_e_days <- cumsum(get_transition_fun('ra_e', pred1 = temp)) > 1
    
    # for each transition, if cumsum ever gets to 1
    if (TRUE %in% e_hl_days) {
      days_to_next <- min(which(e_hl_days))
      surv_to_next <- (1-get_transition_fun('e_m'))^days_to_next
      delay_mat['hl',time+days_to_next] <- N['e',time]*surv_to_next + delay_mat['hl',time+days_to_next]
    }
    
    if (TRUE %in% eul_qun_days) {
      days_to_next <- min(which(eul_qun_days))
      surv_to_next <- (1-get_transition_fun('el_m'))^days_to_next
      delay_mat['qun', time+days_to_next] <- N['eul', time]*surv_to_next + delay_mat['qun', time+days_to_next]
    }
    
    if (TRUE %in% eil_qin_days) {
      days_to_next <- min(which(eul_qun_days))
      surv_to_next <- (1-get_transition_fun('el_m'))^days_to_next
      delay_mat['qin', time+days_to_next] <- N['eil', time]*surv_to_next + delay_mat['qin', time+days_to_next]
    }
    
    if (TRUE %in% eun_qua_days) {
      days_to_next <- min(which(eun_qua_days))
      surv_to_next <- (1-get_transition_fun('en_m'))^days_to_next
      delay_mat['qua', time+days_to_next] <- N['eun', time]*surv_to_next + delay_mat['qua', time+days_to_next]
    }
    
    if (TRUE %in% ein_qia_days) {
      days_to_next <- min(which(ein_qia_days))
      surv_to_next <- (1-get_transition_fun('en_m'))^days_to_next
      delay_mat['qia', time+days_to_next] <- N['ein', time]*surv_to_next + delay_mat['qia', time+days_to_next]
    }
    
    # we hande egg laying similarly to other development transitions
    # the difference is that we multiply the number of surviving reproductive adults by
    # the number of eggs each adult produces
    if (TRUE %in% ra_e_days) {
      days_to_next <- min(which(ra_e_days))
      surv_to_next <- (1-get_transition_fun('ra_m'))^days_to_next
      delay_mat['e', time+days_to_next] <- N['ra', time]*surv_to_next * get_transition_fun('n_e') + delay_mat['e', time+days_to_next]
    }
    
    # now, we've used conditions at time "time" to predict future population sizes, so we can 
    # update the population size at time + 1
    # first, multiply by the transition matrix, which should have 0 transition probability for 
    # transitions that are being handled as delays, e.g. ['e', 'hl]
    # second, we add the new individuals that are emerging from a delay at time + 1
    N[,time + 1] <- N[,time] %*% trans_matrix + delay_mat[,time + 1]
    
  }
  
  rownames(N) <- life_stages
  return(N)
}

# currently run() copies the input population matrix N, might be more efficient to modify it in place
N_out <- run(dim(weather)[1])


### 06 thoughts on delay_mat
time <- 100
temp2 <- v_temp(time:(time + max_delay))

days_to_hatch <- min(which(cumsum(get_transition_fun("e_hl", pred1 = temp2)) > 1))
surv_to_hatch <- (1-get_transition_fun('e_m'))^days_to_hatch

delay_mat['hl',time+days_to_hatch] <- N['e',time]*surv_to_hatch + delay_mat['hl',time+days_to_hatch]

N[,time+1] <- delay_mat[,time+1]


out <- run(steps=10)

