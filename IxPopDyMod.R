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
rownames(N) <- life_stages
initial_pop <- runif(length(life_stages), min = 1000, max = 1000) %>% as.integer()
N[,1] <- initial_pop

# 01 functions to grab the parameters that determine the transition matrix at a given time
get_temp <- function(time, weather) {
  
  temp <- weather %>%
    filter(j_day == time) %>%
    select(tmean) %>% 
    as.numeric() # tbl to vector
  
  # return(runif(1, min = 30, max = 70)) # placeholder
  return(temp)
}

v_temp <- Vectorize(function(x) get_temp(x,weather))

get_vpd <- function(time, weather) {
  # DA comment: yeah all the other papers use RH, but I think the VPD is the more
  # biologically relavent value, but if all the parameterization is iwth RH, we might
  # have to go back to RH. Eitherway if you know the temp you can convert between RH and VPD
  
  vpd <- weather %>% 
    filter(j_day == time) %>%
    select(vpdmean) %>% 
    as.numeric()
  
  return(vpd)
} 

# The weather parameters for generating the transition matrix are single vals (return values of get_temp, get_vpd).
# What host community data do we need and how will host densities be stored (format/data structure)?  
# Currently, these functions return vectors of length n_host_spp
# Parameters from research_strat p.4: 
#   For the ith host species and jth tick life stage we have parameters defining the feeding preference, 
#   pj,i, tick feeding success rate, sj,i , and reservoir competence, ri
# Some of these parameters are based on tick and host sp., (pj,i and sj,i), does that mean that the host
# community input (csv) would have some tick data as well? Then the tick_parameters input csv could 
# just have parameters about weather impacts on transition probability
get_host_densities <- function(time, host_community) {return(runif(n_host_spp))} 
get_host_preferences <- function(tick_parameters) {return(runif(n_host_spp))}
get_host_reservoir_competences <- function(host_community) {return(runif(n_host_spp))} # TODO based on tick params or host community?

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

gen_trans_matrix <- function(time, life_stages) {
  
  # get the parameters 
  temp = get_temp(time, weather)
  vpd = get_vpd(time, weather)
  host_densities = get_host_densities(time)
  
  # initialize the transition matrix with zeros
  n_life_stages <- length(life_stages)
  trans_matrix <- matrix(0, ncol = n_life_stages, nrow = n_life_stages, 
                         dimnames = list(life_stages, life_stages))
  
  # calculate the transition probabilities for possible transitions
  trans_matrix['e', 'hl'] <- get_transition_fun("egg_larva", pred1 = temp)
  trans_matrix['e', 'e'] <- 1 - trans_matrix['e', 'hl'] - get_transition_fun('egg_mort')
  
  trans_matrix['hl', 'ql'] <- get_transition_fun('larva_harden')
  trans_matrix['hl', 'hl'] <- 1 - trans_matrix['hl', 'ql'] - get_transition_fun('larva_mort')
  
   trans_matrix['ql', 'fl'] <- get_transition_fun('larva_quest', pred1 = temp) * get_transition_fun('host_finding', pred1 = sum(host_den * l_pref))
   trans_matrix['ql', 'ql'] <- 1 - trans_matrix['ql', 'fl'] - get_transition_fun('larva_mort')
  
  # haven't actually handled infected/uninfected here (depends on hosts), this just says that there's 
  # a 50/50 chance that an uninfected feeding larva or nymph is infected/uninfected when it becomes engorged
  # density dependent feeding success? Yikes, will need to track how many of each life stage on each host every day?????
  # for now ignore density dependent feeding success
  # this doesn't have time delay
  trans_matrix['fl', 'eul'] <- sum((1-host_rc) * (l_feed_success * ( (host_den * l_pref)/sum(host_den * l_pref))))
  trans_matrix['fl', 'eil'] <- sum(host_rc* (l_feed_success * ( (host_den * l_pref)/sum(host_den * l_pref))))
  trans_matrix['fl', 'fl'] <- 1 - trans_matrix['fl', 'eul'] - trans_matrix['fl', 'eil'] - get_transition_fun('larva_engorged_mort')
  
  # commented out engorged larvae to questing nymph: if we don't change the (zero) transition 
  # probabilities for these stages, multiplication gives the desired behavior of  any engorged larvae 
  # dropping out of the population matrix at the next time step (first we copy the # individuals to the delay matrix)
  # trans_matrix['eul', 'qun'] <- get_transition_fun('larva_engorged_nymph', pred1 = temp)
  # trans_matrix['eul', 'eul'] <- 1 - trans_matrix['eul', 'qun'] - get_transition_fun('larva_engorged_mort')
  # trans_matrix['eil', 'qin'] <- get_transition_fun('larva_engorged_nymph', pred1 = temp)
  # trans_matrix['eil', 'eil'] <- 1 - trans_matrix['eil', 'qin'] - get_transition_fun('larva_engorged_mort')
  
  # trans_matrix['qun', 'fun'] skipped because depends on host community
  # trans_matrix['qun', 'qun'] ...
  # trans_matrix['qin', 'fin'] ...
  # trans_matrix['qin', 'qin'] ...
  
  trans_matrix['fun', 'eun'] <- get_transition_fun('nymph_feed_engorged') / 2 # placeholder, dependent on hosts 
  trans_matrix['fun', 'ein'] <- get_transition_fun('nymph_feed_engorged') / 2
  trans_matrix['fun', 'fun'] <- 1 - trans_matrix['fun', 'eun'] - trans_matrix['fun', 'ein'] - get_transition_fun('nymph_engorged_mort')
  
  trans_matrix['fin', 'ein'] <- get_transition_fun('nymph_feed_engorged')
  trans_matrix['fin', 'fin'] <- 1 - trans_matrix['fin', 'ein'] - get_transition_fun('nymph_engorged_mort')

  # TODO: implement as delay  
  # trans_matrix['eun', 'qua'] <- get_transition_fun('nymph_engorged_adult', pred1 = temp)
  # trans_matrix['eun', 'eun'] <- 1 - trans_matrix['eun', 'qua'] - get_transition_fun('adult_quest_mort')
  # trans_matrix['ein', 'qia'] <- get_transition_fun('nymph_engorged_adult', pred1 = temp)
  # trans_matrix['ein', 'ein'] <- 1 - trans_matrix['ein', 'qia'] - get_transition_fun('adult_quest_mort')
 
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
  # steps: number of iterations
  
  # update N[, time + 1]
  for (time in 1:(steps - 1)) {

    # generate transition matrix for (time -> time + 1), which is based on 
    # conditions (weather and host community) at day "time"
    trans_matrix <- gen_trans_matrix(time, life_stages)
    
    # update the delay matrix, also based on conditions at day "time"
    temp2 <- v_temp(time:(time + max_delay))
    
    # for each transition using a delay, generate a vector of whether the cumsum is > 1 on each day
    e_hl_days <- cumsum(get_transition_fun("egg_larva", pred1 = temp2)) > 1
    eul_qun_days <- cumsum(get_transition_fun('larva_engorged_nymph', pred1 = temp2)) > 1
    eil_qin_days <- cumsum(get_transition_fun('larva_engorged_nymph', pred1 = temp2)) > 1    
    
    # for each transition, if cumsum ever gets to 1
    if (TRUE %in% e_hl_days) {
      days_to_hatch <- min(which(e_hl_days))
      surv_to_hatch <- (1-get_transition_fun('egg_mort'))^days_to_hatch
      delay_mat['hl',time+days_to_hatch] <- N['e',time]*surv_to_hatch + delay_mat['hl',time+days_to_hatch]
    }
    
    if (TRUE %in% eul_qun_days) {
      days_to_quest <- min(which(eul_qun_days))
      surv_to_quest <- (1-get_transition_fun('larva_engorged_mort'))^days_to_quest
      delay_mat['qun', time+days_to_quest] <- N['eul', time]*surv_to_hatch + delay_mat['qun', time+days_to_quest]
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

days_to_hatch <- min(which(cumsum(get_transition_fun("egg_larva", pred1 = temp2)) > 1))
surv_to_hatch <- (1-get_transition_fun('egg_mort'))^days_to_hatch

delay_mat['hl',time+days_to_hatch] <- N['e',time]*surv_to_hatch + delay_mat['hl',time+days_to_hatch]

N[,time+1] <- delay_mat[,time+1]


out <- run(steps=10)

