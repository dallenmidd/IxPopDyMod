## Ixodes population dynamics model
## Dave Allen and Myles Stokowski

require(tidyverse)

# 00
# read inputs
# host_community <- read_csv('inputs/host_community.csv')
weather <- read_csv('inputs/weather.csv')
tick_params <- read_csv('inputs/tick_parameters.csv')
tick_funs <- read_csv('inputs/tick_functions.csv')

# hard code in some values as placeholders until we have nicely formatted inputs
n_host_spp = 4
life_stages = c('e', 'hl', 'ql', 'fl', 'eul', 'eil',                        # larvae
                'qun', 'qin', 'fun', 'fin', 'eun', 'ein',                   # nymphs
                'qua', 'qia', 'fua', 'fia', 'ra')                           # adults

delay_mat <- matrix(nrow = length(life_stages), ncol = dim(weather)[1], data = 0)
rownames(delay_mat) <- life_stages

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

# 03
# functions that calculate individual transition probabilities for advancing to consecutive life stage
# this generic function will pull out the functional form and parameters need to make the transition function
get_transition_fun <- function(which_trans, pred1 = NULL, pred2 = NULL, functions = tick_funs, parameters = tick_params) {
  f <- functions %>%
    filter(transition == which_trans) %>%
    pull(transition_fun) %>%
    get()
  
  params <- parameters %>%
    filter(transition == transition) %>%
    pull(param_value)
  
  names(params) <- parameters %>%
    filter(transition == transition) %>%
    pull(param_name)
  
  f(x = pred1, y = pred2, p =  params) %>% unname()
}

# Dave: We will get these transition probs from other studies
# Ogden et al 2004: https://doi.org/10.1603/0022-2585-41.4.622
# Ogden et al. 2005: https://doi.org/10.1016/j.ijpara.2004.12.013
# Dobson et al. 2011: https://doi.org/10.1111/j.1365-2664.2011.02003.x
# Wallace et al 2019: https://doi.org/10.1155/2019/9817930

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
  
  # trans_matrix['ql', 'fl'] <- get_transition_fun('larva_quest', pred1 = temp) * PROB OF FINDING HOST IF QUESTING
  # trans_matrix['ql', 'ql'] <- 1 - trans_matrix['ql', 'fl'] - get_transition_fun('larva_mort')

  # haven't actually handled infected/uninfected here (depends on hosts), this just says that there's 
  # a 50/50 chance that an uninfected feeding larva or nymph is infected/uninfected when it becomes engorged
  trans_matrix['fl', 'eul'] <- get_transition_fun('larva_feed_engorged') / 2 # placeholder splitting pop into half infected/uninfected
  trans_matrix['fl', 'eil'] <- get_transition_fun('larva_feed_engorged') / 2
  trans_matrix['fl', 'fl'] <- 1 - trans_matrix['fl', 'eul'] - trans_matrix['fl', 'eil'] - get_transition_fun('larva_engorged_mort')
  
  trans_matrix['eul', 'qun'] <- get_transition_fun('larva_engorged_nymph', pred1 = temp)
  trans_matrix['eul', 'eul'] <- 1 - trans_matrix['eul', 'qun'] - get_transition_fun('larva_engorged_mort')
  trans_matrix['eil', 'qin'] <- get_transition_fun('larva_engorged_nymph', pred1 = temp)
  trans_matrix['eil', 'eil'] <- 1 - trans_matrix['eil', 'qin'] - get_transition_fun('larva_engorged_mort')

  # trans_matrix['qun', 'fun'] skipped because depends on host community
  # trans_matrix['qun', 'qun'] ...
  # trans_matrix['qin', 'fin'] ...
  # trans_matrix['qin', 'qin'] ...
  
  trans_matrix['fun', 'eun'] <- get_transition_fun('nymph_feed_engorged') / 2 # placeholder, dependent on hosts 
  trans_matrix['fun', 'ein'] <- get_transition_fun('nymph_feed_engorged') / 2
  trans_matrix['fun', 'fun'] <- 1 - trans_matrix['fun', 'eun'] - trans_matrix['fun', 'ein'] - get_transition_fun('nymph_engorged_mort')
  
  trans_matrix['fin', 'ein'] <- get_transition_fun('nymph_feed_engorged')
  trans_matrix['fin', 'fin'] <- 1 - trans_matrix['fin', 'ein'] - get_transition_fun('nymph_engorged_mort')
  
  trans_matrix['eun', 'qua'] <- get_transition_fun('nymph_engorged_adult', pred1 = temp)
  trans_matrix['eun', 'eun'] <- 1 - trans_matrix['eun', 'qua'] - get_transition_fun('adult_quest_mort')
  
  trans_matrix['ein', 'qia'] <- get_transition_fun('nymph_engorged_adult', pred1 = temp)
  trans_matrix['ein', 'ein'] <- 1 - trans_matrix['ein', 'qia'] - get_transition_fun('adult_quest_mort')
  
  # trans_matrix['qua', 'fua'] skipped because depends on hosts
  # trans_matrix['qua', 'qua'] ...
  # trans_matrix['qia', 'fia'] ...
  # trans_matrix['qia', 'qia'] ...
  
  # trans_matrix['fua', 'ra'] skipped becuase depends on hosts
  # trans_matrix['fia', 'ra'] ...
  
  return(trans_matrix)
} 

# 05 iteratively run model

run <- function(steps, life_stages, pop) {
  # steps: number of iterations
  # life_stages: vector of life stage names (there's likely a cleaner way to pass this)
  # pop: initial population vector
  
  # at each step, generate the trans_matrix for that time
  # then update the population vector as product of pop and trans_matrix
  for (time in 1:steps) {
    trans_matrix <- gen_trans_matrix(time, life_stages)
    pop <- pop %*% trans_matrix %>% c()
    names(pop) <- life_stages
  }
  
  return(pop)
}

initial_pop <- runif(length(life_stages), min = 0, max = 100) %>% as.integer()
run(250, life_stages, initial_pop)


### 06 thoughts on delay_mat
time <- 100
temp2 <- v_temp(time:(time + 300))

days_to_hatch <- min(which(cumsum(get_transition_fun("egg_larva", pred1 = temp2)) > 1))
surv_to_hatch <- (1-get_transition_fun('egg_mort'))^days_to_hatch

delay_mat['hl',time+days_to_hatch] <- N['e',time]*surv_to_hatch + delay_mat['hl',time+days_to_hatch]

N[,time+1] <- delay_mat[,time+1]



