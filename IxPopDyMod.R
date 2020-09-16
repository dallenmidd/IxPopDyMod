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
life_stages = c('e', 'hl', 'ql', 'fl', 'el', 'oun', 'oin', 'qun', 'qin', 'fua', 'fia', 'ra') 

# 01 functions to grab the parameters that determine the transition matrix at a given time
get_temp <- function(time, weather) {
  
  temp <- weather %>%
    filter(j_day == time) %>%
    select(tmean) %>% 
    as.numeric() # tbl to vector

  # return(runif(1, min = 30, max = 70)) # placeholder
  return(temp)
}

get_rh <- function(time, weather) {
  # DA comment: yeah all the other papers use RH, but I think the VPD is the more
  # biologically relavent value, but if all the parameterization is iwth RH, we might
  # have to go back to RH. Eitherway if you know the temp you can convert between RH and VPD
  
  rh <- weather %>% 
    filter(j_day == time) %>%
    select(vpdmean) %>% # TODO placeholder - is rh a function of this (vpdmean) or ppt (ppt water?)?
    as.numeric()
  
  # return(runif(1))
  return(rh)
} 

# The weather parameters for generating the transition matrix are single vals (return values of get_temp, get_rh).
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

expo_fun <- function(x, p) p['a']*x^p['b']
briere_fun <- function(x, p) p['q']*x*(x-p['tmin'])*sqrt(p['tmax']-x) # https://doi.org/10.7554/eLife.58511

# 03
# functions that calculate individual transition probabilities for advancing to consecutive life stage
m_egg_larvae <- function(temp, fun = tick_funs, param = tick_params) {
  f <- fun %>%
    filter(transition == 'egg_larva') %>%
    pull(transition_fun) %>%
    get()
  
  params <- param %>%
    filter(transition == 'egg_larva') %>%
    pull(param_value)
  
  names(params) <- param %>%
    filter(transition == 'egg_larva') %>%
    pull(param_name)
  
  f(x = temp, p =  params)
  
}


m_larvae_hardening_larvae <- function(temp, rh) {return(runif(1))}
m_hardening_larvae_questing_larvae <- function(host_densities) {return(runif(1))}

# probabilities of remaining in a given life stage
m_egg_egg <- function() {return(runif(1))}
# TODO not sure where to find how the inputs influence the growth (vs mortality) 
# rates. Some functions are defined in research_strat.pdf for transition probabilities
# to consecutive life stage, but can't find for remaining in same stage. Any chance these
# are in the presentation that Dave screenshared briefly?

# TODO this function naming scheme is quickly getting unweildy... need more concise alternative
# could do acronyms like this:
#
# e_hl
# hl_ql
# ql_fl
# fl_el
# el_oun
# el_oin
# oun_qun
# oin_qin
# qun_fua
# qun_fia
# qin_fia
# fua_ra
# fia_ra
# ra_e
# (and also fxns for remaining in life stage, e.g. e_e)
# 
# OR, could avoid defining functions for each transition probability and instead do it all within 
# the matrix generating function. I think having separate functions could come in handy though, e.g. for
# fitting individual relationships between params (temp, rh, host community) and transition probabilities

# 04
# at each step, we generate a new transition matrix whose transition probabilities
# are based on the input data (weather, host_community) at that time 

gen_trans_matrix <- function(time, life_stages) {
  
  # get the parameters 
  temp = get_temp(time, weather)
  rh = get_rh(time, weather)
  host_densities = get_host_densities(time)

  # initialize the transition matrix with zeros
  n_life_stages <- length(life_stages)
  trans_matrix <- matrix(0, ncol = n_life_stages, nrow = n_life_stages, 
                         dimnames = list(life_stages, life_stages))

  # calculate the transition probabilities for possible transitions
  trans_matrix['e', 'e'] <- m_egg_egg()
  trans_matrix['e', 'hl'] <- m_egg_larvae(temp)
  
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
run(10, life_stages, initial_pop)








