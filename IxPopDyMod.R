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
# host_comm <- read_csv('inputs/host_comm.csv') #### DA Note: will have to think about how to handle this
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

# functions to extract tick age, process, and infection status from life_stage name

# return age of tick in life_stage
# e.g. age("fl") = "l"
age <- function(life_stage) {
  substr(life_stage, str_length(life_stage), str_length(life_stage))
}

# return current process of tick in life_stage
process <- function(life_stage) {
  #ifelse(str_length(life_stage) > 1,    substr(life_stage, 0, 1), '')
  ifelse(substr(life_stage, 0, 1) != '', substr(life_stage, 0, 1), '')
}

# return whether a life_stage is infected
# unlike the 3 previous and similar functions, infected() returns a boolean
infected <- function(life_stage) {
  str_detect(life_stage, "i")
  
}

# matches
# return all the life_stages matching a general life_stage string with wildcard characters ("_")
match_general <- function(string) {
  life_stages[str_detect(life_stages, pattern = str_replace_all(string, "_", "."))]
}

is_match <- function(specific, general) {
  str_detect(specific, general %>% str_replace_all('_', '.'))
}

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
    return(get_host_den(time[1])) 
    # Density dependent mortality for delay transitions was breaking because of unintended
    # consequences of operations on vectors of various lengths. As a fix, we always return 
    # a vector of length n_host_spp. This is probably fine because the temporal resolution 
    # for host community data is likely to be low enough that it's fine to repeat the same
    # value. Additionally, it's more consistent because we employ a similar strategy for 
    # the N predictor, where we always return a single value, the feeding tick population size 
    # at the current step (not a vector of feeding tick population size over time)
  } else if (TRUE %in% str_detect(life_stages, str_replace_all(pred, "_", "."))) {
    # unlike the other predictors which are length == max_delay + 1, 
    # this will always be a vector of length == 1
    return(sum(N[match_general(pred), time[1]]))
  } else {
    print("error: couldn't match pred")
  }
}

# vector subset: return a subset of named vector v where the names are in sub_names
v_sub <- function(v, sub_names) subset(v, names(v) %in% sub_names)

# 02 functional forms for transition probabilities
## DA Note: this might want to be moved to a new file, maybe part of input?
## DA Note: in the simple example can we show a simpiler verison of a host comm?
expo_fun <- function(x, y, p) ifelse(x>0,p['a']*x^p['b'],0)
briere_fun <- function(x, y, p) ifelse(x>p['tmin'] & x<p['tmax'],p['q']*x*(x-p['tmin'])*sqrt(p['tmax']-x),0) # https://doi.org/10.7554/eLife.58511
constant_fun <- function(x, y, p) p['a']
# constant_fun <- function(x, y, p) rep(p['a'], max_delay)
binomial_fun <- function(x, y, p) 1-(1-p['a'])^x

feed_fun <- function(x, y, p) {
  binomial_fun(
    x = sum(x * v_sub(p, 'pref')),
    y = NULL,
    p = p
  )}

# An alternative version of feed_fun() that is the product of the binomial
# and briere functions. If we implemented this, we wouldn't have to handle
# taking the product of multiple transition functions to claculate a probability
# in gen_trans_matrix. In this new version, the function is the product of the 
# briere_fun, which determines prob of active questing, and 
# binomial_fun, which determines prob of host finding
# x is host_den for binomial_fun, y is temp for briere_fun
# feed_fun <- function(x, y, p) {
#   binomial_fun(
#     x = sum(x * v_sub(p, 'pref')),
#     y = NULL,
#     p = p
#   ) * briere_fun(y, NULL, p)
# }

# x = host_den
engorge_fun <- function(x, y, p) sum(ifelse(rep(p['from_infected'], n_host_spp), 1, abs(ifelse(p['to_infected'], 0, 1) - v_sub(p, 'host_rc'))) * 
                                       (v_sub(p, 'feed_success') * ((x * v_sub(p, 'pref')) / sum(x * v_sub(p, 'pref')))))


# density dependent mortality
# x = host_den, y = number of feeding ticks
density_fun <- function(x, y, p) sum((v_sub(p, 'a') + (v_sub(p, 'b') * log((v_sub(p, 'c') + y * v_sub(p, 'pref') * x / sum(v_sub(p, 'pref') * x)) / x))) * 
                                                                                            v_sub(p, 'pref') * x / sum(v_sub(p, 'pref') * x))

# 03
# calculate individual transition probabilities for advancing to consecutive life stage

# This generic function will pull out the functional form and parameters needed to make the transition function, 
# then evaluate it using supplied predictors (pred1, pred2) like temperature or host density
# This approach takes an entire transition_row, since there can be multiple rows with the same 'from' and 'to'
# transition_row: a row from the tick_funs tibble
get_transition_val <- function(time, transition_row, N, parameters = tick_params) {
  
  f <- transition_row[['transition_fun']] %>% get()
  
  params <- parameters %>% 
    filter(is_match(transition_row[['from']], from), is_match(transition_row[['to']], to)) %>%
    pull(param_value)
    
  names(params) <- parameters %>%
    filter(is_match(transition_row[['from']], from), is_match(transition_row[['to']], to)) %>%
    pull(param_name)
  
  # useful for debugging with test_transitions(), for seeing the parameters
  # that are being grabbed for each transition via pattern matching
  # print(params)
  
  f(x = get_pred(time, transition_row[['pred1']], transition_row[['delay']], N), 
    y = get_pred(time, transition_row[['pred2']], transition_row[['delay']], N),
    p = params) %>% unname()
  # TODO: currently engorge_fun() uses parameters to handle infection, which is redundant bc that info 
  # is in the from and to life_stage strings. We could pass the from and to strings to f(), so that
  # engorge_fun() could use infected() to determine from_infected and to_infected
}

# 04
# at each step, we generate a new transition matrix whose transition probabilities
# are based on the input data (weather, host_community) at that time 
# Myles note: I think all the transitions we're handling this way are questing -> feeding
gen_trans_matrix <- function(time, N) {
  
  # initialize the transition matrix with NAs
  n_life_stages <- length(life_stages)
  trans_matrix <- matrix(NA, ncol = n_life_stages, nrow = n_life_stages, 
                         dimnames = list(life_stages, life_stages))
  
  transitions <- tick_funs %>% 
    filter(delay == 0,
           to %in% life_stages)  # exclude mortality
  
  mort <- tick_funs %>%
    filter(delay == 0, 
           to == 'm')

  if (nrow(transitions) > 0) {
    for (t in seq_len(nrow(transitions))) {
      from <- transitions[t,]$from
      to <- transitions[t,]$to
      # If there are multiple lines in the tick_funs input for a given transition, we take the product
      # of these probabilities. Currently, this applies to the questing to feeding transitions, which are 
      # the product of P(active questing) and P(host finding). Not sure if we want to implement something
      # similar for delay transition 
      
      # We initialize the trans_matrix with NA entries rather than zeros.
      # Previously, we assumed that if a trans_matrix cell was 0, then it hadn't been modified.
      # However, some functions can return 0 transition probabilities, which was happening 
      # with the briere_fun since the temperature (20) was above tmax for adults (16). 
      # In turn, the transition probability for questing to feeding adults was not 0 * feed_fun() = 0,
      # but just feed_fun(). Now, with the current (20 degree) temp data, no adults reach feeding,
      # so as expected model output population of feeding adults and all subsequent stages are lower
      # than previously.
      # Using NAs instead of zeros is a fix, but an alternative would be to not allow/handle taking
      # the product of multiple transition rows, and instead combine functions (see the commented out)
      # feed_fun() code above
      trans_matrix[from, to] <- ifelse(is.na(trans_matrix[from, to]), 1, trans_matrix[from, to]) *
        get_transition_val(time, transitions[t, ], N) * transitions[t, ]$fecundity
      # pretty printing of trans_matrix
      # print(ifelse(trans_matrix == 0, ".", trans_matrix %>% as.character() %>% substr(0, 4)), quote = FALSE)
    }
  }
  
  trans_matrix <- ifelse(is.na(trans_matrix), 0, trans_matrix)
  
  if (nrow(mort) > 0) {
    for (m in seq_len(nrow(mort))) {
      from_val <- mort[m,]$from
      # TODO temporary fix: fecundity is a multi-element vector if there are multiple (non-mort) 
      # transitions for the same "from". I think these should be all have the same fecundity, 
      # for now we'll just assume that is true and use the first one
      fecundity <- transitions %>% filter(from == from_val, to != 'm') %>% pull(fecundity) %>% .[1]
      trans_matrix[from_val, from_val] <- fecundity - sum(trans_matrix[from_val,]) - get_transition_val(time, mort[m,], N)
    }
  }
  
  return(trans_matrix)
}

# based on the current time, delay_arr and N (population matrix), return a delay_arr for the next time step
update_delay_arr <- function(time, delay_arr, N) {
  
  # select all delay transition functions, including mortality
  transitions <- tick_funs %>% filter(delay == 1)
  
  # loop through these transitions by from_stage 
  for (from_stage in transitions %>% pull(from) %>% unique()) {
    
    # for a given delay transition, every "from" stage has a unique "to" stage
    trans <- transitions %>% filter(from == from_stage, to != 'm')
    to_stage <- trans[['to']]
    
    # daily probability of transitioning to the next stage
    val <- get_transition_val(time, trans, N)
    
    # daily mortality during the delayed transition
    # (each "from" stage has a unique mortality transition)
    mort <- transitions %>% filter(from == from_stage, to == 'm') %>% get_transition_val(time, ., N)
    
    # Constant functions (for a fixed delay transition) return a single value
    # We increase the length so that we can do a cumsum over the vector
    # We add 1 for consistency with output vector length from non-constant fxns,
    # which is determined by time:(time + max_delay) in get_pred()
    if (length(val) == 1) val <- rep(val, max_delay + 1)
    
    days <- cumsum(val) >= 1
    
    if (TRUE %in% days) {
      
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
      
      } else {
        # Constant mortality
        surv_to_next <- (1 - mort) ^ days_to_next
      }
      
      # number of ticks emerging from from_stage to to_stage at time + days_to_next is the number of ticks 
      # that were already going to emerge then plus the current number of ticks in the from_stage * survival * fecundity
      delay_arr[to_stage, from_stage, time + days_to_next] <- delay_arr[to_stage, from_stage, time + days_to_next] +
        N[from_stage, time] * surv_to_next * trans[['fecundity']]
    }
  }
  return(delay_arr)
}

# 05 iteratively run model for steps iterations, starting with initial_population
run <- function(steps, initial_population) {
  
  # initialize a delay array of all zeros
  # dimensions: to, from, time
  delay_arr <- array(dim = c(length(life_stages), length(life_stages), steps + max_delay),
                       dimnames = list(life_stages, life_stages, NULL),
                       data = 0)
  
  
  # host community array, hard coding three tick life stages
  # this keeps track of the number of ticks of each life stage on the average host of each host spp on each day
  # hc_array <- array(dim = c(3, n_host_spp, steps + max_delay), 
  #                   data = 0, 
  #                   dimnames = list(c('l','n','a'), host_comm %>% pull(host_spp) %>% unique(), NULL)) 
  
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
  # (2) update the delay_mat based on conditions at "time" 
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
    trans_matrix <- gen_trans_matrix(time, N)
  
    # calculate the number of ticks entering delayed development
    delay_arr <- update_delay_arr(time, delay_arr, N)

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


# for use in testing, faster than running the whole model and useful for seeing which functions are being 
# problematic when the model breaks 
test_transitions <- function() {
  
  # initialize a population matrix with 10 of each tick life_stage on day 1
  N <- matrix(nrow = length(life_stages), ncol = steps, data = 0)
  N[,1] <- 10 
  rownames(N) <- life_stages
  
  # select which functions to test
  funs <- tick_funs %>%
    filter(transition_fun == 'density_fun')
  
  # loop through all the transition functions and calculate transition probabilities
  for (i in 1:nrow(funs)) {
    print(paste(funs[[i, 'from']], funs[[i, 'to']], funs[[i, 'transition_fun']]))
    print(get_transition_val(1, funs[i,], N))
  }
}


# run the model and extract the output population matrix and delay_matrix
out <- run(steps, initial_population)
out_N <- out[[1]]
out_N_developing <- out[[2]]
out_delay_mat <- out[[3]]

# get a more complete picture of the population size over time by summing the 
# population matrix (of ticks not in delays) with the N_developing matrix (of ticks
# currently undergoing a delay)
out_N <- out_N + out_N_developing
# As expected, out_N_developing only has nonzero entries corresponding to life stages
# with delay transitions FROM that life stage, specifically:
# c("__e" "eia" "eil" "ein" "eua" "eul" "eun" "f_l" "fia" "fin" "fua" "fun" "h_l")
# These are all the feeding to engorged transitions, the engorged to next life stage transitions, 
# as well as egg to hardening larvae, and hardening to questing larvae
# In turn, the model output (inspect graph or df) only has increased tick populations for these life stages

# inspect the outputs
# out_N[,1:50]
# out_delay_mat[,1:50]

# convert output population matrix to a friendly format for graphing
out_N_df <- out_N %>% t() %>% as.data.frame() %>% mutate(day = row_number()) %>% 
  pivot_longer(-c(day), names_to = 'stage', values_to = 'pop') %>%
  mutate(age_group = age(stage),
         process = process(stage),
         infected = infected(stage))

# for checking whether model output has changed, a more
# thorough alternative to visually inspecting the output graph
# write_csv(out_N_df, 'inputs/output.csv')
# prev_out_N_df <- read_csv('inputs/output.csv')
# (out_N_df$pop == prev_out_N_df$pop) %>% unique()

# graph population over time
ggplot(out_N_df, aes(x = day, y = pop, color = process, shape = age_group, group = stage)) + 
  geom_point(aes(size = infected)) + # , position = 'jitter') + 
  scale_size_manual(values = c(1, 3)) + 
  scale_shape_manual(values = c(15:18)) + 
  geom_line() + 
  scale_y_log10(limits = c(-1, 1e+05), breaks = c(10, 100, 1000, 10000, 100000, 1000000)) +
  geom_hline(yintercept = 1000)



