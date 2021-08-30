## Ixodes population dynamics model
## Dave Allen and Myles Stokowski

library(tibble)
library(readr)
library(magrittr)
library(dplyr)

devtools::load_all(export_all = FALSE)

# set constant steps, which ensures that model doesn't try to run for longer
# than there is input data increased to see full cycle because with 15 degree
# temp, new eggs emerge around day 310
steps <- 1000

# At each time step, how many time steps should we look into the future to see
# if any ticks emerge from a time delay? This value is a somewhat arbitrarily
# high constant.
max_delay <- 300

# 00
# read inputs
# weather <- read_csv('inputs/weather.csv')
# constant temperature for testing
# weather <- tibble(tmean = seq(from = 15, to = 15, length.out = steps),
#                   j_day = seq(from = 1, to = steps))

# ogden weather
weather <- read_csv('inputs/2021-04-04_Ogden/ogden_weather.csv')

# non constant temp (sine wave approx Midd temp)
# annualtemp <- 15*sin(2*pi/365*(x-110)) +5
# repeatabunch <- rep(annualtemp,100)
# weather <- tibble(tmean = repeatabunch[1:steps],
#                   j_day = seq(from = 1, to = steps))

# host_comm <- read_csv('inputs/host_comm.csv')
host_comm <- tibble(
  j_day = rep(1:steps, each=3),
  host_spp = rep(c('mouse', 'squirrel', 'deer'), steps),
  host_den = rep(c(100, 8, 8), steps)) %>%
  # host_den = runif(steps * 3, .75, 1.25) * rep(c(40, 8, 0.25), steps)) %>%
  arrange(j_day, host_spp)

# Ogden host community
# host_comm <- tibble(
#   j_day = rep(1:steps, each=2),
#   host_spp = rep(c('rodent', 'deer'), steps),
#   host_den = rep(c(200, 20), steps)) %>%
#   arrange(j_day, host_spp)

n_host_spp <- host_comm %>% pull(host_spp) %>% unique() %>% length()


# option to run on simple inputs for testing
simple <- FALSE

if (simple) {
  tick_params <- read_csv('inputs/2021-04-04_Ogden/tick_parameters.csv')  %>%
    arrange(host_spp)
  tick_transitions <- read_csv('inputs/2021-04-04_Ogden/tick_transitions.csv')
  life_stages <- tick_transitions %>% pull(from) %>% unique()
} else {
  tick_params <- read_csv('inputs/tick_parameters.csv') %>%
    arrange(host_spp) # sort so parameters are in same host_spp order for
                      # pairwise vector calculations
  tick_transitions <- read_csv('inputs/tick_transitions.csv')
  life_stages <- tick_transitions %>% pull(from) %>% unique()
}

# set initial population
initial_population <- rep(0, length(life_stages))
names(initial_population) <- life_stages
if (simple) {
  # initial_population['r_a'] <- 10 # start with only one cohort (adults)
  initial_population['q_a'] <- 10000 # Ogden initial population
} else {
  initial_population['rua'] <- 10 # start with only one cohort (adults)
}

# inspect test results before running
IxPopDyMod::print_all_params(tick_transitions, tick_params)
IxPopDyMod::test_transitions(life_stages, tick_transitions, tick_params,
                             max_delay, host_comm, weather)
IxPopDyMod::test_lifecycles(life_stages, tick_transitions)

# run the model and extract the output population matrix and delay_matrix
out <- IxPopDyMod::run(
  steps,
  initial_population,
  tick_transitions,
  tick_params,
  life_stages,
  max_delay,
  host_comm,
  weather)

out_N_df <- IxPopDyMod::output_to_df(out)

# for checking whether model output has changed, a more
# thorough alternative to visually inspecting the output graph
# write_csv(out_N_df, 'outputs/ogden_output.csv')
# prev_out_N_df <- read_csv('outputs/output.csv')
# (out_N_df$pop == prev_out_N_df$pop) %>% unique()

# graph population over time
IxPopDyMod::graph_population_each_group(out_N_df)

# just see roughly whether population is increasing or decreasing
IxPopDyMod::graph_population_overall_trend(out_N_df)
