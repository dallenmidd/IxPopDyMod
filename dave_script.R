## Ixodes population dynamics model
## Dave Allen and Myles Stokowski

library(tibble)
library(readr)
library(magrittr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(igraph)

source("core_functions.R")
source("user_defined_functions.R")
source("testing_functions.R")

# set constant steps, which ensures that model doesn't try to run for longer than there is input data
# increased to see full cycle because with 15 degree temp, new eggs emerge around day 310
steps <- 1200

# At each time step, how many time steps should we look into the future to see if any
# ticks emerge from a time delay? This value is a somewhat arbitrarily high constant.
max_delay <- 300

# 00
# read inputs 
 weather <- tibble(tmean = seq(from = 15, to = 15, length.out = steps), j_day = seq(from = 1, to = steps))

# host_comm <- read_csv('inputs/host_comm.csv') #### DA Note: will have to think about how to handle this
 host_comm <- tibble(
   j_day = rep(1:steps, each=3), 
   host_spp = rep(c('mouse', 'squirrel', 'deer'), steps), 
   host_den = rep(c(1, 0.5, 0.1), steps)) %>%
   arrange(j_day, host_spp)


n_host_spp <- host_comm %>% pull(host_spp) %>% unique() %>% length()


# option to run on simple inputs for testing

tick_params <- read_csv('inputs/2021-03-19_Dave_test/tick_parameters.csv')  %>% arrange(host_spp)
tick_transitions <- read_csv('inputs/2021-03-19_Dave_test/tick_transitions.csv')
life_stages <- tick_transitions %>% pull(from) %>% unique()


# set initial population
initial_population <- rep(0, length(life_stages))
names(initial_population) <- life_stages
initial_population['r_a'] <- 10 # start with only one cohort (adults)

# inspect test results before running
print_all_params()
test_transitions()
test_lifecycles()

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
# write_csv(out_N_df, 'outputs/ogden_output.csv')
# prev_out_N_df <- read_csv('outputs/output.csv')
# (out_N_df$pop == prev_out_N_df$pop) %>% unique()

# graph population over time
ggplot(out_N_df, aes(x = day, y = pop, color = process, shape = age_group, group = stage)) + 
  geom_point(aes(size = infected)) + # , position = 'jitter') + 
  scale_size_manual(values = c(1, 3)) + 
  scale_shape_manual(values = c(15:18)) + 
  geom_line() + 
  scale_y_log10(limits = c(1e+01, 1e+07), breaks = c(10, 100, 1000, 10000, 100000, 1000000)) +
  geom_hline(yintercept = 1000)

# just see roughly whether population is increasing or decreasing
out_N_df %>% 
  #filter(age_group == 'a') %>% 
  group_by(day) %>% 
  summarise(tot = sum(pop)) %>% 
  mutate(lambda = tot/lag(tot)) %>% 
  filter(is.finite(lambda), lambda >0) %>%
  summarise(lambda = exp(mean(log(lambda)))) %>%
  as.numeric()




