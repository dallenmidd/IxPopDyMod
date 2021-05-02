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
source("graphing_functions.R")

steps <- 3500
max_delay <- 300

input_dir <- 'inputs/2021-04-04_Ogden/'

host_comm <- read_csv(str_c(input_dir, 'host_comm.csv'))
n_host_spp <- host_comm %>% pull(host_spp) %>% unique() %>% length()

tick_params <- 
  read_csv(str_c(input_dir, '/tick_parameters.csv')) %>%
  arrange(host_spp)
tick_transitions <- read_csv(str_c(input_dir, 'tick_transitions.csv'))
life_stages <- tick_transitions %>% pull(from) %>% unique()

# set initial population
initial_population <- rep(0, length(life_stages))
names(initial_population) <- life_stages
initial_population['q_a'] <- 10000 # Ogden initial population

locations <- c("exeter", "hanover", "kapuskasing_cda", 
               "new_glasgow", "point_pelee", "south_baymouth")

for (location in locations) {
  
  print(paste("running model for location:", location))
  
  weather <- read_csv(str_c(input_dir, "make_ogden_fig7/weather_", location, ".csv"))

  # inspect test results before running
  print_all_params()
  test_transitions()
  test_lifecycles(graph = FALSE)
  
  out <- run(steps, initial_population)
  out_N_df <- output_to_df(out)

}


