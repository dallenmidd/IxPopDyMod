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

mean_dd_gt_zero <- c(3336, 3100, 2317, 3536, 3791, 2733)

# turning off running model
if (FALSE) {
  for (location in locations) {
    
    print(paste("running model for location:", location))
    
    weather <- read_csv(str_c(input_dir, "make_ogden_fig7/weather_", location, ".csv"))
  
    # inspect test results before running
    print_all_params()
    test_transitions()
    test_lifecycles(graph = FALSE)
    
    out <- run(steps, initial_population)
    out_N_df <- output_to_df(out)
    
    write_csv(out_N_df, str_c("outputs/", location, "_output.csv"))
  
  }
}

# plot the results
dfs <- lapply(locations, function(location) 
  read_csv(str_c("outputs/", location, "_output.csv"))) 
dfs <- lapply(dfs, function(df) filter(df, age_group == "a", process == "a"))

titles <- paste(locations, mean_dd_gt_zero)

pop_plots <- mapply(
  function(df, title) list(graph_population_each_group(df, title)), 
  dfs, titles)

lambda_plots <-  mapply(
  function(df, title) list(graph_population_overall_trend(df, title)), 
  dfs, titles)

do.call(gridExtra::grid.arrange, lambda_plots)
do.call(gridExtra::grid.arrange, pop_plots)

# return the max number of adults during the ninth full calendar year
# (which is the last full year if model was run for 3500 steps)
max_adults_ninth_year <- function(out_N_df) {
  out_N_df %>% 
    filter(day > 365 * 8, day < 365 * 9) %>% 
    pull(pop) %>% 
    max()
}

fig7_tbl <- tibble(
  dd = mean_dd_gt_zero,
  max_adults = unlist(lapply(dfs, max_adults_ninth_year))  
)

fig7 <- ggplot(fig7_tbl, aes(mean_dd_gt_zero, max_adults)) + 
  geom_point() + 
  xlim(2000, 4000) + 
  ylim(0, 2000)

  

