# library(IxPopDyMod)
devtools::load_all()
library(tidyverse)
library(usethis)

# super simple example config without delays
config_ex_1 <- config(
  cycle = life_cycle(
    transition("__e", "__l", function(a) a, transition_type = "probability", parameters = c(a = 1)),
    transition("__e", NULL, function(a) a, transition_type = "probability", mortality_type = "per_day", parameters = c(a = 0)),
    transition("__l", "__n", function(a) a, transition_type = "probability", parameters = c(a = 0.01)),
    transition("__l", NULL, function(a) a, transition_type = "probability", mortality_type = "per_day", parameters = c(a = 0.99)),
    transition("__n", "__a", function(a) a, transition_type = "probability", parameters = c(a = 0.1)),
    transition("__n", NULL, function(a) a, transition_type = "probability", mortality_type = "per_day", parameters = c(a = 0.9)),
    transition("__a", "__e", function(a) a, transition_type = "probability", parameters = c(a = 1000)),
    transition("__a", NULL, function(a) a, transition_type = "probability", mortality_type = "per_day", parameters = c(a = 0))
  ),
  initial_population = c("__a" = 1000),
  steps = 29L
)
usethis::use_data(config_ex_1, overwrite = TRUE)

# simple example config using delays
config_ex_2 <- read_config("data-raw/config_ex_2/config_ex_2.yml")
use_data(config_ex_2, overwrite = TRUE)

# recreate ogden et al. 2005 model in our package framework
ogden2005 <- read_config("data-raw/ogden2005/ogden_config.yml")
use_data(ogden2005, overwrite = TRUE)

# for vignette, show varying weather data
temp_example_config <- read_config("data-raw/temp_example_config/config.yml")
use_data(temp_example_config, overwrite = TRUE)

# for vignette, show modifying host community
host_example_config <- read_config("data-raw/host_example_config/config.yml")
use_data(host_example_config, overwrite = TRUE)

# for vignette, show infection dynamics

# we define find_host so that validate_config() passes
# however, it is not loaded as a function in the package, so we need to define it
# whenever we want to use this config
find_host <- function(x, y, a, pref) {
  1 - (1 - a)^sum(x * pref)
}

infect_example_config <- read_config("data-raw/infect_example_config/config.yml")
use_data(infect_example_config, overwrite = TRUE)

# config for modeling winter tick population
winter_tick <- read_config("data-raw/winter_tick/stable.yml")
use_data(winter_tick, overwrite = TRUE)
