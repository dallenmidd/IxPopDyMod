library(IxPopDyMod)
library(tidyverse)
library(usethis)

# super simple example config without delays
config_ex_1 <- read_config('data-raw/config_ex_1/config_ex_1.yml')
use_data(config_ex_1, overwrite = TRUE)

# simple example config using delays
config_ex_2 <- read_config('data-raw/config_ex_2/config_ex_2.yml')
use_data(config_ex_2, overwrite = TRUE)

# recreate ogden et al. 2005 model in our package framework
ogden2005 <- read_config('data-raw/ogden2005/ogden_config.yml')
use_data(ogden2005, overwrite = TRUE)
