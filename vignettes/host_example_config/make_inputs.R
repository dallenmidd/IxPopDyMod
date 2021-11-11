library(tidyverse)
steps <- 1200
max_delay <- 365
l <- steps + max_delay

weather <- tibble(tmean = seq(from = 25, to = 25, length.out = l), j_day = seq(from = 1, to = l))
write_csv(weather, 'vignettes/host_example_config/weather.csv')

host_comm <- tibble(
  j_day = rep(1:l, each=3),
  host_spp = rep(c('mouse', 'squirrel', 'deer'), l),
  host_den = rep(c(5, 2, 1), l)) %>%
  arrange(j_day, host_spp)
write_csv(host_comm, 'vignettes/host_example_config/host_comm.csv')

