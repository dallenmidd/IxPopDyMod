library(tidyverse)
steps <- 1200
max_delay <- 365
l <- steps + max_delay

weather <- tibble(tmean = seq(from = 15, to = 15, length.out = l), j_day = seq(from = 1, to = l))
write_csv(weather, 'inputs/2021-03-19_Dave_test/weather.csv')

host_comm <- tibble(
  j_day = rep(1:l, each=3),
  host_spp = rep(c('mouse', 'squirrel', 'deer'), l),
  host_den = rep(c(1, 0.5, 0.1), l)) %>%
  arrange(j_day, host_spp)
write_csv(host_comm, 'inputs/2021-03-19_Dave_test/host_comm.csv')

