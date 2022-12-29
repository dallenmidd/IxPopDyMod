library(tidyverse)

steps <- 3650
max_delay <- 365
l <- steps + max_delay

host_comm <- tibble(
  j_day = rep(1:l, each = 3),
  host_spp = rep(c("mouse", "squirrel", "deer"), l),
  host_den = rep(c(100, 8, 8), l)
) %>%
  arrange(j_day, host_spp)

write_csv(host_comm, "inputs/improved_ixodes_scapularis/host_den.csv")
