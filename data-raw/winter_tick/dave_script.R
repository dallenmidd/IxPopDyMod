# play around with teh winter tick model
devtools::load_all()
library(tidyverse)
require(cowplot) # for multi-panel plots

winter_tick <- read_config("data-raw/winter_tick/stable.yml")

orignial_run <- run(winter_tick)
growth_rate(orignial_run)


winter_tick_more_hosts <- winter_tick
winter_tick_more_hosts$predictors[1, "value"] <- 0.36
more_hosts_run <- run(winter_tick_more_hosts)
growth_rate(more_hosts_run)

winter_tick_hostsx10 <- winter_tick
winter_tick_hostsx10$predictors[1, "value"] <- 1.8
hostx10_run <- run(winter_tick_hostsx10)
growth_rate(hostx10_run)
## slower growth?
## no seen graphs below

p1 <- orignial_run %>%
  filter(pop > 1) %>%
  graph_population_each_group()
p2 <- more_hosts_run %>%
  filter(pop > 1) %>%
  graph_population_each_group()
p3 <- hostx10_run %>%
  filter(pop > 1) %>%
  graph_population_each_group()

plot_grid(p1, p2, p3, labels = c("baseline", "2x hosts", "10x hosts"))
growth_rate(orignial_run)
growth_rate(more_hosts_run)
growth_rate(hostx10_run)
## growth_rate not lining up with growth shown in plots!!
# maybe you know about this issue?


winter_tick_half_hosts <- winter_tick
winter_tick_half_hosts$predictors[1, "value"] <- 0.09
half_hosts_run <- run(winter_tick_half_hosts)
growth_rate(half_hosts_run)

winter_tick_hostsx0.1 <- winter_tick
winter_tick_hostsx0.1$predictors[1, "value"] <- 0.018
hostx0.1_run <- run(winter_tick_hostsx0.1)
growth_rate(hostx0.1_run)


p1 <- orignial_run %>%
  filter(pop > 1) %>%
  graph_population_each_group()
p2 <- half_hosts_run %>%
  filter(pop > 1) %>%
  graph_population_each_group()
p3 <- hostx0.1_run %>%
  filter(pop > 1) %>%
  graph_population_each_group()

plot_grid(p1, p2, p3, labels = c("baseline", "half hosts", "tenth hosts"))
# for half hosts populatoin is decreasing but
# growth rate is greater than 1

## all that being said behavior looks good
## in relationship to moose/host density


winter_tick$predictors %>%
  filter(pred == "max_temp") %>%
  ggplot(aes(j_day, value)) +
  geom_line()

winter_tick_warm <- winter_tick
winter_tick_warm$predictors <-
  winter_tick_warm$predictors %>%
  mutate(value = ifelse(pred == "max_temp", value + 2.5, value))
warm_run <- run(winter_tick_warm)

winter_tick_cold <- winter_tick
winter_tick_cold$predictors <-
  winter_tick_cold$predictors %>%
  mutate(value = ifelse(pred == "max_temp", value - 2.5, value))
cold_run <- run(winter_tick_cold)

p1 <- cold_run %>%
  filter(pop > 1) %>%
  graph_population_each_group()
p2 <- orignial_run %>%
  filter(pop > 1) %>%
  graph_population_each_group()
p3 <- warm_run %>%
  filter(pop > 1) %>%
  graph_population_each_group()

plot_grid(p1, p2, p3, labels = c("cold", "baseline", "warm"))
## looks like cold is decreasing and warm is increasing
## also looks like a change in phenology?
## long life cycle for cold? shorter for warm?




# trying to figure out transition probabilities
# e -> q_l
# q_l -> a_l
# a_l -> e_a
# e_a -> r_a
# r_a -> e
# just want to see whether they pass the "sniff test"
# I assume there product is 1, to get stability
orignial_run %>%
  filter(day < 365.5) %>%
  group_by(stage) %>%
  summarise(max(pop), sum(pop))

# Because the same individual can be in a stage for
# multiple days, not 100% sure we can really see
# how many q_l, for example, there ever where
# but here are my guesses
# 30000 e -> 15000 q_l = 50%
# 15000 q_l -> ??? a_l = ???
# ??? a_l -> 10? 774? e_a  = ?? (this should be 3% right??)
# ?? e_a -> 10 r_a = ??
# 10 r_a -> 30000 e_a = 3000 (we know this)
