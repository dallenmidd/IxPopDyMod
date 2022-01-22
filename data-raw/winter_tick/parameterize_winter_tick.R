# parameterize winter tick
devtools::load_all()
library(tidyverse)

winter_tick <- read_config('data-raw/winter_tick/config.yml')

# borrow some code from test_transition_values() to get the values of
# evaluated transitions
print_transition_values <- function(cfg) {
  life_stages <- get_life_stages(cfg$transitions)

  # initialize a population matrix with 10 of each tick life_stage on day 1
  # and an empty N_developing matrix
  N <- matrix(nrow = length(life_stages),
              ncol = cfg$steps + cfg$max_delay,
              data = 0)
  rownames(N) <- life_stages
  N_developing <- N
  N[,1] <- 10

  funs <- add_params_list(cfg$transitions, cfg$parameters)

  transition_values <-
    lapply(
      seq_len(nrow(funs)),
      function(row_index) {
        get_transition_val(
          time = 1,
          transition_row_with_parameters = funs[row_index, ],
          N = N,
          N_developing = N_developing,
          max_delay = cfg$max_delay,
          life_stages = life_stages,
          predictors = cfg$predictors) %>%
          setNames(str_c(funs[row_index, 'from'],
                         ' to ',
                         funs[row_index, 'to']))
        }
      )

  transition_values
}

# starting with just testing values for questing larvae, because Dave said that
# q in the feed_fun/briere_fun could be funky/dependent on tmin and tmax

# new version of feed_fun with parameters fixed except temp and q
feed_fun_fixed <- function(temp, q, a) {
  feed_fun(x = 0.18, # host density of moose
           y = temp,
           a = a, # TODO unsure what this means/does,
           q = q,
           pref = 1, # pref only for moose
           tmin = 0,
           tmax = 35
           )
}

temps <- seq(0, 35, 5)
lapply(temps, function(temp) {
  feed_fun_fixed(temp, q = 7e-4, a = 4e-4)
  })

# let's see the same for the deer tick, to get a sense of behavior we want
deer_tick_funs <- add_params_list(ogden2005$transitions, ogden2005$parameters)
deer_tick_funs[[18, 'params_list']]
deer_tick_feed_fun <-  function(temp) {
  ogden_feed_fun(x = temp,
                 y = NULL,
                 a = 0.0207,
                 q = 7e-4,
                 tmin = 10,
                 tmax = 35)
}
deer_tick_temps <- seq(10, 35, 5)
lapply(deer_tick_temps, deer_tick_feed_fun)

# The values we're getting for the winter tick are much lower than ogden. One
# difference is that ogden_feed_fun uses constant host finding probability,
# specified by parameter 'a', which has value 0.0207 the Ogden config. Parameter
# 'a' still affects host_finding probability in feed_fun, but it has a different
# effect. Presumably the host finding part of feed_fun for the winter tick
# should be somewhere in vicinity of 0.0207

# so we can isolate the host finding part
feed_fun
host_finding <- function(a) {
  x <- 0.18 # host den of moose
  pref <- 1 # preference for moose

  1 - (1 - a) ^ sum(x * pref)
}

# the a value we've been using so far in the winter tick config is 0.0004
winter_tick$parameters %>% filter(param_name == 'a', from == 'q_l')

# which results in a super low host finding probability - something is definitely
# off here
# could solve for the value of 'a' that makes the function evaluate to 0.0207,
# but for now we'll just guess

host_finding(0.0004)
host_finding(0.01)
host_finding(0.1) # this is close enough
host_finding(0.12)

# let's try winter tick feed fun again with this new a value
lapply(temps, function(temp) {
  feed_fun_fixed(temp, q = 7e-4, a = 0.1)
})
# this looks much more like the feed_fun results for the deer tick

# let's now try re-running the model with this updated value for 'a'

out_before <- run(winter_tick) # a = 0.0004
winter_tick$parameters[6, 'param_value'] <- 0.1
out_after <- run(winter_tick) # a = 0.1

# new version of graphing fun with lower limit
graph_population_each_group_lower_limit <- function(out) {
  out %>%
    mutate(pop = ifelse(pop < 1, 0, pop)) %>%
    graph_population_each_group()
}

out_before %>%
  mutate(label = 'a = 0.0004') %>%
  bind_rows(out_after %>% mutate(label = 'a = 0.1')) %>%
  graph_population_each_group_lower_limit() +
  facet_wrap(~label)

# population is still decreasing with this new a value, but definitely an improvement

winter_tick$steps <- 365 * 4
out_long <- run(winter_tick)
out_long %>% graph_population_each_group_lower_limit() +
  geom_vline(xintercept = seq(365, 365 *4, 365))

# population survives into a third year/season

# I think that weather data is starting on January 1, which may not make
# sense given the life stage that we are starting with
plot_yearly_predictors <- function(predictors_table) {
  predictors_table %>%
  filter(!is.na(j_day),
         j_day < 365) %>%
  ggplot(aes(j_day, value, color = pred)) +
  geom_point()
}

geom_vline(xintercept = seq(365, 365 *4, 365))
plot_yearly_predictors(winter_tick$predictors)
winter_tick$initial_population
# we are starting with 10 reproductive ticks - reproductive ticks should actually
# be around in roughly late spring/early summer
# two options - start with a different life stage, or start with weather data
# from a different time in the year
# from Drew and Samuel 1986, figure 5: oviposition onset is May 19/20 - June 20
# peaking around May 30 - June 10. So we could have reproductive adults emerge
# around June 5. In order to accomplish that, we could keep the same
# initial_population in the config, and alter the weather so it starts around
# June 5. That is roughly j_day 5 * 30 + 5 = 155
winter_tick$predictors <-
  winter_tick$predictors %>%
  mutate(true_j_day = j_day,
         j_day = j_day - 155) %>%
  filter(is.na(j_day) |
           j_day > 0)

plot_yearly_predictors(winter_tick$predictors)

# now we run the model again with the updated weather data
out_starting_in_june <- run(winter_tick)
out_starting_in_june %>%
  graph_population_each_group_lower_limit() +
  geom_vline(xintercept = seq(365, 365 *4, 365))

# not sure this is any better. One potential issue is that the full life cycle
# takes a little less than a year (see the vertical lines) - this was also
# an issue with the previous output, and may result from the fixed length delay
# transitions (egg to questing, and especially attached larvae to engorged adult)
# maybe instead, we could have engorged adults emerge when the temperature goes
# above a threshold? but also need to make sure they don't emerge immediately in
# the fall after a larvae becomes attached...




# TODO find values of all the transitions, as a factor of predictors and parameters
# The only predictors that we are using for winter tick (pred1 and pred2 in
# transitions table) are host_den, snow_cover, and max_temp. We've been keeping
# host_den constant at 0.18 moose/km2, so the only variable predictors are
# snow_cover and max_temp, which means we could plot each transition value as a
# function of these two predictors (on x and y axis)



winter_tick$predictors %>%
  filter(!is.na(j_day)) %>%
  group_by(pred) %>%
  summarize(
    min = min(value),
    max = max(value)
  )

funs <- add_params_list(winter_tick$transitions, winter_tick$parameters)






