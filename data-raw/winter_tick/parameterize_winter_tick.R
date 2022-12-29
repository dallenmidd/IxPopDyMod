# parameterize winter tick
devtools::load_all()
library(tidyverse)

winter_tick <- read_config("data-raw/winter_tick/config.yml")

# borrow some code from test_transition_values() to get the values of
# evaluated transitions
print_transition_values <- function(cfg) {
  life_stages <- get_life_stages(cfg$transitions)

  # initialize a population matrix with 10 of each tick life_stage on day 1
  # and an empty N_developing matrix
  N <- matrix(
    nrow = length(life_stages),
    ncol = cfg$steps + cfg$max_delay,
    data = 0
  )
  rownames(N) <- life_stages
  N_developing <- N
  N[, 1] <- 10

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
          predictors = cfg$predictors
        ) %>%
          setNames(str_c(
            funs[row_index, "from"],
            " to ",
            funs[row_index, "to"]
          ))
      }
    )

  transition_values
}

# starting with just testing values for questing larvae, because Dave said that
# q in the feed_fun/briere_fun could be funky/dependent on tmin and tmax

# new version of feed_fun with parameters fixed except temp and q
feed_fun_fixed <- function(temp, q, a) {
  feed_fun(
    x = 0.18, # host density of moose
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
deer_tick_funs[[18, "params_list"]]
deer_tick_feed_fun <- function(temp) {
  ogden_feed_fun(
    x = temp,
    y = NULL,
    a = 0.0207,
    q = 7e-4,
    tmin = 10,
    tmax = 35
  )
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

  1 - (1 - a)^sum(x * pref)
}

# the a value we've been using so far in the winter tick config is 0.0004
winter_tick$parameters %>% filter(param_name == "a", from == "q_l")

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
winter_tick$parameters[6, "param_value"] <- 0.1
out_after <- run(winter_tick) # a = 0.1

# new version of graphing fun with lower limit
graph_population_each_group_lower_limit <- function(out) {
  out %>%
    mutate(pop = ifelse(pop < 1, 0, pop)) %>%
    graph_population_each_group()
}

out_before %>%
  mutate(label = "a = 0.0004") %>%
  bind_rows(out_after %>% mutate(label = "a = 0.1")) %>%
  graph_population_each_group_lower_limit() +
  facet_wrap(~label)

# population is still decreasing with this new a value, but definitely an improvement

winter_tick$steps <- 365 * 4
out_long <- run(winter_tick)
out_long %>% graph_population_each_group_lower_limit() +
  geom_vline(xintercept = seq(365, 365 * 4, 365))

# population survives into a third year/season

# I think that weather data is starting on January 1, which may not make
# sense given the life stage that we are starting with
plot_yearly_predictors <- function(predictors_table) {
  predictors_table %>%
    filter(
      !is.na(j_day),
      j_day < 365
    ) %>%
    ggplot(aes(j_day, value, color = pred)) +
    geom_point()
}

geom_vline(xintercept = seq(365, 365 * 4, 365))
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
  mutate(
    true_j_day = j_day,
    j_day = j_day - 155
  ) %>%
  filter(is.na(j_day) |
    j_day > 0)

plot_yearly_predictors(winter_tick$predictors)

# now we run the model again with the updated weather data
out_starting_in_june <- run(winter_tick)
out_starting_in_june %>%
  graph_population_each_group_lower_limit() +
  geom_vline(xintercept = seq(365, 365 * 4, 365))

# not sure this is any better. One potential issue is that the full life cycle
# takes a little less than a year (see the vertical lines) - this was also
# an issue with the previous output, and may result from the fixed length delay
# transitions (egg to questing, and especially attached larvae to engorged adult)
# maybe instead, we could have engorged adults emerge when the temperature goes
# above a threshold? but also need to make sure they don't emerge immediately in
# the fall after a larvae becomes attached...

# in this output graph, we see some population (> 1) for all life stages from
# eggs to engorged adults. However, we don't see any reproductive adults, which
# should emerge next.
out_starting_in_june %>%
  filter(
    pop > 0,
    stage == "e_a"
  ) %>%
  print(n = 100)
# there are e_a ticks starting on day 259
# and the number/population seems reasonable

out_starting_in_june %>%
  filter(
    pop > 0,
    stage == "r_a"
  )
# reproductive ticks emerge on day 322, but pop is only 0.0579
# since we started the model with 10 r_a ticks, this is too low for a stable pop
# between when e_a (day 259) and r_a (322) ticks emerge, is 63 days, but the
# transition is not a delay transition, so the transition value must be zero
# until day 322-1 = 321. Let's look at the predictor values to see why
winter_tick$predictors %>%
  filter(j_day %in% 255:325) %>%
  ggplot(aes(j_day, value, color = pred)) +
  geom_point() +
  geom_hline(yintercept = 15) + # max_temp threshold
  geom_vline(xintercept = 321) # day of first reproductive adults

winter_tick$transitions %>%
  filter(from == "e_a")

winter_tick$parameters %>%
  filter(from == "e_a")

# transition probability is a function of max_temp
lapply(
  10:20,
  function(temp) {
    expo_shifted_fun(
      x = temp,
      y = NULL,
      a = 0.01,
      b = 1.2,
      c = 15
    )
  }
)
# value is 0 if max_temp is below zero

# takeaway: reproductive ticks (predictably) don't emerge until temperature is
# above 15C threshold, and in the mean time, the population of engorged adults
# goes way down
out_starting_in_june %>%
  filter(
    stage == "e_a",
    day %in% 259:322
  ) %>%
  ggplot(aes(day, pop)) +
  geom_point()

# why are the e_a ticks dying so much? because the mortality rate is high
# until snowmelt, specifically, daily probability of mortality is 0.64
snow_cover_fun
winter_tick$parameters %>% filter(from == "e_a", to == "m")
# I got that number from Drew and Samuel 1986. Looking back at that paper,
# they define survival as the % of engorged adults that laid eggs (rather than
# dying before egg laying). This is a "per_capita_m" type of mortality, but we're
# modelling it as a daily mortality.
# TODO could we convert to a daily mortality rate?

# when (calendar year) are engorged adults emerging?
# based on Drew and Samuel 1986, it should be early march to mid may
# in current model, it's clear they emerge starting e_a. the end of when
# they emerge is trickier to determine - it's based on a fixed delay transition
# from attached larvae. But based on graph above, it seems like the bulk of new
# engorged adults have emerged by j_day 290
winter_tick$transitions %>% filter(to == "e_a")

# earlier we shifted j_day of the weather by -155
winter_tick$predictors %>%
  filter(j_day %in% c(259, 290))
414 - 365 # ~ feb 20 (259 + 155 - 365)
445 - 365 # ~ march 20
# it looks like engorged adults are emerging too early in the spring
# which makes sense because we were getting cycles in the population that were
# less than a year long

# The fixed length delay transition from attached larvae to engorged adult
# should probably go. We could use Addison and McLaughlin 1988 to break this
# up into multiple stages. This paper has timing for the length of larvae and
# nymph and adult stages on moose, including time of engorged adult "detachment",
# I can't tell from paper whether detachment is different from drop off -
# detachment being when tick stops feeding, drop off being when it physically
# drops off moose onto the ground?
# But even if we break it up using the durations of the individual on-host life
# stages from Addison and McLaughlin 1988, we still ultimately get a fixed delay,
# rather than engorged adults emerging/dropping off based on climate...
# For now, based on that paper, 190-192 days may be a better delay length than
# 175 days for a_l to e_a, so we'll change that
winter_tick$parameters %>% print()
winter_tick$parameters[10, "param_value"] <- 1 / 190

# rerunning model to see how this affects timing
out_longer_period_on_moose <- winter_tick %>% run()
out_longer_period_on_moose %>%
  graph_population_each_group_lower_limit() +
  geom_vline(xintercept = seq(365, 365 * 4, 365))

# population stays higher longer - probably because engorged adults are emerging
# later in the spring when weather is better
# this also seems to result in a pattern that is more consistent annually - which
# may be more evident from this graph where we don't set a lower limit so we
# keep seeing the population cycle after pop goes below 1
out_longer_period_on_moose %>%
  graph_population_each_group() +
  geom_vline(xintercept = seq(365, 365 * 4, 365))

# lets repeat some of plotting we did earlier for the model run, about the timing
# of e_a and r_a ticks

out_longer_period_on_moose %>%
  filter(
    pop > 0,
    stage == "e_a"
  ) %>%
  print(n = 100)
# e_a ticks now emerge on day 273

out_longer_period_on_moose %>%
  filter(
    stage %in% c("e_a"),
    day %in% 270:350
  ) %>%
  ggplot(aes(day, pop, color = stage)) +
  geom_point()
# e_a ticks peak around day 280, mostly done by day 300

out_longer_period_on_moose %>%
  filter(
    pop > 0,
    stage == "r_a"
  )
# r_a ticks still emerge on day 322, which makes sense because it's based on
# the same 15C temp threshold
# r_a tick population is slightly bigger (~0.2 rather than ~0.05 on day 322),
# but still too small for a stable population

# lets think about whether this timing makes sense based on tick phenology
273 + 155 - 365 # 63 = March 3
280 + 155 - 365 # 70 = March 10
300 + 155 - 365 # 90 = March 30
# so engorged adults are appearing (emerging and peaking) almost entirely in March
# which is probably still a little early

# even though r_a populations are way too low in magnitude, we may still be able
# to see phenology by plotting
out_longer_period_on_moose %>%
  filter(
    day >= 322,
    day < 322 + 50,
    stage == "r_a"
  ) %>%
  ggplot(aes(day, pop)) +
  geom_point()

# or same for eggs
out_longer_period_on_moose %>%
  filter(
    day >= 322,
    day < 322 + 50,
    stage == "__e"
  ) %>%
  ggplot(aes(day, pop)) +
  geom_point()
# this reveals that essentially all egg laying is done by jday 350/355
# which is
355 + 155 - 365 # day 145 = May 24

# @Dave I would start here. Still run the code above to set things up, which
# may take a few minutes - but no need to look over it. First bit of this section
# is more background - I'll put in another note about what I'm actively thinking
# about.

# looking at Drew and Samuel 1986, egg laying should starting around May 20,
# peaking May 30-June 10, ending by June 20. When you also consider that egg
# laying in reality happens over a ~20-30 day period, peaking around day 6,
# our model is doing egg laying too early
# Currently the reason that egg laying is pushed back even to where it is now,
# ending around May 24, is that the e_a to r_a transition only happens when
# the temp is above 15C. So e_a ticks just die and die till that threshold is
# met. It would be far better to model this as a delay transition.
# TODO use Drew and Samuel 1986, fig 4 to fit this transition
winter_tick$transitions %>%
  filter(from == "e_a")

# in order to do this fitting, use nls() - see Dave's example

# Consider e_a ticks that were placed in the grassland on March 19, 1983
# They took ~78 days +-3 to begin ovipositing (equivalent in our model to
# duration of transition to r_a). Survival was low, around 5%.
# The question is, why did they take ~78 days, as a function of temperature over
# the period between March 19, and March 19th plus 78 days.
# March 19th = Julian day 79, so the Julian day range is 79:(79 + 78)
# We need to get the temp for each day in this range
temp_data <-
  winter_tick$predictors %>%
  filter(
    pred == "max_temp",
    # have to advance a year (+365) to get data
    true_j_day %in% (79 + 365):(79 + 78 + 365)
  )

temp_data %>%
  ggplot(aes(j_day, value)) +
  geom_point()

# looking at how Ogden does this delay
ogden2005$transitions %>%
  filter(from == "e_a")
ogden2005$parameters %>%
  filter(str_detect("e_a", from))
# lets calculate values of this transition for a reasonable range of temps
ogden_expo_fun <- function(temp) {
  expo_fun(
    x = temp,
    a = 0.000769,
    b = 1.4
  )
}

transition_value_per_temp <-
  sapply(0:30, ogden_expo_fun)

ggplot(
  tibble(temp = 0:30, val = transition_value_per_temp),
  aes(temp, val)
) +
  geom_point()


# what happens if we apply ogden's e_a to r_a delay expo_fun to our winter tick
# weather data?
temp_data <-
  temp_data %>%
  mutate(
    temp = value,
    transition_value = ogden_expo_fun(temp),
    cumsum = cumsum(transition_value),
    days_passed = row_number()
  )

temp_data %>%
  ggplot(aes(temp, transition_value)) +
  geom_point()

temp_data %>%
  ggplot(aes(j_day, transition_value)) +
  geom_point()

temp_data %>%
  ggplot(aes(days_passed, cumsum)) +
  geom_point()

# cumsum reaches 1 around day 55
# we could adjust parameters to push it back to day ~78
# a value of 1.15 for parameter b is one way to accomplish (I found this just by
# plugging in and testing a few values)
ogden_expo_fun2 <- function(temp, a = 0.000769, b = 1.15) expo_fun(temp, NULL, a = a, b = b)
temp_data %>%
  mutate(
    temp = value,
    transition_value = ogden_expo_fun2(temp),
    cumsum = cumsum(transition_value),
    days_passed = row_number()
  ) %>%
  ggplot(aes(days_passed, cumsum)) +
  geom_point()

# now lets consider the cohort starting on april 2, which took ~63+-5 days to
# develop, starting on april 2, which is Julian day 93
# notice that this results in r_a emerging right around the same day as the previous
# cohort (Julian day 79 + 78 = 157 for previous cohort, 93 + 63 = 156 for this one)
# I was initially concerned this may be hard to reconcile, however for the first
# ~15 days between March 19th and April 2, the cumsum is only ~0.03, so the
# j_day that both groups reach a cumsum of 1 is similar

temp_data2 <-
  winter_tick$predictors %>%
  filter(
    pred == "max_temp",
    # have to advance a year (+365) to get data
    true_j_day %in% (93 + 365):(93 + 63 + 365)
  )

temp_data2 %>%
  mutate(
    temp = value,
    transition_value = ogden_expo_fun2(temp),
    cumsum = cumsum(transition_value),
    days_passed = row_number()
  ) %>%
  ggplot(aes(days_passed, cumsum)) +
  geom_point()


# We could approach this a little differently - by calculating the daily
# value of the expo_fun over each day throughout the season. Then starting at
# each day that a cohort starts, compute when the cumsum reaches 1.

# The total range of starting dates for engorged adults is March 6 to May 22.
# Those starting on May 22 take ~20 days. For a little buffer, going up to end
# of June should be sufficient.
temp_data3 <-
  winter_tick$predictors %>%
  filter(
    pred == "max_temp",
    # 66 is March 6th
    # 182 is June 30th
    true_j_day %in% (66 + 365):(182 + 365)
  ) %>%
  mutate(
    transition_value = ogden_expo_fun2(value),
    days_passed = row_number(),
    true_j_day = true_j_day - 365
  ) %>%
  select(j_day, true_j_day, days_passed, temp = value, transition_value)

# NOTE: we are using weather station data from 1982, should be okay because
# tick phenology pretty similar between 1982 and 1983 (in Drew and Samuel Fig 4)

# grassland cohorts, 1983
# start date - start jday - preoviposition duration
# March 19   -      79    - 78
# April 2    -      93    - 63
# April 16   -      107   - 42
# April 30   -      121   - 37

# what is the sum of daily transition values for the given range
# (the goal would be for them to sum to 1 for each period)
transition_cumsum_between_true_j_days <- function(start, duration) {
  temp_data3 %>%
    filter(true_j_day %in% start:(start + duration)) %>%
    pull(transition_value) %>%
    sum()
}

transition_cumsum_between_true_j_days(79, 78)
transition_cumsum_between_true_j_days(93, 63)
transition_cumsum_between_true_j_days(107, 42)
transition_cumsum_between_true_j_days(121, 37)
# the values are good for the first 2, but too low for the second 2

# how long from start day does it take for the daily transition values to
# sum to 1?
duration_till_cumsum_gte_1 <- function(start) {
  temp_data3 %>%
    filter(true_j_day >= start) %>%
    mutate(cumsum = cumsum(transition_value)) %>%
    filter(cumsum < 1) %>%
    nrow()
}

# @Dave this (also represented in next code chunk with a graph) is my best
# attempt so far to model this transition - and what I think we need to tweak

sapply(
  c(79, 93, 107, 121),
  duration_till_cumsum_gte_1
)
# desired results (from grassland cohorts, 1983 table above):
# 78, 63, 42, 37

# attempting to recreate lower left subfig of Drew and Samuel 1986 fig 4 based
# on the function we just roughly fit
tibble(
  start_date = c(79:121, 79, 93, 107, 121),
  preoviposition_period = c(sapply(79:121, duration_till_cumsum_gte_1), 78, 63, 42, 37),
  group = c(rep("our model", length(79:121)), rep("Drew and Samuel 1986", 4))
) %>%
  ggplot(aes(start_date, preoviposition_period, color = group)) +
  geom_point() +
  xlim(66, 143) + # march 6 to may 22
  ylim(0, 100)

# TODO left off here, so the issue is that the transition is taking
# too long when we start later in the season

# TODO this doesn't work - not sure how to make this compatible w formula
# fit function using y ~ x and nls?
# y would be duration of oviposition period? or cumsum of daily probabilities, always == 1?
# x would be starting day? or temperature over each day?
x <- c(79, 93, 107, 121)
y <- c(78, 63, 42, 37)
x <- x - 66 # subtract 66 because first true_j_day is 66
# this results in an
# index on the temps vector
mydata <- data.frame(x = x, y = y)
temps <- temp_data3$temp
nls(
  formula = y ~ sum(sapply(
    # temp_data3[temp_data3['true_j_day'] %in% y:(y + x), 'temp'],
    temps[x:(x + y)],
    function(x) {
      ifelse(x > 0, a * x^b, 0)
    }
  )),
  data = mydata,
  start = list(a = .000769, b = 1.15)
)
# NOTE: I don't think this is playing by the rules of formulas...

# @Dave see up to here

# function that says the day it transitions

day_to_ovi <- function(start_day, a, b) {
  winter_tick$predictors %>%
    filter(
      pred == "max_temp",
      # 66 is March 6th
      # 182 is June 30th
      true_j_day %in% (66 + 365):(182 + 365)
    ) %>%
    mutate(
      transition_value = ogden_expo_fun2(value, a = a, b = b),
      days_passed = row_number(),
      true_j_day = true_j_day - 365
    ) %>%
    select(j_day, true_j_day, days_passed, temp = value, transition_value) %>%
    filter(true_j_day >= start_day) %>%
    mutate(cumsum = cumsum(transition_value)) %>%
    filter(cumsum < 1) %>%
    nrow()
}



day_to_ovi(79, a = 0.000769, b = 1.15)
day_to_ovi(79, a = 0.0004, b = 1.5)


x <- c(79, 93, 107, 121)
y <- c(78, 63, 42, 37)
mydata <- data.frame(x = x, y = y)
plot(x, y)

nls(
  formula = y ~ day_to_ovi(start_day = x),
  data = mydata,
  start = list(a = 0.000769, b = 1.15)
)

nll_first_day <- function(a, b) {
  pred_day <- numeric(length = 4)
  for (i in 1:4) pred_day[i] <- day_to_ovi(start_day = mydata$x[i], a = a, b = b)
  -sum(dnorm(x = mydata$y, mean = pred_day, sd = 10, log = TRUE))
}


try_a <- seq(from = 0.0001, to = 0.001, length = 10)
try_b <- seq(from = 1, to = 2, length = 10)
nllmat <- matrix(nrow = 10, ncol = 10)
for (a1 in 1:10) {
  for (b1 in 1:10) {
    nllmat[a1, b1] <- nll_first_day(a = try_a[a1], b = try_b[b1])
  }
}

min(nllmat)
nllmat == min(nllmat) # nllmat[2,7]
nllmat[2, 7]
nll_first_day(try_a[2], try_b[7])
try_a[2]
try_b[7]

start_dates <- c(79, 93, 107, 121)
durations <- lapply(
  start_dates,
  function(start) {
    day_to_ovi(start, a = try_a[2], b = try_b[7])
  }
)

# compare our model with new parameterization to Drew and Samuel 1986
tibble(
  start_date = c(79:121, 79, 93, 107, 121),
  preoviposition_period = c(
    sapply(
      79:121,
      function(start) {
        day_to_ovi(start,
          a = try_a[2],
          b = try_b[7]
        )
      }
    ),
    78, 63, 42, 37
  ),
  group = c(rep("our model", length(79:121)), rep("Drew and Samuel 1986", 4))
) %>%
  ggplot(aes(start_date, preoviposition_period, color = group)) +
  geom_point() +
  xlim(66, 143) + # march 6 to may 22
  ylim(0, 100)

# this seems reasonably close. Now let's run the model with these new params

winter_tick$transitions
winter_tick$transitions[7, "transition_fun"] <- "expo_fun"
winter_tick$transitions[7, "delay"] <- TRUE
winter_tick$transitions[8, "delay"] <- TRUE

winter_tick$parameters <- winter_tick$parameters[-c(14), ] # drop parameter 'c'
winter_tick$parameters[c(12, 13), "param_value"] <- c(try_a[2], try_b[7])

# so now we've fixed the e_a to r_a transition, but mortality for that transition
# is probably still bad
# It would probably be ideal to model daily mortality, as a function of snow
# cover and/or temperature. Then fit this function with a similar process as
# above. But a simpler approach is just doing per_capita_m, based on whether
# adults drop off hosts before snowmelt (survival 11%), or after (survival mean
# of 73% and 55%). I had initially intended to do this, but implemented it
# incorrectly... so all we have to do is switch to per_capita_m
# actually, these numbers are survival and I was using them as mortality,
# need to do 1 - (survival) to get mortality
winter_tick$transitions[8, "to"] <- "per_capita_m"
winter_tick$parameters[c(14, 15), "to"] <- "per_capita_m"
winter_tick$parameters[c(14, 15), ]
winter_tick$parameters[c(14, 15), "param_value"] <- c(1 - .64, 1 - 0.11)


# let's try running it
# make sure we haven't broken anything obvious
validate_config(winter_tick)

# I think we just want to use the first day of snow cover data - it's throwing
# a warning, and anyway we want to base mortality on whether there is snow
# on the day that ticks drop off
snow_cover_fun
# I modified snow_cover_fun() so it just uses x[1]
devtools::load_all()
snow_cover_fun
validate_config(winter_tick)
# warning fixed

out_updated_ea_ra_transition <- run(winter_tick)
out_updated_ea_ra_transition %>%
  graph_population_each_group_lower_limit() +
  geom_vline(xintercept = seq(365, 365 * 4, 365))

# now the population is increasing
# timing seems to follow a 365 day cycle - eggs always emerge at a multiple of
# 365 days
out_updated_ea_ra_transition %>%
  filter(
    stage == "__e",
    pop > 1,
    day %% 365 == 0
  ) %>%
  mutate(
    growth_rate = pop / lag(pop)
  )
# appears that between years, egg population is increasing by a factor of 17
# let's drill down more into an individual year
out_updated_ea_ra_transition %>%
  filter(day < 366) %>%
  graph_population_each_group_lower_limit()

out_updated_ea_ra_transition %>%
  filter(
    pop > 1,
    stage %in% c("__e", "r_a"),
    day > 100
  )

winter_tick$initial_population
# if we want a stable population, we should have about 10 r_a at the end of the
# first year - instead r_a peak at 83

# could use Drew and Samuel 1986 for validation - rough look at survival between
# life stages
# they found that between habitats, c(25, 10, 27) percent of engorged females
# survived and laid viable eggs
out_updated_ea_ra_transition %>%
  filter(
    pop > 1,
    # stage %in% c('e_a', 'r_a'),
    day < 370
  ) %>%
  group_by(stage) %>%
  slice(which.max(pop)) %>%
  arrange(day)


out_updated_ea_ra_transition %>%
  filter(
    pop > 1,
    stage %in% c("e_a", "r_a"),
    day < 370,
    day > 250
  ) %>%
  graph_population_each_group()


# As a very rough way of getting the population stable, we could adjust the
# survival of attached larvae to engorged adults. This is probably the value
# that I have least literature on, so it's not so unreasonable to adjust it.
winter_tick_short <- winter_tick
winter_tick_short$steps <- 400
diff_on_moose_survival <- vary_param(winter_tick_short,
  param_row = 11,
  values = 1 - (1:5) / 20
)
out_diff_on_moose_survival <- run_all_configs(diff_on_moose_survival,
  parallel = TRUE
)

lapply(
  c(out_diff_on_moose_survival, list("initial" = out_updated_ea_ra_transition)),
  function(df) {
    filter(
      df,
      pop > 1,
      day <= 365,
      day > 100
    ) %>%
      group_by(stage) %>%
      summarize(max_pop = max(pop))
  }
)

winter_tick_stable <- winter_tick
winter_tick_stable$parameters[11, "param_value"] <- 0.97
out_winter_tick_stable <- run(winter_tick_stable)
out_winter_tick_stable %>%
  graph_population_each_group_lower_limit()
out_winter_tick_stable %>%
  group_by(stage, year = floor(day / 365)) %>%
  summarize(max = max(pop)) %>%
  arrange()

winter_tick_stable %>%
  write_config(
    config_path = "data-raw/winter_tick/stable.yml",
    transitions_path = "data-raw/winter_tick/stable_transitions.csv",
    parameters_path = "data-raw/winter_tick/stable_parameters.csv",
    predictors_path = "data-raw/winter_tick/stable_predictors.csv"
  )

# can pickup here without running prev code
devtools::load_all()
library(tidyverse)
cfg <- read_config("data-raw/winter_tick/stable.yml")
cfg$steps <- 365 * 2
o <- run(cfg)
o <- o %>%
  mutate(
    true_j_day = day + 155,
    date = as.Date(true_j_day, origin = "1982-01-01"),
    month = month(date)
  )

o %>%
  filter(day <= 365) %>%
  mutate(pop = ifelse(pop < 1, 0, pop)) %>%
  ggplot(aes(date, pop, color = stage)) +
  geom_point() +
  geom_line() +
  scale_y_log10() +
  scale_x_date(
    date_breaks = "1 month",
    labels = function(date) month(date)
  ) +
  xlab("month")


graph(pop)
graph_population_each_group_lower_limit() +
  scale_x_date()


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
