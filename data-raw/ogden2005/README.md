Replicating the Ogden et al. (2005) model
================

This page describes how we replicate the [Ogden et
al. (2005)](https://doi.org/10.1016/j.ijpara.2004.12.013) *Ixodes
scapularis* population dynamics model in our package framework. The
Ogden et al. (2005) model is included in the package with the
`ogden2005` config.

## Differences with Odgen et al. (2005)

We were able to largely replicate Odgen et al.’s (2005) model directly
with our model structure, but make a few changes:

- Ogden et al. (2005) have density-dependent reduction fecundity based
  on the number of adult ticks on deer. We take fecundity (number of
  eggs produced) as a constant.
- Ogden et al. (2005) handle host finding differently
  - They give a weekly probability of host-finding. We translate this
    into a daily probability.
  - They use a curve (Figure 3) to determine the probability a tick
    quests based on temperature. We approximate these curves with Briere
    functions.
- Temperature inputs for the default model run were digitized from
  Figure 2.
- Temperature inputs for the multiple model runs under different
  climates (Figure 7) were downloaded from the Government of Canada’s
  [Canadian Climate Normals
  page](https://climate.weather.gc.ca/climate_normals/index_e.html).

Even with these slight differences we are able to qualitatively
replicate the model’s results.

## Specifying the model

Here is the R code to specify the Ogden et al. (2005) model in our
package

``` r
# library(IxPopDyMod)
library(tidyverse)
devtools::load_all()
ogden2005 <- config(
  life_cycle(
  # from, to, fun, transition_type, mortality_type = NULL, predictors = NULL, parameters = list()
    transition("__e", "h_l", expo_fun, "duration", predictors = list(x = predictor_spec("temp", FALSE)), parameters = list(a = 2.92e-05, b = 2.27)),
    transition("__e", NULL, constant_fun, "duration", mortality_type = "per_day", parameters = list(a = 0.002)),
    transition("e_l", NULL, constant_fun, "duration", mortality_type = "per_day", parameters = list(a = 0.003)),
    transition("e_l", "q_n", expo_fun, "duration", predictors = list(x = predictor_spec("temp", FALSE)), parameters = list(a = 9.883278e-06, b = 2.55)),
    transition("e_n", NULL, constant_fun, "duration", mortality_type = "per_day", parameters = list(a = 0.002)),
    transition("e_n", "q_a", expo_fun, "duration", predictors = list(x = predictor_spec("temp", FALSE)), parameters = list(a = 0.0006265664, b = 1.21)),
    transition("a_l", "e_l", constant_fun, "duration", parameters = list(a = 0.5)),
    transition("a_n", "e_n", constant_fun, "duration", parameters = list(a = 0.25)),
    transition("h_l", "q_l", constant_fun, "duration", parameters = list(a = 0.0476)),
    transition("h_l", NULL, constant_fun, "duration", mortality_type = "per_day", parameters = list(a = 0.006)),
    transition("q_l", NULL, constant_fun, "probability", mortality_type = "per_day", parameters = list(a = 0.006)),
    transition("e_a", "r_a", expo_fun, "duration", predictors = list(x = predictor_spec("temp", FALSE)), parameters = list(a = 0.0007692308, b = 1.42)),
    transition("r_a", "__e", constant_fun, "probability", parameters = list(a = 3000)),
    transition("e_a", NULL, constant_fun, "duration", mortality_type = "per_day", parameters = list(a = 1e-04)),
    transition("a_a", "e_a", constant_fun, "duration", parameters = list(a = 0.111)),
    transition("q_a", NULL, constant_fun, "probability", mortality_type = "per_day", parameters = list(a = 0.006)),
    transition("q_n", NULL, constant_fun, "probability", mortality_type = "per_day", parameters = list(a = 0.006)),
    transition("q_l", "a_l", ogden_feed_fun, "probability", predictors = list(x = predictor_spec("temp")), parameters = list(a = 0.0207, q = 7e-04, tmax = 35, tmin = 10)),
    transition("q_n", "a_n", ogden_feed_fun, "probability", predictors = list(x = predictor_spec("temp")), parameters = list(a = 0.0207, q = 7e-04, tmax = 35, tmin = 10)),
    transition("q_a", "a_a", ogden_feed_fun, "probability", predictors = list(x = predictor_spec("temp")), parameters = list(a = 0.0459, q = 0.0088, tmax = 16, tmin = 3)),
    transition("a_l", NULL, density_fun, "duration", mortality_type = "throughout_transition", predictors = list(x = predictor_spec("host_den"), y = predictor_spec("[af]..")), parameters = list(a = 0.65, b = 0.049, c = 1.01, pref = c(deer = 0, mouse = 1))),
    transition("a_n", NULL, density_fun, "duration", mortality_type = "throughout_transition", predictors = list(x = predictor_spec("host_den"), y = predictor_spec("[af]..")), parameters = list(a = 0.55, b = 0.049, c = 1.01, pref = c(deer = 0, mouse = 1))),
    transition("a_a", NULL, density_fun, "duration", mortality_type = "throughout_transition", predictors = list(x = predictor_spec("host_den"), y = predictor_spec("[af]..")), parameters = list(a = 0.5, b = 0.049, c = 1.01, pref = c(deer = 1, mouse = 0)))
  ),
  initial_population = c(q_a = 10000),
  steps = 3500,
  preds = readr::read_csv("predictors.csv")
)
```

    ## Warning: Setting row names on a tibble is deprecated.

## Ogden et al. (2005) Figure 4

In Figure 4 Ogden et al. (2005) show that their model can replicate tick
seasonality data collected from Long Point, Ontario. Here we should that
our version of Odgen’s model largely follows their model output. For
this graph we digitized the model predicted values from Figure 4. We
omit the observed values here, because our purpose is to show we can
replicate Ogden’s model output rather than whether we are replicating
observed data.

``` r
our_mod <- run(ogden2005)
ogd_mod <- read.csv('fig4_data/fig4_pred_data.csv')

# take just actively questing ticks 6 yrs into the run get stabilized population
questing <- our_mod %>%
  mutate(post_eq_jday = day - 365 * 6) %>%
  filter(post_eq_jday > 0,
         post_eq_jday < 365 * 2,
         str_detect(stage,'a_'))

# get julian days shown in Fig 4 for each life stage
fig4_jdays_l <- ogd_mod %>%
  filter(stage == 'pl') %>%
  pull(jday) %>%
  unique()
fig4_jdays_n <- ogd_mod %>%
  filter(stage == 'pn') %>%
  pull(jday) %>%
  unique()
fig4_jdays_a <- ogd_mod %>%
  filter(stage == 'pa') %>%
  pull(jday) %>%
  unique()  
  

# prepare our data for merge
our_to_merge <- questing %>%
  filter((post_eq_jday %in% fig4_jdays_l & stage == 'a_l') |
         (post_eq_jday %in% fig4_jdays_n & stage == 'a_n') |
         (post_eq_jday %in% fig4_jdays_a & stage == 'a_a') ) %>%
  transmute(jday = post_eq_jday,
            num = pop, 
            stage = stage
           )

merged_fig4 <- rbind(ogd_mod, our_to_merge) %>%
  mutate(which_model = ifelse(str_detect(stage,'a_'),'IxPopDy','Ogden 2005'),
         life_stage = recode(stage,pa = 'Adult', a_a = 'Adult', 
                             pn = 'Nymph', a_n = 'Nymph',
                             pl = 'Larva', a_l = 'Larva')) %>%
  # scale both to proportion of total questing that year (y axis in Figure 4)
  group_by(which_model, life_stage) %>%
  mutate(num = num/sum(num)) %>%
  ungroup() %>%
  mutate(life_stage = factor(life_stage, levels = c('Larva', 'Nymph', 'Adult')))

merged_fig4 %>%
  ggplot(aes(jday, num, shape = which_model, linetype = which_model)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  xlab('Day of year') +
  ylab('Proportion of year total') +
  facet_wrap(~life_stage, dir = 'v')
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

## Ogden et al. (2005) Figure 7

Figure 7 shows the predicted number of ticks at a range of locations in
Canada. Here we reproduce the figure for a subset of locations, just six
from across the latitudinal range in Ontario. We are looking to
replicate the behavior in Figure 7 for these sites.

``` r
# a quick function to quickly change of the weather in an existing model config
set_weather <- function(cfg, weather) {
  temp_weather <- read_csv(paste0(
    "fig7_data/weather_",
    weather, ".csv"
  ))
  cfg$preds <- tibble(
    value = c(20, 200, temp_weather$tmean),
    j_day = c(NA, NA, temp_weather$j_day),
    pred = c("host_den", "host_den", rep("temp", dim(temp_weather)[1])),
    pred_subcategory = c("deer", "rodent", rep(NA, dim(temp_weather)[1]))
  )

  return(cfg)
}

locations <- c(
  "exeter", "hanover", "kapuskasing_cda",
  "new_glasgow", "point_pelee", "south_baymouth"
)

mean_dd_gt_zero <- c(3336, 3100, 2317, 3536, 3791, 2733)

configs <- sapply(locations, function(x) {
  set_weather(ogden2005, x)
},
simplify = FALSE
)

outputs <- lapply(configs, run)

dfs <- lapply(outputs, function(df) filter(df, stage == 'a_a'))

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
```

We look to replicate the general behavior in Ogden et al. (2005)’s
Figure 7 that tick density is near zero until degree days reach about
2750 C and then increase linearly from there.

``` r
ggplot(fig7_tbl, aes(mean_dd_gt_zero, max_adults)) +
  geom_point() +
  xlim(2000, 4000) +
  xlab("Mean annual degree-days >O °C") +
  ylab("Maximum no. adult ticks at equilibrium") +
  theme_classic()
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
