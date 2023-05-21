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
config_ex_2 <- config(
  cycle = life_cycle(
    transition("__e", "__l", function(a) a, transition_type = "duration", parameters = c(a = 1)),
    transition("__e", NULL, function(a) a, transition_type = "duration", mortality_type = "per_day", parameters = c(a = 0.01)),
    transition("__l", "__n", function(a) a, transition_type = "duration", parameters = c(a = 0.01)),
    transition("__l", NULL, function(a) a, transition_type = "duration", mortality_type = "per_day", parameters = c(a = 0.05)),
    transition("__n", "__a", function(a) a, transition_type = "duration", parameters = c(a = 0.1)),
    transition("__n", NULL, function(a) a, transition_type = "duration", mortality_type = "per_day", parameters = c(a = 0.1)),
    transition("__a", "__e", function(a) a, transition_type = "probability", parameters = c(a = 489.3045))
  ),
  initial_population = c("__a" = 1000),
  steps = 365L
)
usethis::use_data(config_ex_2, overwrite = TRUE)

# recreate ogden et al. 2005 model in our package framework
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
  preds = readr::read_csv("./data-raw/ogden2005/predictors.csv")
)
usethis::use_data(ogden2005, overwrite = TRUE)

# for vignette, show varying weather data
temp_example_config <- config(
  life_cycle(
    transition("__e", "q_l", expo_fun, "duration", predictors = list(x = predictor_spec('temp', FALSE)), parameters = list(a = 2.92e-05, b = 2.27)),
    transition("__e", NULL, constant_fun, "duration", mortality_type = 'per_day', parameters = list(a = 0.02)),
    transition("q_l", "e_l", constant_fun, "probability", parameters = list(a = 0.05)),
    transition("q_l", NULL, constant_fun, "probability", mortality_type = 'per_day', parameters = list(a = 0.02)),
    transition("e_l", "q_n", expo_fun, "duration", predictors = list(x = predictor_spec('temp', FALSE)), parameters = list(a = 9.88E-06, b = 2.55)),
    transition("e_l", NULL, constant_fun, "duration", mortality_type = 'per_day', parameters = list(a = 0.02)),
    transition("q_n", "e_n", constant_fun, "probability", parameters = list(a = 0.05)),
    transition("q_n", NULL, constant_fun, "probability", mortality_type = 'per_day', parameters = list(a = 0.02)),
    transition("e_n", "q_a", expo_fun, "duration", predictors = list(x = predictor_spec('temp', FALSE)), parameters = list(a = 6.27E-04, b = 1.21)),
    transition("e_n", NULL, constant_fun, "duration", mortality_type = 'per_day', parameters = list(a = 0.02)),
    transition("q_a", "e_a", constant_fun, "probability", parameters = list(a = 0.05)),
    transition("q_a", NULL, constant_fun, "probability", mortality_type = 'per_day', parameters = list(a = 0.02)),
    transition("e_a", "r_a", expo_fun, "duration", predictors = list(x = predictor_spec('temp', FALSE)), parameters = list(a = 7.7E-04, b = 1.42)),
    transition("e_a", NULL, constant_fun, "duration", mortality_type = 'per_day', parameters = list(a = 0.02)),
    transition("r_a", "__e", constant_fun, "probability", parameters = list(a = 3000))
  ),
  initial_population = c(r_a = 10),
  steps = 730,
  # Predictor data for this example config is the temperature data from the Ogden config
  # (host density data is dropped).
  preds = readr::read_csv("./data-raw/ogden2005/predictors.csv") %>%
            dplyr::filter(pred == "temp")
)
usethis::use_data(temp_example_config, overwrite = TRUE)

# for vignette, show modifying host community
host_example_config <- config(
  life_cycle(
    transition("__e", "q_l", expo_fun, "duration", predictors = list(x = predictor_spec('temp', FALSE)), parameters = list(a = 2.92e-05, b = 2.27)),
    transition("__e", NULL, constant_fun, "duration", mortality_type = 'per_day', parameters = list(a = 0.02)),
    transition("q_l", "e_l", find_n_feed, "probability", predictors = list(x = predictor_spec("host_den")), parameters = list(a = 0.01, pref = c(deer = 0.25, mouse = 1, squirrel = 0.25), feed_success = c(deer = 0.49, mouse = 0.49, squirrel = 0.17))),
    transition("q_l", NULL, constant_fun, "probability", mortality_type = 'per_day', parameters = list(a = 0.02)),
    transition("e_l", "q_n", expo_fun, "duration", predictors = list(x = predictor_spec('temp', FALSE)), parameters = list(a = 9.88E-06, b = 2.55)),
    transition("e_l", NULL, constant_fun, "duration", mortality_type = 'per_day', parameters = list(a = 0.02)),
    transition("q_n", "e_n", find_n_feed, "probability", predictors = list(x = predictor_spec("host_den")), parameters = list(a = 0.01, pref = c(deer = 0.25, mouse = 1, squirrel = 1), feed_success = c(deer = 0.49, mouse = 0.49, squirrel = 0.17))),
    transition("q_n", NULL, constant_fun, "probability", mortality_type = 'per_day', parameters = list(a = 0.02)),
    transition("e_n", "q_a", expo_fun, "duration", predictors = list(x = predictor_spec('temp', FALSE)), parameters = list(a = 6.27E-04, b = 1.21)),
    transition("e_n", NULL, constant_fun, "duration", mortality_type = 'per_day', parameters = list(a = 0.02)),
    transition("q_a", "e_a", find_n_feed, "probability", predictors = list(x = predictor_spec("host_den")), parameters = list(a = 0.01, pref = c(deer = 1, mouse = 0, squirrel = 0), feed_success = c(deer = 0.49, mouse = 0, squirrel = 0))),
    transition("q_a", NULL, constant_fun, "probability", mortality_type = 'per_day', parameters = list(a = 0.02)),
    transition("e_a", "r_a", expo_fun, "duration", predictors = list(x = predictor_spec('temp', FALSE)), parameters = list(a = 7.7E-04, b = 1.42)),
    transition("e_a", NULL, constant_fun, "duration", mortality_type = 'per_day', parameters = list(a = 0.02)),
    transition("r_a", "__e", constant_fun, "probability", parameters = list(a = 3000))
  ),
  initial_population = c(r_a = 10),
  steps = 300,
  preds = tibble::tibble(
    pred = c("host_den", "host_den", "host_den", "temp" ),
    pred_subcategory = c("deer", "mouse", "squirrel", NA),
    j_day = NA,
    value = c(1, 5, 2, 25)
  )
)
usethis::use_data(host_example_config, overwrite = TRUE)

# for vignette, show infection dynamics

find_host <- function(x, a, pref) {
  1 - (1 - a)^sum(x * pref)
}

infect_example_config <- config(
  life_cycle(
    transition("__e", "q_l", constant_fun, "duration", parameters = list(a = 0.02)),
    transition("__e", NULL, constant_fun, "duration", mortality_type = 'per_day', parameters = list(a = 0.01)),
    transition("q_l", "f_l", find_host, "probability", predictors = list(x = predictor_spec("host_den")), parameters = list(a = 1e-4, pref = c(deer = 1, mouse = 0.05))),
    transition("q_l", NULL, constant_fun, "probability", mortality_type = 'per_day', parameters = list(a = 0.01)),
    transition("f_l", "eil", infect_fun, "probability", predictors = list(x = predictor_spec("host_den")), parameters = list(from_infected = 0, to_infected = 1, host_rc = c(deer = 0.01, mouse = 0.5), pref = c(deer = 1, mouse = 0.05))),
    transition("f_l", "eul", infect_fun, "probability", predictors = list(x = predictor_spec("host_den")), parameters = list(from_infected = 0, to_infected = 0, host_rc = c(deer = 0.01, mouse = 0.5), pref = c(deer = 1, mouse = 0.05))),
    transition("eil", "qin", constant_fun, "duration", parameters = list(a = 0.025)),
    transition("eil", NULL, constant_fun, "duration", mortality_type = 'per_day', parameters = list(a = 0.01)),
    transition("eul", "qun", constant_fun, "duration", parameters = list(a = 0.025)),
    transition("eul", NULL, constant_fun, "duration", mortality_type = 'per_day', parameters = list(a = 0.01)),
    transition("qin", "fin", find_host, "probability", predictors = list(x = predictor_spec("host_den")), parameters = list(a = 1e-4, pref = c(deer = 1, mouse = 0.05))),
    transition("qin", NULL, constant_fun, "probability", mortality_type = 'per_day', parameters = list(a = 0.01)),
    transition("qun", "fun", find_host, "probability", predictors = list(x = predictor_spec("host_den")), parameters = list(a = 1e-4, pref = c(deer = 1, mouse = 0.05))),
    transition("qun", NULL, constant_fun, "probability", mortality_type = 'per_day', parameters = list(a = 0.01)),
    transition("fun", "ein", infect_fun, "probability", predictors = list(x = predictor_spec("host_den")), parameters = list(from_infected = 0, to_infected = 1, host_rc = c(deer = 0.01, mouse = 0.5), pref = c(deer = 1, mouse = 0.05))),
    transition("fun", "eun", infect_fun, "probability", predictors = list(x = predictor_spec("host_den")), parameters = list(from_infected = 0, to_infected = 0, host_rc = c(deer = 0.01, mouse = 0.5), pref = c(deer = 1, mouse = 0.05))),
    transition("fin", "ein", infect_fun, "probability", predictors = list(x = predictor_spec("host_den")), parameters = list(from_infected = 1, to_infected = 1, host_rc = c(deer = 0.01, mouse = 0.5), pref = c(deer = 1, mouse = 0.05))),
    transition("ein", "qia", constant_fun, "duration", parameters = list(a = 0.025)),
    transition("ein", NULL, constant_fun, "duration", mortality_type = 'per_day', parameters = list(a = 0.01)),
    transition("eun", "qua", constant_fun, "duration", parameters = list(a = 0.025)),
    transition("eun", NULL, constant_fun, "duration", mortality_type = 'per_day', parameters = list(a = 0.01)),
    transition("qia", "fia", find_host, "probability", predictors = list(x = predictor_spec("host_den")), parameters = list(a = 1e-4, pref = c(deer = 1, mouse = 0))),
    transition("qia", NULL, constant_fun, "probability", mortality_type = 'per_day', parameters = list(a = 0.01)),
    transition("qua", "fua", find_host, "probability", predictors = list(x = predictor_spec("host_den")), parameters = list(a = 1e-4, pref = c(deer = 1, mouse = 0))),
    transition("qua", NULL, constant_fun, "probability", mortality_type = 'per_day', parameters = list(a = 0.01)),
    transition("fua", "eia", infect_fun, "probability", predictors = list(x = predictor_spec("host_den")), parameters = list(from_infected = 0, to_infected = 1, host_rc = c(deer = 0.01, mouse = 0.5), pref = c(deer = 1, mouse = 0))),
    transition("fua", "eua", infect_fun, "probability", predictors = list(x = predictor_spec("host_den")), parameters = list(from_infected = 0, to_infected = 0, host_rc = c(deer = 0.01, mouse = 0.5), pref = c(deer = 1, mouse = 0))),
    transition("fia", "eia", infect_fun, "probability", predictors = list(x = predictor_spec("host_den")), parameters = list(from_infected = 1, to_infected = 1, host_rc = c(deer = 0.01, mouse = 0.5), pref = c(deer = 1, mouse = 0))),
    transition("eia", "r_a", constant_fun, "duration", parameters = list(a = 0.025)),
    transition("eia", NULL, constant_fun, "duration", mortality_type = 'per_day', parameters = list(a = 0.01)),
    transition("eua", "r_a", constant_fun, "duration", parameters = list(a = 0.025)),
    transition("eua", NULL, constant_fun, "duration", mortality_type = 'per_day', parameters = list(a = 0.01)),
    transition('r_a','__e', constant_fun, 'probability', parameters = list(a = 500))
  ),
  initial_population = c(r_a = 10),
  steps = 500,
  preds = tibble::tibble(
    pred = "host_den",
    pred_subcategory = c("deer", "mouse"),
    j_day = NA,
    value = c(0.5, 15)
  )
)
usethis::use_data(infect_example_config, overwrite = TRUE)

# config for modeling winter tick population
expo_shifted_fun <- function(x, a, b, c) {
  ifelse(x >c, a * (x - c) ^ b, 0)
}

winter_tick <- config(
  life_cycle(
    transition("__e", "q_l", constant_fun, "duration", parameters = list(a = 0.0125)),
    transition("__e", NULL, constant_fun, "duration", mortality_type = 'throughout_transition', parameters = list(a = 0.5)),
    transition("q_l", "a_l", feed_fun, "probability", predictors = list(x = predictor_spec('host_den'), y = predictor_spec('max_temp')), parameters = list(a = 4e-4, pref = 1, q = 7e-4, tmax = 35, tmin = 0)),
    transition('q_l', NULL, snow_cover_fun, "probability", mortality_type = 'per_day', predictors = list(x = predictor_spec('snow_cover')), parameters = c(no_snow_mort = 0.06, snow_mort = 0.95)),
    transition('a_l', 'e_a', constant_fun, 'duration', parameters = list(a = 0.00571)),
    transition('a_l', NULL, constant_fun, 'duration', mortality_type = 'throughout_transition', parameters = list(a = 0.5)),
    transition('e_a', 'r_a', expo_shifted_fun, 'probability', predictors = list(x = predictor_spec('max_temp')), parameters = list(a = 0.01, b = 1.2, c = 15)),
    transition('e_a', NULL, snow_cover_fun, 'probability', predictors = list(x = predictor_spec('snow_cover')), mortality_type = 'per_day', parameters = c(no_snow_mort = 0.11, snow_mort = 0.64)),
    transition('r_a', '__e', constant_fun, 'probability', parameters = c(a = 3000))
  ),
  initial_population = c(r_a = 10),
  steps = 500,
  preds = readr::read_csv("./data-raw/winter_tick/predictors.csv")
)
usethis::use_data(winter_tick, overwrite = TRUE)
