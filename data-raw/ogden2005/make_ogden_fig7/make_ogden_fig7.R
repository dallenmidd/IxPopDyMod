library(tidyverse)

ogden_config <- read_config('inputs/2021-04-04_Ogden/ogden_config.yml')

set_weather <- function(cfg, weather) {
  cfg$weather <- read_csv(paste0('inputs/2021-04-04_Ogden/make_ogden_fig7/weather_',
                                 weather, '.csv'))
  cfg
}

locations <- c("exeter", "hanover", "kapuskasing_cda",
               "new_glasgow", "point_pelee", "south_baymouth")

mean_dd_gt_zero <- c(3336, 3100, 2317, 3536, 3791, 2733)

configs <- sapply(locations, function(x) {set_weather(ogden_config, x)},
                  simplify = FALSE)

out <- run_all_configs(configs, parallel = TRUE)
# write_csv(out, 'outputs/ogden_fig7_output.csv')
# out <- read_csv('outputs/ogden_fig7_output.csv')

# plot the results
dfs <- lapply(locations, function(location)
  filter(out, config == location))
dfs <- lapply(dfs, function(df) filter(df, age_group == "a", process == "a"))

titles <- paste(locations, mean_dd_gt_zero)

pop_plots <- mapply(
  function(df, title) list(graph_population_each_group(df, title)),
  dfs, titles)

lambda_plots <-  mapply(
  function(df, title) list(graph_population_overall_trend(df, title)),
  dfs, titles)

do.call(gridExtra::grid.arrange, lambda_plots)
do.call(gridExtra::grid.arrange, pop_plots)

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

fig7 <- ggplot(fig7_tbl, aes(mean_dd_gt_zero, max_adults)) +
  geom_point(size = 4, shape = 0) +
  xlim(2000, 4000) +
  ylim(0, 2000) +
  xlab("Mean annual degree-days >O °C") +
  ylab("Maximum no. adult ticks at equilibrium") +
  theme_classic(base_size = 20)



