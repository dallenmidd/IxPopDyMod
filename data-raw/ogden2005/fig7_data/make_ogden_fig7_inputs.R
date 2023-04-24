# quick hacky script to prepare the weather inputs for reproducing fig7

library(tidyverse)
library(lubridate)

# I selected 6 stations in Ontario from Ogden Table 2, across a range of
# latitudes/mean DD > 0.
# Point Pelee, New Glasgow, Exeter, Hanover, South Baymouth, Kapuskasing CDA

n_locations <- 6

fig7 <- read_csv(
  "data-raw/ogden2005/make_ogden_fig7/all_normals.csv"
) %>%
  mutate(
    date = ymd(paste("1990", rep(seq(1, 12), n_locations), "01", sep = "-")),
    month = (month(date)),
  )


days_of_year <- tibble(
  date = seq(
    from = min(as.Date(fig7$date)),
    to = ceiling_date(max(as.Date(fig7$date)), "month") - days(1),
    by = "1 days"
  ),
  month = (month(date))
)

fig7 <- fig7 %>% select(-c(date))

ogden_weather <- full_join(
  days_of_year, fig7,
  by = "month"
) %>%
  arrange(station) %>%
  mutate(j_day = rep(seq(1:365), n_locations)) %>%
  select(-c(month, date))

years <- 11
ogden_weather_long <- tibble(
  station = rep(ogden_weather$station, years),
  tmean = rep(ogden_weather$daily_avg_temp, years)
) %>%
  arrange(station) %>%
  mutate(j_day = rep(seq(1, 365 * years), n_locations))

ogden_weather_long %>%
  ggplot(aes(x = j_day, y = tmean, color = station)) +
  geom_point()

station_names <- unique(ogden_weather_long$station)

for (i in station_names) {
  name <- str_replace_all(i, " ", "_") %>% str_to_lower()
  write_csv(
    filter(ogden_weather_long, station == i) %>% select(-c(station)),
    str_c("data-raw/ogden2005/make_ogden_fig7/weather_", name, ".csv")
  )
}
