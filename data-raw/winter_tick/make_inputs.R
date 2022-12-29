library(tidyverse)

# digitize Figure 2 from Drew and Samuel 1986
# which is mean weekly max/min temperatures in three habitats
# in Alberta, 1982 and 1983, for months April - December

# we could then fill in the remaining months (January - March)
# using climate normals data?

# grassland weekly max temp data from fig 5
weekly_max_temp <- read_csv("data-raw/winter_tick/drew_samuel_1986_fig5_grassland_digitized.csv")

weekly_max_temp %>%
  ggplot(aes(x = j_day, y = temp)) +
  geom_point() +
  geom_line()

# get it into the form we need for predictors.csv
# which has columns: pred, pred_subcategory, j_day, value

# actually, we can get similar data as with figure and more from this site
# https://climate.weather.gc.ca/climate_data/daily_data_e.html?hlyRange=1994-02-01%7C2022-01-12&dlyRange=1981-11-01%7C2022-01-12&mlyRange=1981-01-01%7C2007-11-01&StationID=1873&Prov=AB&urlExtension=_e.html&searchType=stnName&optLimit=yearRange&StartYear=1840&EndYear=2022&selRowPerPage=25&Line=0&searchMethod=contains&Month=1&Day=12&txtStationName=elk+island&timeframe=2&Year=1982

daily_weather <- read_csv("data-raw/winter_tick/en_climate_daily_AB_3012275_1982_P1D.csv")
daily_weather %>%
  ggplot(aes(
    x = `Date/Time`,
    y = `Snow on Grnd (cm)`
  )) +
  geom_point() +
  geom_line()

# other potentially useful climate vars are wind, min/max temp, snowfall...

predictors <- daily_weather %>%
  mutate(j_day = 1:365) %>%
  rename(max_temp = `Max Temp (°C)`, snow_cover = `Snow on Grnd (cm)`) %>%
  pivot_longer(
    cols = c(max_temp, snow_cover),
    values_to = "value", names_to = "pred"
  ) %>%
  mutate(
    pred_subcategory = NA,
    value = ifelse(is.na(value), 0, value)
  ) %>%
  select(pred, pred_subcategory, j_day, value)

# repeat for more years
years <- 11
accumulate <- predictors
for (i in 1:(years - 1)) {
  accumulate <- bind_rows(accumulate, predictors)
}
days <- seq_len(nrow(accumulate) / 2)
days <- c(days, days) %>% sort()
accumulate$j_day <- days

# add a row for host density
accumulate <- rbind(
  list("host_den", "moose", NA, 0.18), accumulate
)

write_csv(accumulate, "data-raw/winter_tick/predictors.csv")
