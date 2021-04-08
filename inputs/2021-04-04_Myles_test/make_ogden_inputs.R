# quick script for getting the monthly temperature data from Ogden 2005
# to daily intervals based on the (variable) number of days in each month

library(tidyverse)
library(lubridate)

# I first digitized Figure 2 from Ogden et al. 2005
# data are mean monthly normal temperature in degrees C
fig2 <-  read_csv(
  'inputs/2021-04-04_Myles_test/Ogden_2005_fig2_digitzed.csv',
  col_names = c('month_str', 'tmean')) %>% 
  mutate(
    date = ymd(paste("1990", seq(1, 12), "01", sep = "-")),
    month = (month(date)) 
    )

days_of_year <- tibble(
  date = seq(from = min(as.Date(fig2$date)),
             to = ceiling_date(max(as.Date(fig2$date)), "month") - days(1),
             by = "1 days"),
  month = (month(date)))

# drop extra colums so the join works
fig2 <- fig2 %>% select(-c(date, month_str))

ogden_weather <- inner_join(
  days_of_year, fig2, 
  by = c('month', 'month'))  %>% 
  mutate(j_day = seq(1:365)) %>% 
  select(-c(month, date))

# if we want the data to repeat for more than a year...
years <- 10
ogden_weather <- tibble(
  tmean = rep(ogden_weather$tmean, years),
  j_day = seq(1, 365 * years)
)

# plot for a sanity check
ggplot(ogden_weather, aes(j_day, tmean)) + geom_line()


write_csv(ogden_weather, 'inputs/2021-04-04_Myles_test/ogden_weather.csv')

