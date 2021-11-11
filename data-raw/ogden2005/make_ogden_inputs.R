# quick script for getting the monthly temperature data from Ogden 2005
# to daily intervals based on the (variable) number of days in each month

# also includes quick calculations for determining daily probability of host finding
# using Ogden's approach

library(tidyverse)
library(lubridate)

#########################
# weather data
#########################

# I first digitized Figure 2 from Ogden et al. 2005
# data are mean monthly normal temperature in degrees C
fig2 <-  read_csv(
  'inputs/2021-04-04_Ogden/ogden_2005_fig2_digitzed.csv',
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

# drop extra columns so the join works
fig2 <- fig2 %>% select(-c(date, month_str))

ogden_weather <- inner_join(
  days_of_year, fig2,
  by = c('month' = 'month'))  %>%
  mutate(j_day = seq(1:365)) %>%
  select(-c(month, date))

# if we want the data to repeat for more than a year...
years <- 11
ogden_weather <- tibble(
  tmean = rep(ogden_weather$tmean, years),
  j_day = seq(1, 365 * years)
)

# plot for a sanity check
ggplot(ogden_weather, aes(j_day, tmean)) + geom_line()


write_csv(ogden_weather, 'inputs/2021-04-04_Ogden/ogden_weather.csv')

###############################
# host finding probability
###############################

# p_w = prob tick finds host in a week
# p_d = prob tick finds host in a day
# p_w = 1 - (1 - p_d) ^ 7

# larvae and nymphs
p_w <- .0089 * 200 ^ .515
p_d <- 1 - ((1 - p_w) ^ (1/7))
# p_d = 0.02071142

# adults
p_w <- .06 * 20 ^ .515
p_d <- 1 - ((1 - p_w) ^ (1/7))
# p_d = 0.04597015



#################################
# host community input
#################################
days <- 365 * 11

host_comm <- tibble(
  j_day = rep(1:days, each=2),
  host_spp = rep(c('rodent', 'deer'), days),
  host_den = rep(c(200, 20), days)) %>%
  arrange(j_day, host_spp)

write_csv(host_comm, 'inputs/2021-04-04_Ogden/host_comm.csv')



