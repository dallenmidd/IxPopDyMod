# plots digitized using https://apps.automeris.io/wpd/
# .tar files can be loaded there to edit digitization

library(tidyverse)

pl <- read_csv('data-raw/ogden2005/make_ogden_fig4/predicted_larvae.csv',
               col_names = c('jday', 'n')) %>% mutate(group = 'pl')
pn <- read_csv('data-raw/ogden2005/make_ogden_fig4/predicted_nymphs.csv',
               col_names = c('jday', 'n')) %>% mutate(group = 'pn')
pa <- read_csv('data-raw/ogden2005/make_ogden_fig4/predicted_adults.csv',
               col_names = c('jday', 'n')) %>% mutate(group = 'pa')
ol <- read_csv('data-raw/ogden2005/make_ogden_fig4/observed_larvae.csv',
               col_names = c('jday', 'n')) %>% mutate(group = 'ol')
on <- read_csv('data-raw/ogden2005/make_ogden_fig4/observed_nymphs.csv',
               col_names = c('jday', 'n')) %>% mutate(group = 'on')
oa <- read_csv('data-raw/ogden2005/make_ogden_fig4/observed_adults.csv',
               col_names = c('jday', 'n')) %>% mutate(group = 'oa')

# merge all data
df <- rbind(pl, pn, pa, ol, on, oa) %>%
  arrange(jday, group) %>%
  # It looks like Ogden Fig 4 only uses the population values from the model on
  # days where there was field sampling -- we should do the same.
  mutate(jday = round(jday))

# recreate fig 4 plots to make sure they were digitized correctly
fig4a <- df %>%
  filter(group %in% c('ol', 'pl')) %>%
  ggplot(aes(jday, n, color = group)) +
  geom_point() +
  geom_line()

fig4b <- df %>%
  filter(group %in% c('on', 'pn')) %>%
  ggplot(aes(jday, n, color = group)) +
  geom_point() +
  geom_line()

fig4c <- df %>%
  filter(group %in% c('oa', 'pa')) %>%
  ggplot(aes(jday, n, color = group)) +
  geom_point() +
  geom_line()

fig4 <- gridExtra::grid.arrange(fig4a, fig4b, fig4c)
