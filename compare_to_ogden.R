# compare_to_ogden.R
# compare our model output to Ogden figure 4

library(tidyverse)
library(gridExtra)
source('inputs/2021-04-04_Myles_test/make_ogden_fig4/make_ogden_fig4.R')
out_N_df <- read_csv('outputs/ogden_output.csv')

questing <- out_N_df %>% 
  # look at data after the model has run for a while and stabilized
  mutate(post_eq_jday = day - 365 * 6) %>% 
  filter(post_eq_jday > 0,
         post_eq_jday < 365 * 2,
         process == 'q')

to_merge <- questing %>% 
  group_by(age_group) %>% 
  mutate(pop_fraction = pop/sum(pop)) %>%
  ungroup() %>%
  transmute(jday = post_eq_jday,
            n = pop_fraction * 100, # multiply by constant just to scale so similar to Ogden
            group = str_c(process, age_group))

merged <- rbind(df, to_merge)

merged4a <- merged %>% 
  filter(group %in% c('ol', 'pl', 'ql')) %>% 
  ggplot(aes(jday, n, color = group)) + 
  geom_point() + 
  geom_line()

merged4b <- merged %>% 
  filter(group %in% c('on', 'pn', 'qn')) %>%
  ggplot(aes(jday, n, color = group)) + 
  geom_point() + 
  geom_line()

merged4c <- merged %>%
  filter(group %in% c('oa', 'pa', 'qa')) %>% 
  ggplot(aes(jday, n, color = group)) + 
  geom_point() + 
  geom_line()

grid.arrange(merged4a, merged4b, merged4c)

# Comparing our results to Ogden, increase in questing larvae is 
# faster and decrease is slower, but I think that is because of how 
# we define questing ticks -- as ticks that are ready to quest 
# and may or may not be actively questing on any given day. I'm pretty
# sure Ogden is modelling number of actively questing ticks on a 
# given day, because that is what you can compare to the number of 
# questing ticks collected in the field through drag sampling.


