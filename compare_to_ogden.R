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
         process == 'a')

to_merge <- questing %>% 
  group_by(age_group) %>% 
  mutate(pop_fraction = pop/sum(pop)) %>%
  ungroup() %>%
  transmute(jday = post_eq_jday,
            n = pop_fraction * 20, # multiply by constant just to scale so similar to Ogden
            group = str_c(process, age_group))

merged <- rbind(df, to_merge)

merged4a <- merged %>% 
  filter(group %in% c('ol', 'pl', 'al')) %>% 
  ggplot(aes(jday, n, color = group)) + 
  geom_point() + 
  geom_line()

merged4b <- merged %>% 
  filter(group %in% c('on', 'pn', 'an')) %>%
  ggplot(aes(jday, n, color = group)) + 
  geom_point() + 
  geom_line()

merged4c <- merged %>%
  filter(group %in% c('oa', 'pa', 'aa')) %>% 
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

# Perhaps the most obvious difference between our model and Ogden's is that 
# the adult questing ticks peak earlier in our model. Instead, we should 
# be comparing Ogden's results to the number of ATTACHED (not questing) ticks, 
# because attached is the stage whose population is determined by the 
# probability of questing (questing in our model just means
# has emerged from development and will quest if the weather is right).

# Having changed to looking at attached rather than questing ticks, 
# we're getting a really close fit with Ogden's model. However, this is still
# scaled by an arbitrary contant just to see phenology - the absolute numbers 
# don't mean anything at this point. One factor that would make this challenging
# is that our attached ticks are dependent on the probability of finding hosts, 
# which is a constant. This is different from Ogden's observed or modelled questing
# ticks, which are presumably just based on probability of questing, not 
# p(quest) * p(find host)
