## QUick and dirty code to make a climate input file
# Dave Allen
require(tidyverse)
require(lubridate)

# Read in PRISM raw data 2016-2019

prismfiles <- list.files('inputs/prism_raw/')

prismData <- read_csv(paste('inputs/prism_raw/',prismfiles[1],sep=''),skip = 11, col_names = c('site','long','lat','elev','date','ppt','tmean','tdmean','vpdmin','vpdmax'))
for (i in 2:length(prismfiles))
{
  tempData <- read_csv(paste('inputs/prism_raw/',prismfiles[i],sep=''),skip = 11, col_names = c('site','long','lat','elev','date','ppt','tmean','tdmean','vpdmin','vpdmax'))
  prismData <- prismData %>% bind_rows(tempData)
}

# for now just do the climate file for foote
mysite <- 'Foote'

site_climate <- prismData %>%
  filter(site == mysite) %>%
  mutate(j_day = yday(date)) %>%
  group_by(j_day) %>%
  filter(j_day < 365.5) %>%
  summarise(tmean = mean(tmean), ppt = mean(ppt), vpdmean = mean(vpdmin+vpdmax)/2)
  
  
site_climate %>%
  ggplot(aes(j_day,vpdmean)) +
  geom_line()

write_csv(x = site_climate, path = 'inputs/weather.csv')  