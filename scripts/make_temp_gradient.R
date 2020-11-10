library(tidyverse)
library(lubridate)

tiger1 <- read.csv(file = "data_loggers/Tiger_SitesAdded_1.csv")
tiger2 <- read.csv(file = "data_loggers/Tiger_SitesAdded_2.csv")

tiger <- rbind(tiger1, tiger2) %>%  
  mutate(doy = yday(mdy_hms(FORMATTED.DATE.TIME)))

sphinx <- read.csv(file = "data_loggers/Sphinx_SitesAdded.csv") %>%  
  mutate(doy = yday(mdy_hms(FORMATTED.DATE.TIME))) %>% 
  filter(SITE == "BACA")

ggplot( ) + 
  geom_point(sphinx, mapping = aes(x = doy, y = Temperature, color = SITE)) +
  geom_smooth(sphinx, mapping = aes(x = doy, y = Temperature))


jdf <- left_join(sphinx, tiger, by = "FORMATTED.DATE.TIME") %>% 
  mutate(diff = Temperature.x - Temperature.y)

ggplot(jdf) + 
  geom_point(mapping = aes(x = doy.x, y = Temperature.x, color = SITE.x)) +
  geom_smooth(mapping = aes(x = doy.x, y = Temperature.x, color = "IS BACA")) +
  geom_point(mapping = aes(x = doy.y, y = Temperature.y, color = SITE.y)) +
  geom_smooth(mapping = aes(x = doy.y, y = Temperature.y, color = "not BACA"))

ggplot(jdf) + 
  geom_boxplot(mapping = aes(x = SITE.y, y = diff))
