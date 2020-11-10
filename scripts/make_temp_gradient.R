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
  mutate(temp_diff = Temperature.x - Temperature.y,
         rh_diff = Relative.Humidity.x - Relative.Humidity.y,
         hsi_diff = Heat.Stress.Index.x - Heat.Stress.Index.y)

ggplot(jdf) + 
  geom_point(mapping = aes(x = doy.x, y = Temperature.x, color = SITE.x)) +
  geom_smooth(mapping = aes(x = doy.x, y = Temperature.x, color = "IS BACA")) +
  geom_point(mapping = aes(x = doy.y, y = Temperature.y, color = SITE.y)) +
  geom_smooth(mapping = aes(x = doy.y, y = Temperature.y, color = "not BACA"))

ggplot(jdf) + 
  geom_boxplot(mapping = aes(x = SITE.y, y = temp_diff))

jdf2 <- na.omit(jdf) %>% 
  filter(SITE.y != "")

ggplot(jdf2) + 
  geom_boxplot(mapping = aes(x = SITE.y, y = temp_diff))

gradient <- jdf2 %>% 
  group_by(SITE.y) %>% 
  summarise(mean_temp = mean(temp_diff),
            mean_rh = mean(rh_diff),
            mean_hsi = mean(hsi_diff))













auca <- filter(jdf2, SITE.y == "AUCA")

ggplot(auca) +   
  geom_point(mapping = aes(x = FORMATTED.DATE.TIME, y = Temperature.y, color = SITE.y)) 

prcr <- filter(jdf2, SITE.y == "PRCR")

ggplot(prcr) +   
  geom_point(mapping = aes(x = FORMATTED.DATE.TIME, y = Temperature.y, color = SITE.y)) 

rist <- filter(jdf2, SITE.y == "RIST")

ggplot(rist) +   
  geom_point(mapping = aes(x = FORMATTED.DATE.TIME, y = Temperature.y, color = SITE.y)) 


demi <- filter(jdf2, SITE.y == "DEMI")

ggplot(demi) +   
  geom_point(mapping = aes(x = FORMATTED.DATE.TIME, y = Temperature.y, color = SITE.y)) 

bowa <- filter(jdf2, SITE.y == "BOWA")

ggplot(bowa) +   
  geom_point(mapping = aes(x = FORMATTED.DATE.TIME, y = Temperature.y, color = SITE.y)) 


biva <- filter(jdf2, SITE.y == "BIVA")

ggplot(biva) +   
  geom_point(mapping = aes(x = FORMATTED.DATE.TIME, y = Temperature.y, color = SITE.y)) 


cofr <- filter(jdf2, SITE.y == "COFR")

ggplot(cofr) +   
  geom_point(mapping = aes(x = FORMATTED.DATE.TIME, y = Temperature.y, color = SITE.y)) 



joma <- filter(jdf2, SITE.y == "JOMA")

ggplot(joma) +   
  geom_point(mapping = aes(x = FORMATTED.DATE.TIME, y = Temperature.y, color = SITE.y)) 

baca <- filter(jdf2, SITE.x == "BACA")

ggplot(baca) +   
  geom_point(mapping = aes(x = FORMATTED.DATE.TIME, y = Temperature.x, color = SITE.x)) 

baca2 <- filter(baca, doy.x >= 275 & doy.x <= 290)

ggplot(baca2) +   
  geom_point(mapping = aes(x = FORMATTED.DATE.TIME, y = Temperature.x, color = SITE.x)) 
