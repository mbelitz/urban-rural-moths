# load libraries
library(googledrive)
library(googlesheets4)
library(dplyr)
library(ggplot2)
library(lubridate)

frass_drive <- drive_get("FRASS Measurements")
frass <- range_read(frass_drive)

frass <- frass %>% 
  mutate(doy = lubridate::yday(Date)) %>% 
  rename(Mass = 4)

ggplot(frass, mapping = aes(x = doy, y = Mass, color = Site)) +
  geom_point() + 
  geom_smooth(se = F)

f2 <- frass %>% 
  filter(doy < 320) %>% 
  group_by(doy, Site) %>% 
  summarise(avg_mass = mean(Mass))

ggplot(f2, mapping = aes(x = doy, y = avg_mass, color = Site)) +
  geom_point() + 
  geom_smooth(se = F)

f3 <- frass %>% 
  filter(doy < 320) %>% 
  filter(is.na(Notes)) %>% 
  group_by(doy, Site) %>% 
  summarise(avg_mass = mean(Mass))

ggplot(f3, mapping = aes(x = doy, y = avg_mass, color = Site)) +
  geom_point() + 
  geom_smooth(se = F)

f4 <- f3 %>% 
  mutate(urban = case_when(Site == "AUCA" | Site == "RIST" | Site == "PRCR" ~ "Rural",
                           Site == "BACA" | Site == "JOMA" | Site == "COFR" ~ "Urban",
                           Site == "DEMI" | Site == "BIVA" | Site == "BOWA" ~ "Suburban"))

ggplot(f4, mapping = aes(x = doy, y = avg_mass, color = urban)) +
  geom_point() + 
  geom_smooth(se = F) +
  theme_bw()

