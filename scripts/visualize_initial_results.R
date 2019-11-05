# load libraries

library(googlesheets)
library(dplyr)
library(ggplot2)

adults_counted <- gs_title(x = "Adult Moth Datasheet")
counted_moths <- gs_read(adults_counted)

counted_moths <- counted_moths %>% 
  mutate(doy = lubridate::yday(eventDate)) %>% 
  mutate(ui = ifelse(test = location == "AUCA" | location == "RIST" | location == "PRCR",
                    yes = "Rural", no = ifelse(location == "BIVA" | location == "BOWA" | location == "DEMI",
                    yes = "Suburban", no = ifelse(location == "BACA" | location == "COFR" | location == "JOMA",
                    yes = "Urban", no = NA))))
         
        

## quick figures

individual_sums <- counted_moths %>% 
  group_by(location, doy) %>% 
  summarise(macro = sum(macroMoths), micro = sum(microMoths), total = sum(macroMoths)+ sum(microMoths))

indsum_June <- individual_sums %>% 
  filter(doy<= 175)

ggplot(indsum_June) + 
  geom_point(aes(x = doy, y = macro, color = location)) +
  geom_smooth(aes(x = doy, y = macro, color = location), se = FALSE)


urbanization_sums <- counted_moths %>% 
  group_by(ui, doy) %>% 
  summarise(macro = mean(macroMoths), micro = mean(microMoths), total = mean(macroMoths + microMoths))

ggplot(urbanization_sums) + 
  geom_smooth(aes(x = doy, y = macro, color = ui), se = FALSE) +
  geom_point(aes(x = doy, y = macro, color = ui)) 

# macro only 

mar_may_sum <- counted_moths %>% 
  filter(doy <= 175) %>% 
  group_by(location, doy) %>% 
  summarise(macro = mean(macroMoths), micro = mean(microMoths), total = mean(macroMoths + microMoths))

urb_mar_may_sum <- counted_moths %>% 
  filter(doy <= 175) %>% 
  group_by(ui, doy) %>% 
  summarise(macro = mean(macroMoths), micro = mean(microMoths), total = mean(macroMoths + microMoths))

ggplot() + 
  geom_text(data = mar_may_sum, aes(x = doy, y = macro, label = location)) +
  geom_smooth(data = urb_mar_may_sum, aes(x = doy, y = macro, color = ui), se = FALSE)
