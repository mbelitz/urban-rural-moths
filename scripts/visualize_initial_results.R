library(googlesheets)
library(dplyr)
library(ggplot2)

adults_counted <- gs_title(x = "Adult Moth Datasheet")
counted_moths <- gs_read(adults_counted)

counted_moths <- counted_moths %>% 
  mutate(doy = lubridate::yday(eventDate)) %>% 
  mutate(ui = ifelse(test = location == "AUCA" | location == "RIST" | location == "AUCA",
                    yes = "Rural", no = ifelse(location == "BIVA" | location == "BOWA" | location == "DEMI",
                    yes = "Suburban", no = "Urban")))
         
        

## quick figures

individual_sums <- counted_moths %>% 
  group_by(location, doy) %>% 
  summarise(macro = sum(macroMoths), micro = sum(microMoths), total = sum(macroMoths)+ sum(microMoths))

ggplot(individual_sums) + 
  geom_line(aes(x = doy, y = total, color = location))


urbanization_sums <- counted_moths %>% 
  group_by(ui, doy) %>% 
  summarise(macro = mean(macroMoths), micro = mean(microMoths), total = mean(macroMoths + microMoths))

ggplot(urbanization_sums) + 
  geom_line(aes(x = doy, y = total, color = ui))
