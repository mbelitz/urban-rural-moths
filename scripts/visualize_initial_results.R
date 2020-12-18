# load libraries
library(googledrive)
library(googlesheets4)
library(dplyr)
library(ggplot2)

adults_counted <- drive_get("Adult Moth Datasheet")
counted_moths <- sheets_read(adults_counted)

counted_moths <- counted_moths %>% 
  mutate(doy = lubridate::yday(eventDate)) %>% 
  mutate(Site = ifelse(test = location == "AUCA" | location == "RIST" | location == "PRCR",
                    yes = "Rural", no = ifelse(location == "BIVA" | location == "BOWA" | location == "DEMI",
                    yes = "Suburban", no = ifelse(location == "BACA" | location == "COFR" | location == "JOMA",
                    yes = "Urban", no = NA))))
         
        

## quick figures

individual_sums <- counted_moths %>% 
  group_by(location, doy) %>% 
  summarise(macro = sum(macroMoths), micro = sum(microMoths), total = sum(macroMoths)+ sum(microMoths))

indsum_June <- individual_sums

ggplot(indsum_June) + 
  geom_point(aes(x = doy, y = macro, color = location)) +
  geom_smooth(aes(x = doy, y = macro, color = location), se = FALSE)

indsum_June <- indsum_June %>% 
  mutate(Class = case_when(location == "AUCA" | location == "RIST" | location == "PRCR" ~ "Rural",
                         location == "BACA" | location == "JOMA" | location == "COFR" ~ "Urban",
                         location == "DEMI" | location == "BIVA" | location == "BOWA" ~ "Suburban"))

ggplot(indsum_June, mapping = aes(x = doy, y = macro, color = location)) +
  geom_smooth(se = F, formula = y ~ s(x), method = "gam") + 
  theme_classic() +
  facet_wrap(~ Class)


indsum_Aug <- indsum_June %>% 
  filter(doy <= 243)

ggplot(indsum_Aug, mapping = aes(x = doy, y = macro, color = location)) +
  geom_smooth(se = T, formula = y ~ s(x), method = "gam", mapping = aes(fill = location)) + 
  theme_classic() +
  facet_wrap(~ Class)

ggsave(filename = "outputs/adults.png")


urbanization_sums <- counted_moths %>% 
  group_by(Site, doy) %>% 
  summarise(macro = mean(macroMoths), micro = mean(microMoths), total = mean(macroMoths + microMoths))

ggplot(urbanization_sums) + 
  geom_smooth(aes(x = doy, y = macro, color = Site), se = FALSE) +
  geom_point(aes(x = doy, y = macro, color = Site)) 

# macro only 

mar_may_sum <- counted_moths %>% 
  filter(doy <= 175) %>% 
  group_by(location, doy) %>% 
  summarise(macro = mean(macroMoths), micro = mean(microMoths), total = mean(macroMoths + microMoths))

urb_mar_may_sum <- counted_moths %>% 
  filter(doy <= 171) %>% 
  group_by(Site, doy) %>% 
  summarise(macro = mean(macroMoths), micro = mean(microMoths), total = mean(macroMoths + microMoths))

ggplot() + 
  geom_point(data = urb_mar_may_sum, aes(x = doy, y = macro, color = Site)) +
  geom_smooth(data = urb_mar_may_sum, aes(x = doy, y = macro, color = Site)
              ,method = "loess", se = FALSE)

empirical <- ggplot(urbanization_sums) + 
  geom_smooth(aes(x = doy, y = macro, color = Site), size = 1.25, se = FALSE) +
  geom_point(aes(x = doy, y = macro, color = Site)) + 
  labs(x = "Day of year", y = "Mean Macro-moths") +
  scale_y_continuous(expand = c(0,0), limits = c(0,35)) +
  ggtitle("Observed") + 
  scale_color_viridis_d() +
  theme_grey() +
  theme(legend.position = "right")

empirical

