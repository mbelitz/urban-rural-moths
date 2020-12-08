# load libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(mgcv)

# make pheno&biomass df with reweighed data

# read in Frass Measurements

frass <- read.csv("data/Frass_measurements_reweighed.csv") %>% 
  mutate(urban = case_when(Site == "AUCA" | Site == "RIST" | Site == "PRCR" ~ "Rural",
                           Site == "BACA" | Site == "JOMA" | Site == "COFR" ~ "Urban",
                           Site == "DEMI" | Site == "BIVA" | Site == "BOWA" ~ "Suburban")) 

frass <- frass %>% 
  mutate(doy = lubridate::yday(lubridate::mdy(Date)),
         year = year(mdy(Date)))

frass <- frass %>% 
  mutate(doy2 = ifelse(year == 2019,
                       yes = doy,
                       no = doy + 365))

frass_group <-  frass %>% 
  group_by(doy2, Site) %>% 
  summarise(avg_mass = mean(Mass)) %>% 
  mutate(urban = case_when(Site == "AUCA" | Site == "RIST" | Site == "PRCR" ~ "Rural",
                           Site == "BACA" | Site == "JOMA" | Site == "COFR" ~ "Urban",
                           Site == "DEMI" | Site == "BIVA" | Site == "BOWA" ~ "Suburban"))

ggplot(frass_group, mapping = aes(x = doy2, y = avg_mass, color = Site)) +
  geom_point(alpha = 0.2) + 
  geom_smooth(se = F, formula = y ~ s(x, bs = "cc"), method = "gam") + 
  theme_classic() +
  facet_wrap(~urban)


## make site specific gams

site_gam <- function(site){
  
  mdf <- filter(frass_group, Site == site)
  sgam <- gam(avg_mass ~ s(doy2, bs = "cc"), family = gaussian, gamma = 1, data = mdf)
  
  all_doys <-  data.frame(doy2 = 1:365)
  all_doys$avg_mass <-  predict(sgam, newdata = all_doys, 
                                type = 'response')
  
  return(all_doys)
}

#RURAL SITEs
auca_gam <- site_gam(site = "AUCA")
plot(auca_gam)

rist_gam <- site_gam(site = "RIST")
plot(rist_gam)

prcr_gam <- site_gam(site = "PRCR")
plot(prcr_gam)

#Suburban SItes
bowa_gam <- site_gam(site = "BOWA")
plot(bowa_gam)

biva_gam <- site_gam(site = "BIVA")
plot(biva_gam)

demi_gam <- site_gam(site = "DEMI")
plot(demi_gam)

# Urban sites
baca_gam <- site_gam(site = "BACA")
plot(baca_gam)

cofr_gam <- site_gam(site = "COFR")
plot(cofr_gam)

joma_gam <- site_gam(site = "JOMA")
plot(joma_gam)

## MAKE PHENOLOGY ESTIMATES
#' function to make phenoestimates from gam
pheno_fun <- function(gam_out){
  out <- gam_out %>% 
    mutate(
      cum_prob = cumsum(avg_mass),
      cum_perc = cum_prob / max(cum_prob),
      tenth = doy2[which.max(cum_perc >= 0.10)],
      fiftieth = doy2[which.max(cum_perc >= 0.50)],
      nintieth = doy2[which.max(cum_perc >= 0.90)]
    )
  
  return(out)}



frass_pheno_df <- data.frame(
  
  Site = c("AUCA", "RIST", "PRCR",
           "DEMI", "BOWA", "BIVA",
           "COFR", "JOMA", "BACA"),
  
  Class = c("Rural", "Rural", "Rural",
            "Suburban", "Suburban", "Suburban",
            "Urban", "Urban", "Urban"),
  
  tenth = c(pheno_fun(auca_gam)$tenth[1],
            pheno_fun(rist_gam)$tenth[1],
            pheno_fun(prcr_gam)$tenth[1],
            pheno_fun(demi_gam)$tenth[1],
            pheno_fun(bowa_gam)$tenth[1],
            pheno_fun(biva_gam)$tenth[1],
            pheno_fun(cofr_gam)$tenth[1],
            pheno_fun(joma_gam)$tenth[1],
            pheno_fun(baca_gam)$tenth[1]),
  
  fifty = c(pheno_fun(auca_gam)$fiftieth[1],
            pheno_fun(rist_gam)$fiftieth[1],
            pheno_fun(prcr_gam)$fiftieth[1],
            pheno_fun(demi_gam)$fiftieth[1],
            pheno_fun(bowa_gam)$fiftieth[1],
            pheno_fun(biva_gam)$fiftieth[1],
            pheno_fun(cofr_gam)$fiftieth[1],
            pheno_fun(joma_gam)$fiftieth[1],
            pheno_fun(baca_gam)$fiftieth[1]),
  
  ninty = c(pheno_fun(auca_gam)$nintieth[1],
            pheno_fun(rist_gam)$nintieth[1],
            pheno_fun(prcr_gam)$nintieth[1],
            pheno_fun(demi_gam)$nintieth[1],
            pheno_fun(bowa_gam)$nintieth[1],
            pheno_fun(biva_gam)$nintieth[1],
            pheno_fun(cofr_gam)$nintieth[1],
            pheno_fun(joma_gam)$nintieth[1],
            pheno_fun(baca_gam)$nintieth[1])
  
)

write.csv(frass_pheno_df, "outputs/frass_pheno_reweigh.csv", row.names = F)

## MAKE BIOMASS Estimates
frass_biomass_df <- data.frame(
  
  Site = c("AUCA", "RIST", "PRCR",
           "DEMI", "BOWA", "BIVA",
           "COFR", "JOMA", "BACA"),
  
  Class = c("Rural", "Rural", "Rural",
            "Suburban", "Suburban", "Suburban",
            "Urban", "Urban", "Urban"),
  
  max_biomass = c(top_n(auca_gam, n = 1)$avg_mass[1],
                  top_n(rist_gam, n = 1)$avg_mass[1],
                  top_n(prcr_gam, n = 1)$avg_mass[1],
                  top_n(demi_gam, n = 1)$avg_mass[1],
                  top_n(bowa_gam, n = 1)$avg_mass[1],
                  top_n(biva_gam, n = 1)$avg_mass[1],
                  top_n(cofr_gam, n = 1)$avg_mass[1],
                  top_n(joma_gam, n = 1)$avg_mass[1],
                  top_n(baca_gam, n = 1)$avg_mass[1])
  
)

write.csv(frass_biomass_df, "outputs/frass_biomass_reweigh.csv", row.names = F)
