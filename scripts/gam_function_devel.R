library(dplyr)
library(ggplot2)
library(lubridate)
library(rbms)
library(mgcv)
library(stringr)

# read in adult dataset
moth_df <- read.csv("data_products/adultDataSet_validNames.csv") %>% 
  distinct(id, .keep_all = T) %>% 
  mutate(year = year(mdy(eventDate))) %>% 
  mutate(doy = if_else(
    condition = year == 2019,
    true = yday(mdy(eventDate)),
    false = 365 + yday(mdy(eventDate))
  ))

# how many species do we have enough data to estimate gams for?
enoughSites <- moth_df %>% 
  group_by(validName) %>% 
  summarise(totalObs = n(), nSites = length(unique(Site))) %>% 
  filter(nSites >= 2, totalObs >= 9)

# find validNames with NAs in them
enoughSites <- enoughSites %>% 
  mutate(hasNA = str_detect(validName, "NA")) 
enoughSites <- enoughSites %>% 
  filter(hasNA == F)

#filter initial dataset to these species
moth_df_filter <- moth_df %>% 
  filter(validName %in% enoughSites$validName)

# now get an idea of how many observations are occurring per Site per species
enoughObs <- moth_df_filter %>% 
  group_by(validName, Site) %>% 
  summarise(nObs = n()) 

# join enough obs to filtered moth df
moth_df_filter <- left_join(moth_df_filter, enoughObs)

# do any species need to be removed because they no longer have 2 sites
enoughSites2 <- enoughObs %>% 
  group_by(validName) %>% 
  summarise(nSites = length(unique(Site))) %>% 
  filter(nSites >= 2)

#filter to enoughSites2
moth_df_filter <- moth_df_filter %>% 
  filter(validName %in% enoughSites2$validName) %>% 
  mutate(eventDate = lubridate::mdy(eventDate))

# what days had surveys for each site
surveyDates <- read.csv("data_products/surveyDateSheet.csv") %>% 
  select(eventDate, location)

surveyDates$eventDate <- lubridate::as_date(surveyDates$eventDate)
surveyDates <- surveyDates %>% 
  rename(Site = location)
surveyDates <- surveyDates %>% 
  mutate(Site = case_when(Site == "BACA" ~ "Baca",
                             Site == "JOMA" ~ "Joma",
                             Site == "AUCA" ~ "Auca",
                             Site == "BIVA" ~ "Biva",
                             Site == "DEMI" ~ "Demi",
                             Site == "COFR" ~ "Cofr",
                             Site == "RIST" ~ "Rist",
                             Site == "PRCR" ~ "Prcr",
                             Site == "BOWA" ~ "Bowa"))

# write a funciton to draw predicted gams for each species X site combinations
gam_function <- function(x){
  
  sdf <- moth_df_filter %>% 
    filter(validName == x) 
  
  # now we need to get the dates with sampling effort but no observations
  # but first we get a count by date
  sdf <- sdf %>% 
    group_by(Site, eventDate) %>% 
    summarise(count = n()) %>% 
    mutate(validName = x)
  
  sdf_j <- left_join(surveyDates, sdf)
  
  sdf_j <- sdf_j %>% 
    mutate(count = if_else(
      is.na(count), 
      true = 0, 
      false = as.double(count))) %>% 
    mutate(validName = x)
  
  sdf_j <- sdf_j %>% 
    mutate(year = year(eventDate)) %>% 
    mutate(doy = if_else(
      condition = year == 2019,
      true = yday(eventDate),
      false = 365 + yday(eventDate)
    ))
  
  unique_sites <- unique(sdf_j$Site)
  
  # make site specific GAMS
  baca_gam <- gam(count ~ s(doy, k = 12, bs = "cr"), 
                  data = filter(sdf_j, Site == "Baca"))
  baca_points <- predict.gam(baca_gam, 
                            newdata=data.frame(doy=unique(sdf_j$doy)), type="response", se=F)
  baca_plot <- ggplot() +
    geom_point(data = filter(sdf_j, Site == "Baca"), aes(x = doy, y = count)) +
    geom_line(aes(x = unique(sdf_j$doy), y = baca_points)) +
    labs(x = "DOY", y = "Abundance") +
    ggtitle("Baca")
  
  #joma
  joma_gam <- gam(count ~ s(doy, k = 12, bs = "cr"), 
                  data = filter(sdf_j, Site == "Joma"))
  joma_points <- predict.gam(joma_gam, 
                             newdata=data.frame(doy=unique(sdf_j$doy)), type="response", se=F)
  joma_plot <- ggplot() +
    geom_point(data = filter(sdf_j, Site == "Joma"), aes(x = doy, y = count)) +
    geom_line(aes(x = unique(sdf_j$doy), y = joma_points)) +
    labs(x = "DOY", y = "Abundance") +
    ggtitle("Joma")
  
  #cofr
  cofr_gam <- gam(count ~ s(doy, k = 12, bs = "cr"), 
                  data = filter(sdf_j, Site == "Cofr"))
  cofr_points <- predict.gam(gam1, 
                             newdata=data.frame(doy=unique(sdf_j$doy)), type="response", se=F)
  cofr_plot <- ggplot() +
    geom_line(aes(x = unique(sdf_j$doy), y = cofr_points)) +
    labs(x = "DOY", y = "Abundance") +
    ggtitle("Cofr")
  
  #BIVA
  biva_gam <- gam(count ~ s(doy, k = 12, bs = "cr"), 
                  data = filter(sdf_j, Site == "Biva"))
  biva_points <- predict.gam(gam1, 
                             newdata=data.frame(doy=unique(sdf_j$doy)), type="response", se=F)
  biva_plot <- ggplot() +
    geom_line(aes(x = unique(sdf_j$doy), y = biva_points)) +
    labs(x = "DOY", y = "Abundance") +
    ggtitle("Biva")
  
  #bowa
  bowa_gam <- gam(count ~ s(doy, k = 12, bs = "cr"), 
                  data = filter(sdf_j, Site == "Bowa"))
  bowa_points <- predict.gam(gam1, 
                             newdata=data.frame(doy=unique(sdf_j$doy)), type="response", se=F)
  bowa_plot <- ggplot() +
    geom_line(aes(x = unique(sdf_j$doy), y = bowa_points)) +
    labs(x = "DOY", y = "Abundance") +
    ggtitle("Bowa")
  
  #demi
  demi_gam <- gam(count ~ s(doy, k = 12, bs = "cr"), 
                  data = filter(sdf_j, Site == "Demi"))
  demi_points <- predict.gam(gam1, 
                             newdata=data.frame(doy=unique(sdf_j$doy)), type="response", se=F)
  demi_plot <- ggplot() +
    geom_line(aes(x = unique(sdf_j$doy), y = demi_points)) +
    labs(x = "DOY", y = "Abundance") +
    ggtitle("Demi")
  
  #Rist
  rist_gam <- gam(count ~ s(doy, k = 12, bs = "cr"), 
                  data = filter(sdf_j, Site == "Rist"))
  rist_points <- predict.gam(gam1, 
                             newdata=data.frame(doy=unique(sdf_j$doy)), type="response", se=F)
  rist_plot <- ggplot() +
    geom_line(aes(x = unique(sdf_j$doy), y = rist_points)) +
    labs(x = "DOY", y = "Abundance") +
    ggtitle("Rist")
  
  #Prcr
  prcr_gam <- gam(count ~ s(doy, k = 12, bs = "cr"), 
                  data = filter(sdf_j, Site == "Prcr"))
  prcr_points <- predict.gam(gam1, 
                             newdata=data.frame(doy=unique(sdf_j$doy)), type="response", se=F)
  prcr_plot <- ggplot() +
    geom_line(aes(x = unique(sdf_j$doy), y = prcr_points)) +
    labs(x = "DOY", y = "Abundance") +
    ggtitle("Prcr")
  
  #Auca
  auca_gam <- gam(count ~ s(doy, k = 12, bs = "cr"), 
                  data = filter(sdf_j, Site == "Auca"))
  auca_points <- predict.gam(gam1, 
                             newdata=data.frame(doy=unique(sdf_j$doy)), type="response", se=F)
  auca_plot <- ggplot() +
    geom_line(aes(x = unique(sdf_j$doy), y = auca_points)) +
    labs(x = "DOY", y = "Abundance") +
    ggtitle("Auca")
    
  
  cp <- cowplot::plot_grid(baca_plot, joma_plot, cofr_plot,
                           biva_plot, bowa_plot, demi_plot,
                           rist_plot, auca_plot, prcr_plot)
    
    
    
    }
  
  
  
  gam_m <- gam(COUNT ~ s(doy, k=12, bs="cr"), data = filter(ng_j, SITE_ID == "Rist"))
  spar <- gam_m$sp
  gam_points <- predict.gam(gam_m, newdata=data.frame(doy=unique(ng_j$doy)), type="response", se=F)
  plot(unique(ng_j$doy), gam_points)
  
  
  
  
  
}


# get moth_df to rbms datastyle -- let's do this for one species
ng <- moth_df %>% 
  filter(validName == "Nadata gibbosa") %>% 
  mutate(SITE_ID = Site,
         DATE = lubridate::mdy(eventDate),
         SPECIES = validName
  ) %>% 
  select(SITE_ID, DATE, SPECIES)

# now we need to get the dates with sampling effort but no observations
# but first we get a count by date
ng <- ng %>% 
  group_by(SITE_ID, DATE) %>% 
  summarise(COUNT = n()) %>% 
  mutate(SPECIES = ng$SPECIES[1])

ng_j <- left_join(surveyDates, ng)

ng_j <- ng_j %>% 
  mutate(COUNT = if_else(
    is.na(COUNT), true = 0, false = as.double(COUNT)
  ),
  SPECIES = "Nadata gibbosa")

ng_j <- ng_j %>% 
  mutate(year = year(DATE)) %>% 
  mutate(doy = if_else(
    condition = year == 2019,
    true = yday(DATE),
    false = 365 + yday(DATE)
  ))


## i don't think i love the regional gam. let's try the stemkovski way
gam_m <- gam(COUNT ~ s(doy, k=12, bs="cr"), data = filter(ng_j, SITE_ID == "Rist"))
spar <- gam_m$sp
gam_points <- predict.gam(gam_m, newdata=data.frame(doy=unique(ng_j$doy)), type="response", se=F)
plot(unique(ng_j$doy), gam_points)

ggplot(ng_j, aes(x = doy, y = COUNT)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cr", k = 12)) +
  facet_wrap(~SITE_ID)
