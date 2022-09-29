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

surveyDates$eventDate <- lubridate::mdy(surveyDates$eventDate)
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

# don't forget lunar data
lunar.phase <- read.csv("data_products/lunarIllumination.csv") %>% 
  mutate(Date = mdy(Date))
surveyDates <- left_join(surveyDates, lunar.phase, by = c("eventDate" = "Date"))

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
  
  
  # make site specific GAMS
  bd <- filter(sdf_j, Site == "Baca")
  if(length(unique(filter(bd,count>0)$doy)) >= 3){
    
    baca_gam <- gam(count ~ s(doy, k = 12, bs = "cr") + s(lunar.phase, k = 3, bs = "cr"), 
                    data = bd)
    baca_points <- predict.gam(baca_gam, 
                               newdata=data.frame(doy=unique(sdf_j$doy),
                                                  lunar.phase = rep(0.5, length(unique(sdf_j$doy)))), 
                               type="response", se=F)
    baca_points <- ifelse(baca_points < 0, 0, baca_points)
    baca_plot <- ggplot() +
      geom_point(data = filter(sdf_j, Site == "Baca"), aes(x = doy, y = count)) +
      geom_line(aes(x = unique(sdf_j$doy), y = baca_points)) +
      labs(x = "DOY", y = "Abundance") +
      ggtitle("Baca") +
      theme_bw()
    
    baca_df <- data.frame(doy = unique(sdf_j$doy), abund = baca_points, Site = "Baca")
    write.csv(x = baca_df, file = paste0("gamOuputsCSV/", "GAM_Baca_", bw, ".csv"), row.names = F)
    
    
  } else{
    baca_plot <- ggplot() +
      ggtitle("Baca")+
      theme_void()
    
  }
  
  #joma
  jd <- filter(sdf_j, Site == "Joma")
  if(length(unique(filter(jd,count>0)$doy)) >= 3){
    
    joma_gam <- gam(count ~ s(doy, k = 12, bs = "cr") + s(lunar.phase, k = 3, bs = "cr"), 
                    data = jd)
    joma_points <- predict.gam(joma_gam, 
                               newdata=data.frame(doy=unique(sdf_j$doy),
                                                  lunar.phase = rep(0.5, length(unique(sdf_j$doy)))),
                               type="response", se=F)
    joma_points <- ifelse(joma_points < 0, 0, joma_points)
    
    joma_plot <- ggplot() +
      geom_point(data = filter(sdf_j, Site == "Joma"), aes(x = doy, y = count)) +
      geom_line(aes(x = unique(sdf_j$doy), y = joma_points)) +
      labs(x = "DOY", y = "Abundance") +
      ggtitle("Joma") +
      theme_bw()
    
    joma_df <- data.frame(doy = unique(sdf_j$doy), abund = joma_points, Site = "Joma")
    write.csv(x = joma_df, file = paste0("gamOuputsCSV/", "GAM_Joma_", bw, ".csv"), row.names = F)
    
  } else{
    joma_plot <- ggplot() +
      ggtitle("Joma")+
      theme_void()
    
  }
  
  #cofr
  cd <- filter(sdf_j, Site == "Cofr")
  if(length(unique(filter(cd,count>0)$doy))){
  
    cofr_gam <- gam(count ~ s(doy, k = 12, bs = "cr") + s(lunar.phase, k = 3, bs = "cr"), 
                  data = filter(sdf_j, Site == "Cofr"))
    cofr_points <- predict.gam(cofr_gam, 
                             newdata=data.frame(doy=unique(sdf_j$doy),
                             lunar.phase = rep(0.5, length(unique(sdf_j$doy)))), 
    type="response", se=F)
    cofr_points <- ifelse(cofr_points < 0, 0, cofr_points)
    cofr_plot <- ggplot() +
      geom_point(data = filter(sdf_j, Site == "Cofr"), aes(x = doy, y = count)) +
      geom_line(aes(x = unique(sdf_j$doy), y = cofr_points)) +
      labs(x = "DOY", y = "Abundance") +
      theme_bw() +
      ggtitle("Cofr") 
    
    cofr_df <- data.frame(doy = unique(sdf_j$doy), abund = cofr_points, Site = "Cofr")
    write.csv(x = cofr_df, file = paste0("gamOuputsCSV/", "GAM_Cofr_", bw, ".csv"), row.names = F)
    
    } else{
        
        cofr_plot <- ggplot() +
          ggtitle("Cofr")+
          theme_void()
        
      }
  
  #BIVA
  bid <- filter(sdf_j, Site == "Biva")
  if(length(unique(filter(bid,count>0)$doy))){
    
    biva_gam <- gam(count ~ s(doy, k = 12, bs = "cr") + s(lunar.phase, k = 3, bs = "cr"), 
                  data = filter(sdf_j, Site == "Biva"))
    biva_points <- predict.gam(biva_gam, 
                             newdata=data.frame(doy=unique(sdf_j$doy),
                                                lunar.phase = rep(0.5, length(unique(sdf_j$doy)))), 
                             type="response", se=F)
    biva_points <- ifelse(biva_points < 0, 0, biva_points)
    biva_plot <- ggplot() +
      geom_point(data = filter(sdf_j, Site == "Biva"), aes(x = doy, y = count)) +
      geom_line(aes(x = unique(sdf_j$doy), y = biva_points)) +
      labs(x = "DOY", y = "Abundance") +
      theme_bw() +
      ggtitle("Biva") 
    
    biva_df <- data.frame(doy = unique(sdf_j$doy), abund = biva_points, Site = "Biva")
    write.csv(x = biva_df, file = paste0("gamOuputsCSV/", "GAM_Biva_", bw, ".csv"), row.names = F)
    
    } else{
      
      biva_plot <- ggplot() +
        ggtitle("Biva")+
        theme_void()
      
      
    }
  
  #bowa
  bod <- filter(sdf_j, Site == "Bowa")
  if(length(unique(filter(bod,count>0)$doy))){
    
    bowa_gam <- gam(count ~ s(doy, k = 12, bs = "cr") + s(lunar.phase, k = 3, bs = "cr"), 
                  data = filter(sdf_j, Site == "Bowa"))
    bowa_points <- predict.gam(bowa_gam, 
                             newdata=data.frame(doy=unique(sdf_j$doy),
                                                lunar.phase = rep(0.5, length(unique(sdf_j$doy)))), 
                             type="response", se=F)
    bowa_points <- ifelse(bowa_points < 0, 0, bowa_points)
    
    bowa_plot <- ggplot() +
      geom_point(data = filter(sdf_j, Site == "Bowa"), aes(x = doy, y = count)) +
      geom_line(aes(x = unique(sdf_j$doy), y = bowa_points)) +
      labs(x = "DOY", y = "Abundance") +
      theme_bw() +
      ggtitle("Bowa") 
    
    bowa_df <- data.frame(doy = unique(sdf_j$doy), abund = bowa_points, Site = "Bowa")
    write.csv(x = bowa_df, file = paste0("gamOuputsCSV/", "GAM_Bowa_", bw, ".csv"), row.names = F)
    
    } else{
        
        bowa_plot <- ggplot() +
          ggtitle("Bowa")+
          theme_void()
      
      }
  
  #demi
  dod <- filter(sdf_j, Site == "Demi")
  if(length(unique(filter(dod,count>0)$doy))){
  demi_gam <- gam(count ~ s(doy, k = 12, bs = "cr") + s(lunar.phase, k = 3, bs = "cr"), 
                  data = filter(sdf_j, Site == "Demi"))
  demi_points <- predict.gam(demi_gam, 
                             newdata=data.frame(doy=unique(sdf_j$doy),
                                                lunar.phase = rep(0.5, length(unique(sdf_j$doy)))), 
                             type="response", se=F)
  demi_points <- ifelse(demi_points < 0, 0, demi_points)
  demi_plot <- ggplot() +
    geom_point(data = filter(sdf_j, Site == "Demi"), aes(x = doy, y = count)) +
    geom_line(aes(x = unique(sdf_j$doy), y = demi_points)) +
    labs(x = "DOY", y = "Abundance") +
    theme_bw() +
    ggtitle("Demi") 
  
  demi_df <- data.frame(doy = unique(sdf_j$doy), abund = demi_points, Site = "Demi")
  write.csv(x = demi_df, file = paste0("gamOuputsCSV/", "GAM_Demi_", bw, ".csv"), row.names = F)
  
  } else{
      
      demi_plot <- ggplot() +
        ggtitle("Demi")+
        theme_void()
    }
  
  #Rist
  rid <- filter(sdf_j, Site == "Rist")
  if(length(unique(filter(rid,count>0)$doy))){
    rist_gam <- gam(count ~ s(doy, k = 12, bs = "cr") + s(lunar.phase, k = 3, bs = "cr"), 
                  data = filter(sdf_j, Site == "Rist"))
    rist_points <- predict.gam(rist_gam, 
                             newdata=data.frame(doy=unique(sdf_j$doy),
                                                lunar.phase = rep(0.5, length(unique(sdf_j$doy)))), 
                             type="response", se=F)
    rist_points <- ifelse(rist_points < 0, 0, rist_points)
    rist_plot <- ggplot() +
      geom_point(data = filter(sdf_j, Site == "Rist"), aes(x = doy, y = count)) +
      geom_line(aes(x = unique(sdf_j$doy), y = rist_points)) +
      labs(x = "DOY", y = "Abundance") +
      theme_bw() +
      ggtitle("Rist") 
    
    rist_df <- data.frame(doy = unique(sdf_j$doy), abund = rist_points, Site = "Rist")
    write.csv(x = rist_df, file = paste0("gamOuputsCSV/", "GAM_Rist_", bw, ".csv"), row.names = F)
    
    } else{
        
        rist_plot <- ggplot() +
          ggtitle("Rist")+
          theme_void()
        
      }
  
  #Prcr
  prd <- filter(sdf_j, Site == "Prcr")
  if(length(unique(filter(prd,count>0)$doy))){
    prcr_gam <- gam(count ~ s(doy, k = 12, bs = "cr") + s(lunar.phase, k = 3, bs = "cr"), 
                  data = filter(sdf_j, Site == "Prcr"))
    prcr_points <- predict.gam(prcr_gam, 
                             newdata=data.frame(doy=unique(sdf_j$doy),
                                                lunar.phase = rep(0.5, length(unique(sdf_j$doy)))), 
                             type="response", se=F)
    prcr_points <- ifelse(prcr_points < 0, 0, prcr_points)
    prcr_plot <- ggplot() +
      geom_point(data = filter(sdf_j, Site == "Prcr"), aes(x = doy, y = count)) +
      geom_line(aes(x = unique(sdf_j$doy), y = prcr_points)) +
      labs(x = "DOY", y = "Abundance") +
      theme_bw() +
      ggtitle("Prcr") 
    
    prcr_df <- data.frame(doy = unique(sdf_j$doy), abund = prcr_points, Site = "Prcr")
    write.csv(x = prcr_df, file = paste0("gamOuputsCSV/", "GAM_Prcr_", bw, ".csv"), row.names = F)
    
    } else{ 
      
      prcr_plot <- ggplot() +
        ggtitle("Prcr")+
        theme_void()
      
      }
  
  #Auca
  aud <- filter(sdf_j, Site == "Auca")
  if(length(unique(filter(aud,count>0)$doy))){
  auca_gam <- gam(count ~ s(doy, k = 12, bs = "cr") + s(lunar.phase, k = 3, bs = "cr"), 
                  data = filter(sdf_j, Site == "Auca"))
  auca_points <- predict.gam(auca_gam, 
                             newdata=data.frame(doy=unique(sdf_j$doy),
                                                lunar.phase = rep(0.5, length(unique(sdf_j$doy)))), 
                             type="response", se=F)
  auca_points <- ifelse(auca_points < 0, 0, auca_points)
  auca_plot <- ggplot() +
    geom_point(data = filter(sdf_j, Site == "Auca"), aes(x = doy, y = count)) +
    geom_line(aes(x = unique(sdf_j$doy), y = auca_points)) +
    labs(x = "DOY", y = "Abundance") +
    theme_bw() +
    ggtitle("Auca") 
  
  auca_df <- data.frame(doy = unique(sdf_j$doy), abund = auca_points, Site = "Auca")
  write.csv(x = auca_df, file = paste0("gamOuputsCSV/", "GAM_Auca_", bw, ".csv"), row.names = F)
  
  } else{
      
      auca_plot <- ggplot() +
        ggtitle("Prcr")+
        theme_void()
      
    }
    
  
  cp <- cowplot::plot_grid(baca_plot, joma_plot, cofr_plot,
                           biva_plot, bowa_plot, demi_plot,
                           rist_plot, auca_plot, prcr_plot)
    
    
  bw <- stringr::str_replace(x, " ", "_")
  ggsave(filename = paste0("gamOutputs_lunar.phaseIllumination/", bw, ".png"), plot = cp, 
         width = 7, height = 7)
    
  
}

lapply(X = "Nadata gibbosa", FUN = gam_function)

spp_list <- unique(enoughSites2$validName)
lapply(X = spp_list, FUN = gam_function)
  