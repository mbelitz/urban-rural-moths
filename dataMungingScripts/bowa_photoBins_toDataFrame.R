library(dplyr)
library(stringr)

setwd("g:/UF4/MothID/Moth_ID/betterID_workingDir/")

#bowa files
bowa <- list.files(path = "BOWA_AlreadyID", full.names = T, recursive = T)

bowaID <- data.frame(raw_filepath = bowa, 
                     numberOf = str_count(bowa, pattern = "/"))

bowaID <- bowaID %>% 
  mutate(Family = stringr::word(string = raw_filepath, 
                                start = 2, end = 2,sep = "/")) %>% 
  mutate(Genus = case_when(
    numberOf < 4 ~ NA_character_,
    numberOf >= 4 ~ stringr::word(string = raw_filepath, start = 4, end = 4,sep = "/"))) %>% 
  mutate(Species = case_when(
    numberOf < 5 ~ NA_character_,
    numberOf >=5 ~ stringr::word(string = raw_filepath, start = 5, end = 5,sep = "/"))
  ) %>% 
  mutate(imageName = case_when(
    numberOf == 2 ~ stringr::word(string = raw_filepath, start = 3, end = 3,sep = "/"),
    numberOf == 3 ~ stringr::word(string = raw_filepath, start = 4, end = 4,sep = "/"),
    numberOf == 4 ~ stringr::word(string = raw_filepath, start = 5, end = 5,sep = "/"),
    numberOf == 5 ~ stringr::word(string = raw_filepath, start = 6, end = 6,sep = "/"),
    TRUE ~ NA_character_
  )
  )

# now for bowa1 and bowa2
bowa1 <- list.files("BOWA", full.names = T, recursive = T)

bowaID1 <- data.frame(raw_filepath = bowa1, 
                      numberOf = str_count(bowa1, pattern = "/"))

bowaID1 <- bowaID1 %>% 
  mutate(Family = stringr::word(string = raw_filepath, 
                                start = 2, end = 2,sep = "/")) %>% 
  mutate(scientific_name= stringr::word(string = raw_filepath, 
                                        start = 3, end = 3,sep = "/")) %>%
  mutate(Genus = stringr::word(string = scientific_name, 
                               start = 1, end = 1,sep = "_"),
         Species  = stringr::word(string = scientific_name, 
                                  start = 2, end = 2,sep = "_")) %>% 
  mutate(imageName = stringr::word(string = raw_filepath, 
                                   start = 4, end = 4,sep = "/"))





# combine bowa together
bowaID1 <- select(bowaID1, -scientific_name) 

bowa_total <- rbind(bowaID, bowaID1) %>% 
  mutate(scientificName = paste(Genus, Species))

unique(bowa_total$scientificName)
