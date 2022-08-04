library(dplyr)
library(stringr)

setwd("g:/UF4/MothID/Moth_ID/betterID_workingDir/")

#AUCA files
auca <- list.files(path = "AUCA_AlreadyID", full.names = T, recursive = T)

aucaID <- data.frame(raw_filepath = auca, 
                     numberOf = str_count(auca, pattern = "/"))

aucaID <- aucaID %>% 
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

# now for auca1 and auca2
auca1 <- list.files("AUCA1/", full.names = T, recursive = T)

aucaID1 <- data.frame(raw_filepath = auca1, 
                      numberOf = str_count(auca1, pattern = "/"))

aucaID1 <- aucaID1 %>% 
  mutate(Family = stringr::word(string = raw_filepath, 
                                start = 4, end = 4,sep = "/")) %>% 
  mutate(scientific_name= stringr::word(string = raw_filepath, 
                                        start = 5, end = 5,sep = "/")) %>%
  mutate(Genus = stringr::word(string = scientific_name, 
                               start = 1, end = 1,sep = "_"),
         Species  = stringr::word(string = scientific_name, 
                                  start = 2, end = 2,sep = "_")) %>% 
  mutate(imageName = stringr::word(string = raw_filepath, 
                                   start = 6, end = 6,sep = "/"))


auca2 <- list.files("AUCA2/", full.names = T, recursive = T)

aucaID2 <- data.frame(raw_filepath = auca2, 
                      numberOf = str_count(auca2, pattern = "/"))

aucaID2 <- aucaID2 %>% 
  mutate(Family = stringr::word(string = raw_filepath, 
                                start = 4, end = 4,sep = "/")) %>% 
  mutate(scientific_name= stringr::word(string = raw_filepath, 
                                        start = 5, end = 5,sep = "/")) %>%
  mutate(Genus = stringr::word(string = scientific_name, 
                               start = 1, end = 1,sep = "_"),
         Species  = stringr::word(string = scientific_name, 
                                  start = 2, end = 2,sep = "_")) %>% 
  mutate(imageName = stringr::word(string = raw_filepath, 
                                   start = 6, end = 6,sep = "/"))

# combine AUCA together
aucaID1 <- select(aucaID1, -scientific_name) 
aucaID2 <- select(aucaID2, -scientific_name)

auca_total <- rbind(aucaID, aucaID1, aucaID2) %>% 
  mutate(scientificName = paste(Genus, Species))

unique(auca_total$scientificName)
