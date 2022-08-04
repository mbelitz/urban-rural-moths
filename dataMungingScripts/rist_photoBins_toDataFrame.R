library(dplyr)
library(stringr)

setwd("g:/UF4/MothID/Moth_ID/betterID_workingDir/")

#rist files
rist <- list.files(path = "RIST_AlreadyID", full.names = T, recursive = T)

ristID <- data.frame(raw_filepath = rist, 
                     numberOf = str_count(rist, pattern = "/"))

ristID <- ristID %>% 
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

# now for rist1 and rist2
rist1 <- list.files("RIST1", full.names = T, recursive = T)

ristID1 <- data.frame(raw_filepath = rist1, 
                      numberOf = str_count(rist1, pattern = "/"))

ristID1 <- ristID1 %>% 
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


rist2 <- list.files("RIST2", full.names = T, recursive = T)

ristID2 <- data.frame(raw_filepath = rist2, 
                      numberOf = str_count(rist2, pattern = "/"))

ristID2 <- ristID2 %>% 
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
# combine rist together
ristID1 <- select(ristID1, -scientific_name) 
ristID2 <- select(ristID2, -scientific_name)

rist_total <- rbind(ristID, ristID1, ristID2) %>% 
  mutate(scientificName = paste(Genus, Species))

unique(rist_total$scientificName)
