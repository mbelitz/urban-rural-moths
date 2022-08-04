library(dplyr)
library(stringr)

setwd("g:/UF4/MothID/Moth_ID/betterID_workingDir/")

#cofr files
cofr <- list.files(path = "COFR_AlreadyID", full.names = T, recursive = T)

cofrID <- data.frame(raw_filepath = cofr, 
                     numberOf = str_count(cofr, pattern = "/"))

cofrID <- cofrID %>% 
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

# now for cofr1 and cofr2
cofr1 <- list.files("COFR", full.names = T, recursive = T)

cofrID1 <- data.frame(raw_filepath = cofr1, 
                      numberOf = str_count(cofr1, pattern = "/"))

cofrID1 <- cofrID1 %>% 
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


# combine cofr together
cofrID1 <- select(cofrID1, -scientific_name) 

cofr_total <- rbind(cofrID, cofrID1) %>% 
  mutate(scientificName = paste(Genus, Species))

unique(cofr_total$scientificName)
