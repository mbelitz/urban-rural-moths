library(dplyr)
library(stringr)

setwd("g:/UF4/MothID/Moth_ID/betterID_workingDir/")

#joma files
joma <- list.files(path = "JOMA_AlreadyID", full.names = T, recursive = T)

jomaID <- data.frame(raw_filepath = joma, 
                     numberOf = str_count(joma, pattern = "/"))

jomaID <- jomaID %>% 
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

# now for joma1 and joma2
joma1 <- list.files("JOMA", full.names = T, recursive = T)

jomaID1 <- data.frame(raw_filepath = joma1, 
                      numberOf = str_count(joma1, pattern = "/"))

jomaID1 <- jomaID1 %>% 
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


# combine joma together
jomaID1 <- select(jomaID1, -scientific_name) 

joma_total <- rbind(jomaID, jomaID1) %>% 
  mutate(scientificName = paste(Genus, Species))

unique(joma_total$scientificName)
