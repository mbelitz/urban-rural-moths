library(dplyr)
library(stringr)

setwd("g:/UF4/MothID/Moth_ID/betterID_workingDir/")

## on to BACA
baca <- list.files(path = "BACA_AlreadyID", full.names = T, recursive = T)

bacaID <- data.frame(raw_filepath = baca, 
                     numberOf = str_count(baca, pattern = "/"))

bacaID <- bacaID %>% 
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

baca_total <- bacaID %>% 
  mutate(scientificName = paste(Genus, Species))

unique(baca_total$scientificName)
