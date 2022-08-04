library(dplyr)
library(stringr)

setwd("g:/UF4/MothID/Moth_ID/betterID_workingDir/")

#BIVA files
biva <- list.files(path = "BIVA_AlreadyID", full.names = T, recursive = T)

bivaID <- data.frame(raw_filepath = biva, 
                     numberOf = str_count(biva, pattern = "/"))

bivaID <- bivaID %>% 
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

# now for biva1 and biva2
biva1 <- list.files("BIVA", full.names = T, recursive = T)

bivaID1 <- data.frame(raw_filepath = biva1, 
                      numberOf = str_count(biva1, pattern = "/"))

bivaID1 <- bivaID1 %>% 
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


# combine together
bivaID1 <- select(bivaID1, -scientific_name) 

biva_total <- rbind(bivaID, bivaID1) %>% 
  mutate(scientificName = paste(Genus, Species))

unique(biva_total$scientificName)
