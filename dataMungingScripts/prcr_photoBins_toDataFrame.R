library(dplyr)
library(stringr)

setwd("g:/UF4/MothID/Moth_ID/betterID_workingDir/")

#prcr files
prcr <- list.files(path = "PRCR_AlreadyID", full.names = T, recursive = T)

prcrID <- data.frame(raw_filepath = prcr, 
                     numberOf = str_count(prcr, pattern = "/"))

prcrID <- prcrID %>% 
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

# now for prcr1 and prcr2
prcr1 <- list.files("PRCR1", full.names = T, recursive = T)

prcrID1 <- data.frame(raw_filepath = prcr1, 
                      numberOf = str_count(prcr1, pattern = "/"))

prcrID1 <- prcrID1 %>% 
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


prcr2 <- list.files("PRCR2", full.names = T, recursive = T)

prcrID2 <- data.frame(raw_filepath = prcr2, 
                      numberOf = str_count(prcr2, pattern = "/"))

prcrID2 <- prcrID2 %>% 
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
# combine prcr together
prcrID1 <- select(prcrID1, -scientific_name) 
prcrID2 <- select(prcrID2, -scientific_name)

prcr_total <- rbind(prcrID, prcrID1, prcrID2) %>% 
  mutate(scientificName = paste(Genus, Species))

unique(prcr_total$scientificName)
