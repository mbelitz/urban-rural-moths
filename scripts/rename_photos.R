library(googlesheets)
library(googledrive)
library(dplyr)

source("scripts/rename_photos_function.R")

a <- drive_ls(path = "Test Code")

moth_id <- gs_title(x = "Moth Identification")
moth_id_df <- gs_read(moth_id)

cofr <- moth_id_df %>% 
  dplyr::filter(Location == "COFR")

rename_photos(drive_ls_path = "Moth Phenology/Identification Photos/COFR",
              moth_id_dataframe = cofr, Location = "COFR")


rist <- moth_id_df %>% 
  dplyr::filter(Location == "RIST")

rename_photos(drive_ls_path = "Moth Phenology/Identification Photos/RIST",
              moth_id_dataframe = rist, Location = "RIST")
