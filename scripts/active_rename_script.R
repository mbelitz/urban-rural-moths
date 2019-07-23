# call in function
source(file = "scripts/rename_photos_function.R") # Funciton takes two parameters

# 1. drive_ls_path
# 2. moth_id_dataframe
# 3. Location where moth was sampled -- must be in ""

# Rename RIST Photos

moth_id <- gs_title(x = "Moth Identification")
df <- gs_read(moth_id)

RIST <- df %>% 
  dplyr::filter(Location == "RIST")

rename_photos(drive_ls_path = "Moth Phenology/Identification Photos/RIST",
              moth_id_dataframe = RIST, Location = "RIST")


## RENAME BACA PHOTOS

moth_id <- gs_title(x = "Moth Identification")
df <- gs_read(moth_id)

BACA <- df %>% 
  dplyr::filter(Location == "BACA")

rename_photos(drive_ls_path = "Moth Phenology/Identification Photos/BACA",
              moth_id_dataframe = BACA, Location = "BACA")

## RENAME PRCR Photos

moth_id <- gs_title(x = "Moth Identification")
df <- gs_read(moth_id)

PRCR <- df %>% 
  dplyr::filter(Location == "PRCR")

rename_photos(drive_ls_path = "Moth Phenology/Identification Photos/PRCR",
              moth_id_dataframe = PRCR, Location = "PRCR")

