# call in function
source(file = "scripts/rename_photos_function.R") # Funciton takes two parameters

### Three parameters needed for rename_photos function ###
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

# Rename BIVA Photos

moth_id <- gs_title(x = "Moth Identification")
df <- gs_read(moth_id)

BIVA <- df %>% 
  dplyr::filter(Location == "BIVA")

rename_photos(drive_ls_path = "Moth Phenology/Identification Photos/BIVA",
              moth_id_dataframe = BIVA, Location = "BIVA")

# Rename BOWA Photos

moth_id <- gs_title(x = "Moth Identification")
df <- gs_read(moth_id)

BOWA <- df %>% 
  dplyr::filter(Location == "BOWA")

rename_photos(drive_ls_path = "Moth Phenology/Identification Photos/BOWA",
              moth_id_dataframe = BOWA, Location = "BOWA")

# Rename DEMI Photos

moth_id <- gs_title(x = "Moth Identification")
df <- gs_read(moth_id)

DEMI <- df %>% 
  dplyr::filter(Location == "DEMI")

rename_photos(drive_ls_path = "Moth Phenology/Identification Photos/DEMI",
              moth_id_dataframe = DEMI, Location = "DEMI")

# Rename COFR Photos

moth_id <- gs_title(x = "Moth Identification")
df <- gs_read(moth_id)

COFR <- df %>% 
  dplyr::filter(Location == "COFR")

rename_photos(drive_ls_path = "Moth Phenology/Identification Photos/COFR",
              moth_id_dataframe = COFR, Location = "COFR")

# Rename JOMA Photos

moth_id <- gs_title(x = "Moth Identification")
df <- gs_read(moth_id)

JOMA <- df %>% 
  dplyr::filter(Location == "JOMA")

rename_photos(drive_ls_path = "Moth Phenology/Identification Photos/JOMA",
              moth_id_dataframe = JOMA, Location = "JOMA")
