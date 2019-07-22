# call in function
source(file = "scripts/rename_photos_function.R") # Funciton takes two parameters

# 1. drive_ls_path
# 2. moth_id_dataframe

# Rename RIST Photos

moth_id <- gs_title(x = "Moth Identification")
df <- gs_read(moth_id)

rist <- df %>% 
  dplyr::filter(Location == "RIST")

rename_photos(drive_ls_path = "Moth Phenology/Identification Photos/RIST",
              moth_id_dataframe = rist)

