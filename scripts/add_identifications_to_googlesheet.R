library(googledrive)
library(googlesheets)
library(readr)
library(dplyr)

# Read in Google Sheet to R
moth_id <- gs_title(x = "Raw Moth Photo Numbers")
df <- gs_read(moth_id)

AUCA_dirs <- drive_ls("Moth Phenology/Identification Photos/AUCA", type = "folder")
AUCA_Notodontidae_files <- drive_ls("Moth Phenology/Identification Photos/AUCA/Notodontidae")

df_parsenum <- df %>% 
  mutate(photoStartNum = parse_number(photoStart),
         photoEndNum = parse_number(photoEnd))

df_parsenum$photoStartNum = as.numeric(df_parsenum$photoStartNum)
df_parsenum$photoEndNum = as.numeric(df_parsenum$photoEndNum)

df_parsenum <- df_parsenum %>% 
  mutate(name = paste(Location, eventDate, photoStartNum, sep = "_"))

AUCA_Notodontidae_files <- AUCA_Notodontidae_files %>% 
  mutate(photoNum = as.numeric(substring(AUCA_Notodontidae_files$name, first = 17))) %>% 
  mutate(Family = "Notodontidae")

AUCA_Notodontidae_join <- left_join(AUCA_Notodontidae_files, df_parsenum, by = "name")

AUCA_Notodontidae_unique <- dplyr::filter(AUCA_Notodontidae_join, !is.na(photoStart)) %>% 
  dplyr::select(-name, -id, -drive_resource, - photoNum)

df_id <- left_join(df, AUCA_Notodontidae_unique)

write.csv(df_id, "outputs/id_dataframe.csv")

gs_upload(file = "outputs/id_dataframe.csv", sheet_title = "Identified Moth Photos", overwrite = TRUE)

drive_mv(file = "Identified Moth Photos", path = "~/Moth Phenology/")
