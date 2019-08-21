library(googledrive)
library(readr)
library(dplyr)

# script to remove duplicate photos

AUCA_photos <- drive_ls(path = "Moth Phenology/Identification Photos/AUCA")

AUCA_photos <- AUCA_photos %>% 
  dplyr::mutate(duplicate = duplicated(AUCA_photos$name))

only_duplicates <- AUCA_photos %>% 
  dplyr::filter(duplicate == TRUE)

for(i in 1:length(only_duplicates)){
  drive_trash(as_id(only_duplicates$id[i]))
}