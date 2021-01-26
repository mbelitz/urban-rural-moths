library(googlesheets4)
library(googledrive)
library(dplyr)
library(tools)

rename_photos <- function(drive_ls_path, moth_id_dataframe, Location){
  
  a <- drive_ls(path = drive_ls_path) 
  
  a <- a %>% 
    mutate(photo_number = ifelse(test = grepl(pattern = Location, name),
                                 yes = 0,
                                 no = NA))
  
  a <- a %>% 
    mutate(photo_number = ifelse(test = grepl(pattern = "DSC", name),
                                 yes = as.numeric(sub(".JPG*", "", sub("DSC", "", name))),
                                 no = photo_number))
  a <- a %>% 
    mutate(photo_number = ifelse(test = grepl(pattern = "DSCN", name),
                                 yes = as.numeric(sub(".JPG*", "", sub("DSCN", "", name))),
                                 no = photo_number))
  
  a <- a %>% 
    mutate(photo_number = ifelse(test = grepl(pattern = "IMG", name),
                                 yes = as.numeric(sub(".JPG*", "", sub("IMG_", "", name))),
                                 no = photo_number))
  
  a <- a %>% 
    mutate(photo_number = ifelse(test = is.na(photo_number),
                                 yes = tools::file_path_sans_ext(name),
                                 no = photo_number))
  b <- a %>% 
    dplyr::filter(photo_number != 0)
  
  moth_id_df <- moth_id_dataframe %>% 
    mutate(photoStart_number = as.numeric(sub(".JPG*", "", sub("DSCN", "", photoStart)))) %>% 
    mutate(photoEnd_number = as.numeric(sub(".JPG*", "", sub("DSCN", "", photoEnd)))) %>% 
    mutate(num_of_photos = photoEnd_number - photoStart_number + 1)
  
  moth_id_df <- moth_id_df %>% 
    mutate(photoStart_number = ifelse(test = is.na(photoStart_number),
                                      yes = as.numeric(sub(".JPG*", "", sub("IMG_", "", photoStart))),
                                      no = photoStart_number)) %>% 
    mutate(photoEnd_number = ifelse(test = is.na(photoEnd_number),
                                    yes = as.numeric(sub(".JPG*", "", sub("IMG_", "", photoEnd))),
                                    no = photoEnd_number)) %>% 
    mutate(num_of_photos = photoEnd_number - photoStart_number + 1)
  
  moth_id_df <- moth_id_df %>% 
    mutate(photoStart_number = ifelse(test = is.na(photoStart_number),
                                      yes = as.numeric(sub(".JPG*", "", sub("DSC", "", photoStart))),
                                      no = photoStart_number)) %>% 
    mutate(photoEnd_number = ifelse(test = is.na(photoEnd_number),
                                    yes = as.numeric(sub(".JPG*", "", sub("DSC", "", photoEnd))),
                                    no = photoEnd_number)) %>% 
    mutate(num_of_photos = photoEnd_number - photoStart_number + 1)
  
  
  new_moths <- moth_id_df %>% 
    dplyr::filter(is.na(Renamed))
  
  for(i in 1:nrow(b)){
    for(j in 1:nrow(new_moths)){  
      ifelse(b$photo_number[i] >= new_moths$photoStart_number[j] & 
               b$photo_number[i] <= new_moths$photoEnd_number[j], 
             drive_mv(file = as_id(a$id[i]), paste(new_moths$Location[j], 
                                                   new_moths$eventDate[j],
                                                   b$photo_number[i], sep = "_")),
             print("Skipping to next iteration"))
    }}
  
}