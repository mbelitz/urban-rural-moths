library(googlesheets)
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
  
  for(i in 1:nrow(a)){
    for(j in 1:nrow(moth_id_df)){  
      ifelse(a$photo_number[i] >= moth_id_df$photoStart_number[j] & 
               a$photo_number[i] <= moth_id_df$photoEnd_number[j], 
             drive_mv(file = as_id(a$id[i]), paste(moth_id_df$Location[j], 
                                                   moth_id_df$eventDate[j],
                                                   a$photo_number[i], sep = "_")),
             print("Skipping to next iteration"))
    }}
  
}
