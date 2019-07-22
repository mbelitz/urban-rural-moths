library(googlesheets)
library(googledrive)
library(dplyr)

a <- drive_ls(path = "Test Code")

moth_id <- gs_title(x = "Moth Identification")
moth_id_df <- gs_read(moth_id)

rist_test <- moth_id_df %>% 
  dplyr::filter(Location == "RIST")

rist_test_mini <- rist_test[1:3,]

a <- a %>% 
  mutate(photo_number = as.numeric(sub(".JPG*", "", sub("DSCN", "", name))))

rist_test_mini <- rist_test_mini %>% 
  mutate(photoStart_number = as.numeric(sub(".JPG*", "", sub("DSCN", "", photoStart)))) %>% 
  mutate(photoEnd_number = as.numeric(sub(".JPG*", "", sub("DSCN", "", photoEnd)))) %>% 
  mutate(num_of_photos = photoEnd_number - photoStart_number + 1)

for(i in 1:nrow(a)){
  for(j in 1:nrow(rist_test_mini)){  
    ifelse(a$photo_number[i] >= rist_test_mini$photoStart_number[j] & 
             a$photo_number[i] <= rist_test_mini$photoEnd_number[j], 
           drive_mv(file = as_id(a$id[i]), paste(rist_test_mini$Location[j], 
                                                 rist_test_mini$eventDate[j],
                                                 a$photo_number[i], sep = "_")),
           print("Skipping to next iteration"))
    
  }}
