library(googlesheets)
library(googledrive)

a <- gs_ls()

moth_id <- gs_title(x = "Moth Identification")

moth_id_df <- gs_read(moth_id)

rist_test <- moth_id_df %>% 
  dplyr::filter(Location == "RIST")

auca_test <- moth_id_df %>% 
  dplyr::filter(Location == "AUCA")

rist_test_baby_photo <- rist_test_baby$photoStart
photonames <- paste(rist_test_baby_photo, ".JPG", sep = "")

rename <- paste(rist_test_baby$Location, rist_test_baby$eventDate, sep = "")
number_of_photos <- as.numeric(gsub(pattern = "DSCN", replacement = "", rist_test_baby$photoEnd)) - 
  as.numeric(gsub(pattern = "DSCN", replacement = "", rist_test_baby$photoStart)) + 1

rename2 <- paste(rename, rist_test_baby$macroCount, number_of_photos, sep = "_")
rename2

file <- drive_mv(bJPG, rename2)
file <- as_dribble("Moth Phenology/Identification Photos/Copy of Copy of 26494738.JPG")
file

file <- drive_mv("Moth Phenology/Identification Photos/AUCA/Copy of Copy of 26494738.JPG", rename2)

a <- as_dribble("Moth Phenology/Identification Photos/RIST/")
a <- as_dribble("Moth Phenology/Identification Photos/RIST/DSCN5974.JPG")
a

dscn_count <- as.numeric(gsub(pattern = "DSCN", replacement = "", rist_test$photoEnd[1])) - 
  as.numeric(gsub(pattern = "DSCN", replacement = "", rist_test$photoStart[1])) + 1
dscn_count

photos <- vector()
for(i in 1:dscn_count) {
  dscn_num <- as.numeric(gsub(pattern = "DSCN", replacement = "", rist_test$photoStart[1])) - 1
  photos[i] <- paste("DSCN", dscn_num + i, ".JPG", sep = "")
  
}
photos


photo_list <- c()
for(i in 1:nrow(rist_test_baby)){
  number_of_photos <- as.numeric(gsub(pattern = "DSCN", replacement = "", rist_test_baby$photoEnd)) - 
    as.numeric(gsub(pattern = "DSCN", replacement = "", rist_test_baby$photoStart)) + 1
  
  photo_vec <- c()
  m <- matrix()
  for(j in 1:number_of_photos[i]) {
    dscn_num <- as.numeric(gsub(pattern = "DSCN", replacement = "", rist_test_baby$photoStart[i])) - 1
    photo_vec[j] <- paste("DSCN", dscn_num + j, ".JPG", sep = "")
  }
  photo_list[[i]] <- photo_vec
}
photo_vec
photo_list

# try to do this within lists!
lapplyfun <- function(x){as.numeric(sub(".JPG*","", sub("DSCN", "", x)))}
photo_list_numeric <- lapply(photo_list, lapplyfun)
photo_list_numeric

rist_test_baby <- rist_test_baby %>% 
  dplyr::mutate(photoStartNumeric = as.numeric(gsub(pattern = "DSCN", replacement = "", rist_test_baby$photoStart))) %>% 
  dplyr::mutate(photoEndNumeric = as.numeric(gsub(pattern = "DSCN", replacement = "", rist_test_baby$photoEnd)))

photo_list_numeric

for(i in nrow(rist_test_baby)){
  if(x)
}

# now let's rename

photo_unlist <- unlist(photo_list)
photo_unlist
photo_unlist_numeric <- as.numeric(sub(".JPG*","", sub("DSCN", "", photo_unlist)))
photo_unlist_numeric
rename2
photo_start_numeric <- as.numeric(gsub(pattern = "DSCN", replacement = "", rist_test_baby$photoStart))
photo_end_numeric <- as.numeric(gsub(pattern = "DSCN", replacement = "", rist_test_baby$photoEnd))

for(i in 1:length(photo_unlist_numeric)){
  for(j in 1:nrow(rist_test_baby)){
    for(k in 1:length(number_of_photos)){
      if(photo_unlist_numeric[i] >= photo_start_numeric[j] & photo_unlist_numeric[i] <=photo_end_numeric[j]){
      drive_mv(file = paste("Moth Phenology/Identification Photos/", paste(rist_test_baby$Location[j],"/",sep = ""),
                            photo_unlist[i], sep = ""),
              paste(rename[i], rist_test_baby$macroCount[j], k, sep = "_"))
    } else
      print("NADA")
      
  }
}}
  

rename2 <- paste(rename, rist_test_baby$macroCount, number_of_photos, sep = "_")
photo_unlist_numeric %>% dplyr::between(photo_start_numeric[1], photo_end_numeric[1], )
