library(googledrive)

# Function to move Moth Photos in Google Drive

move_photos <- function(id_folder, site_folder, date, photo_start_num, photo_end_num){
  folder_path <- as_dribble(paste("Moth Phenology/Identification Photos", site_folder, id_folder, sep = "/"))
  
  
  for(i in photo_start_num:photo_end_num){
    drive_mv(file = paste(site_folder, date, i, sep = "_"), path = folder_path)
    
  }
}

# Examples

move_photos(id_folder = "Erebidae", site_folder = "RIST", 
            date = "2019-04-07", photo_start_num = 1424, photo_end_num = 1427)






# Use this part when moving photos to Notodontidae folder

for(i in 6833:6834){
  drive_mv(file = paste("AUCA_2019-03-16_", i, sep = ""), path = erebidae)
  
}

erebidae <- as_dribble("Moth Phenology/Identification Photos/RIST/Erebidae")



