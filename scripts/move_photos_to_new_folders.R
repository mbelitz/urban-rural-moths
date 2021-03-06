library(googledrive)

# Function to move Moth Photos in Google Drive

move_photos <- function(id_folder, site_folder, date, photo_start_num, photo_end_num){
  folder_path <- as_dribble(paste("Moth Phenology/Identification Photos", site_folder, id_folder, sep = "/"))
  
  
  for(i in photo_start_num:photo_end_num){
    drive_mv(file = paste(site_folder, date, i, sep = "_"), path = folder_path)
    
  }
}

# Examples

# move_photos(id_folder = "Erebidae", site_folder = "RIST", 
#            date = "2019-04-07", photo_start_num = 1424, photo_end_num = 1427)



# move_photos(id_folder = "Notodontidae", site_folder = "AUCA",
#            date = "2019-03-16", photo_start_num = 6971, photo_end_num = 6979)

# move_photos(id_folder = "Erebidae", site_folder = "PRCR",
#            date = "2019-06-15", photo_start_num = 7268, photo_end_num = 7269)


