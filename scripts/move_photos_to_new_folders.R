library(googledrive)

#Use this when moving photos to Erebidae folder 
erebidae <- as_dribble("Moth Phenology/Identification Photos/AUCA/Erebidae")

drive_mv(file = "AUCA_2019-04-21_6599", path = erebidae)

# Use this part when moving photos to Notodontidae folder



for(i in 6833:6834){
  drive_mv(file = paste("AUCA_2019-03-16_", i, sep = ""), path = erebidae)
  
}



