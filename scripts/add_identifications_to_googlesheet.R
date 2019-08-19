library(googledrive)
library(googlesheets)
library(readr)
library(dplyr)

# Read in Google Sheet to R
moth_id <- gs_title(x = "Moth Identification")
df <- gs_read(moth_id)

AUCA_dirs <- drive_ls("Moth Phenology/Identification Photos/AUCA", type = "folder")

AUCA_Notodontidae_files <- drive_ls("Moth Phenology/Identification Photos/AUCA/Notodontidae")

df_parsenum <- df %>% 
  mutate(photoStartNum = parse_number(photoStart),
         photoEndNum = parse_number(photoEnd))

df_parsenum$photoStartNum = as.numeric(df_parsenum$photoStartNum)
df_parsenum$photoEndNum = as.numeric(df_parsenum$photoEndNum)

AUCA_Notodontidae_files <- AUCA_Notodontidae_files %>% 
  mutate(photoNum = as.numeric(substring(AUCA_Notodontidae_files$name, first = 17))) %>% 
  mutate(Family = "Notodontidae")

# Write new df

for(i in 1:length(AUCA_Notodontidae_files)){
  testdf <- df_parsenum %>% 
    mutate(Family = ifelse(AUCA_Notodontidae_files$photoNum[i] >= photoStartNum & 
                         AUCA_Notodontidae_files$photoNum[i] <= photoEndNum, 
                         yes = AUCA_Notodontidae_files$Family[i],
                         no = Family))
  
}

testdf <- mutate(df_parsenum, test = photoStartNum <= AUCA_Notodontidae_files$photoNum & 
        photoEndNum >= AUCA_Notodontidae_files$photoNum)

which(AUCA_Notodontidae_files$photoNum %in% 
      
      >= df_parsenum$photoStartNum & 
  AUCA_Notodontidae_files$photoNum <= df_parsenum$photoEndNum)
