library(googlesheets4)
library(googledrive)
library(dplyr)
library(ggplot2)

moths <-drive_get("Adult Moth Datasheet")
df <- sheets_read(moths)

total_counts <- df %>% 
  group_by(countedBy) %>% 
  summarise(count = sum(macroMoths))

ggplot() + 
  geom_bar(data = total_counts, aes(countedBy, y = count), stat = "identity")


micro_counts <- df %>% 
  group_by(countedBy) %>% 
  summarise(count = sum(microMoths))

ggplot() + 
  geom_bar(data =micro_counts, aes(countedBy, y = count), stat = "identity")

