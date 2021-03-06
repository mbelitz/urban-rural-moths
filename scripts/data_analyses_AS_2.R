library(tidyverse)

# Script to analyze frass data

# Goal is to run an ANOVAs and LM to see if there are differences of 
#biomass and phenology across an urbanization gradient

# read in data

pheno_data <- read.csv("data_products/frass_pheno_reweigh.csv")

# check to see if the urbanization levels are in the correct order
levels(pheno_data$Class) # not a factor so reorder

pheno_data$Class <- ordered(pheno_data$Class, levels = c("Urban",
                                                         "Suburban",
                                                         "Rural"))

# First we will do this for the tenth percentile

#Let's visualize the data first
ggplot() + 
  geom_boxplot(pheno_data, mapping = aes(x = Class, y = tenth))

tenth.anova <- aov(tenth ~ Class, data = pheno_data)
summary(tenth.anova)
TukeyHSD(tenth.anova)

ggplot() + 
  geom_boxplot(pheno_data, mapping = aes(x = Class, y = fifty))

fifty.anova <- aov(fifty ~ Class, data = pheno_data)
summary(fifty.anova)
TukeyHSD(fifty.anova)

ggplot() + 
  geom_boxplot(pheno_data, mapping = aes(x = Class, y = ninty))

ninty.anova <- aov(ninty ~ Class, data = pheno_data)
summary(ninty.anova)
TukeyHSD(ninty.anova)

# Now let's add in the Urbanization gradient from GIS DATA
urb_data <- read.csv("data_products/urbanization_gradient.csv")
pheno_data_urb <- left_join(pheno_data, urb_data, by = "Site")

ggplot(pheno_data_urb, aes(x = Dev_10, y = tenth)) + 
  geom_point(mapping = aes(color = Class)) + 
  geom_smooth(mapping = aes(), method = 'lm')

tenth.lm <- lm(formula = tenth ~ Dev_10, data = pheno_data_urb)
summary(tenth.lm)  

ggplot(pheno_data_urb, aes(x = Dev_10, y = fifty)) + 
  geom_point(mapping = aes(color = Class)) + 
  geom_smooth(mapping = aes(), method = 'lm')

fifty.lm <- lm(formula = fifty ~ Dev_10, data = pheno_data_urb)
summary(fifty.lm)  

ggplot(pheno_data_urb, aes(x = Dev_10, y = ninty)) + 
  geom_point(mapping = aes(color = Class)) + 
  geom_smooth(mapping = aes(), method = 'lm')

ninty.lm <- lm(formula = ninty ~ Dev_10, data = pheno_data_urb)
summary(ninty.lm)  

## Try to see if you can do this on your own for the fiftieth and nintieth percentiles
## FOr both annovas and lm 
## What do these results mean?



## Also try the whole process for biomass (you'll have to read in new data)

