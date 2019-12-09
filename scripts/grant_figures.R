library(truncnorm)
library(ggplot2)
library(lubridate)
library(dplyr)

nn <- 20000
urban <- c(rtruncnorm(nn * (3/5), a=0, b=365, mean=118, sd=30),
               rtruncnorm(nn * (2/5), a=0, b=365, mean=244, sd=30))

urban_sample <- sample(urban, size = 20000, replace = FALSE)

urban_df <- data.frame(x = urban_sample) %>% 
  mutate(jd_wk = 7 *floor(x/7))

urban_sum_df <- urban_df %>% 
  group_by(jd_wk) %>% 
  summarise(count = n())%>% 
  mutate(Site = "urban")

nn <- 30000
suburban <- c(rtruncnorm(nn * (3/5), a=0, b=365, mean=125, sd=35),
           rtruncnorm(nn * (2/5), a=0, b=365, mean=250, sd=35))

suburban_sample <- sample(suburban, size = 30000, replace = FALSE)

suburban_df <- data.frame(x = suburban_sample) %>% 
  mutate(jd_wk = 7 *floor(x/7))

suburban_sum_df <- suburban_df %>% 
  group_by(jd_wk) %>% 
  summarise(count = n())%>% 
  mutate(Site = "suburban")


nn <- 40000
rural <- c(rtruncnorm(nn * (3/5), a=0, b=365, mean=130, sd=40),
              rtruncnorm(nn * (2/5), a=0, b=365, mean=258, sd=40))

rural_sample <- sample(rural, size = 40000, replace = FALSE)

rural_df <- data.frame(x = rural_sample) %>% 
  mutate(jd_wk = 7 *floor(x/7))

rural_sum_df <- rural_df %>% 
  group_by(jd_wk) %>% 
  summarise(count = n()) %>% 
  mutate(Site = "rural")

total_sum_df <- rbind(rural_sum_df, urban_sum_df, suburban_sum_df) 


theoretical <- ggplot(total_sum_df) +
  geom_line(mapping = aes(x = jd_wk, y = count, color = Site), size = 1.25) +
  labs(x = "Day of year", y = "Relative Abundance") +
  scale_y_continuous(expand = c(0,0), limits = c(0, max(total_sum_df$count + 20))) +
  scale_color_viridis_d() +
  ggtitle("Predicted") +
  theme_grey() +
  theme(axis.text.y = element_blank(), legend.position = "none")

library(gridExtra)
library(grid)

lay <- rbind(c(1,1,1),
             c(2,2,3))
grid.arrange(theoretical, empirical, layout_matrix = lay)

g <- arrangeGrob(theoretical, empirical, layout_matrix = lay)

ggsave("outputs/MM_figure.png", g, dpi = 300, height = 6, width = 7)

