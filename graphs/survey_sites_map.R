setwd("~/Downloads")

library(maps)
library(mapdata)
library(sp)

aus = maps::map("worldHires", "Australia", fill=TRUE, xlim=c(110,160),
         ylim=c(-45,-5), mar=c(0,0,0,0))

library(tidyverse) # load first the other lips and execute map() before tidyverse
coords_30 = readr::read_delim("/media/mari/Crucial X8/coords_30km.csv", delim = ",")

rls_dist = rls %>% 
  select(latitude, longitude, survey_date) %>% 
  distinct() #%>% 
  #filter(survey_date > "2019-12-31")

### australia map
ggplot(fortify(aus), aes(y = lat, x = long, group = group)) + 
  geom_polygon(alpha = 0.5) +
  geom_point(data = coords_30,
             aes(x = longitude, y = latitude),
             inherit.aes = FALSE,
             color = "black", # blue: #1A85FF, pink: #D41159
             size = 3) +
  theme_minimal() + # theme_bw() +
  theme(
    axis.title.x = element_text(size = 22, face= "bold"),
    axis.title.y = element_text(size = 22, face= "bold"),
    axis.text = element_text(size = 16, face= "bold"),
    axis.title = element_blank(),
    #axis.text.x = element_blank(),
    #axis.text.y = element_blank(),
    #axis.ticks = element_blank(),
    #axis.line = element_blank()
  ) +
  xlab("Longitude") +
  ylab("Latitude") 

