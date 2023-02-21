setwd("~/Documents/MSc_thesis/Figures")

library(maps)
library(mapdata)
library(sp)

aus = maps::map("worldHires", "Australia", fill=TRUE, xlim=c(110,160),
         ylim=c(-45,-5), mar=c(0,0,0,0))

library(tidyverse) # load first the other lips and execute map() before tidyverse
coords_30 = readr::read_delim("/media/mari/Crucial X8/coords_30km.csv", delim = ",")

### australia map
ggplot(fortify(aus), aes(y = lat, x = long, group = group)) + 
  geom_polygon(alpha = 0.5) +
  geom_point(data = coords_30,
             aes(x = longitude, y = latitude),
             inherit.aes = FALSE,
             color = "#1A85FF", # blue: #1A85FF, pink: #D41159
             size = 3) +
  theme_classic() + # theme_bw() +
  theme(
    axis.title.x = element_text(size = 22, face= "bold"),
    axis.title.y = element_text(size = 22, face= "bold"),
    axis.text = element_text(size = 16, face= "bold"),
    axis.ticks.length=unit(.25, "cm"),
    axis.line = element_line(linewidth = 0.8)
    #axis.text.x = element_blank(),
    #axis.text.y = element_blank(),
    #axis.ticks = element_blank(),
    #axis.line = element_blank()
  ) +
  xlab("Longitude") +
  ylab("Latitude") 


library(sf)
library(maps)
library(mapdata)

aussi_2 = st_as_sf(map("worldHires", "Australia", fill=TRUE, xlim=c(110,160), ylim=c(-45,-5), mar=c(0,0,0,0)))

ggplot(data = aussi_2) + 
  geom_sf() + 
  geom_point(data = coords_30, aes(x = longitude, y = latitude), size = 3) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_classic() +
  theme(legend.position = c(.9,.85),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.text = element_text(size = 12, face= "bold"),
        legend.title = element_text(size = 12, face= "bold"),
        axis.title.x = element_text(size = 14, face= "bold"),
        axis.title.y = element_text(size = 14, face= "bold"),
        axis.text.x = element_text(size = 10, face= "bold"),
        axis.text.y = element_text(size = 10, face= "bold"),
        axis.ticks.length=unit(.25, "cm"),
        axis.line = element_line(linewidth = 0.8)) +
  scale_colour_viridis(name = "Body size CWM")

