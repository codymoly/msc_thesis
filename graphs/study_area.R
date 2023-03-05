# AUSTRALIAN STATES

# clean memory
rm(list=ls())

# set working directory
setwd("~/Documents/MSc_thesis/Figures")

# read study sites
coords_30 = readr::read_delim("/media/mari/Crucial X8/coords_30km.csv", delim = ",")
min(coords_30$latitude) # -43.45
max(coords_30$latitude) # -9.89
min(coords_30$longitude) # 113.18
max(coords_30$longitude) # 167.96

# libraries
library(sf) # '1.0.9'
library(ozmaps) # ‘0.4.5’
library(ggsn) # ‘0.5.0’

sf_oz <- ozmap_data("states", xlim=c(110,180), ylim=c(-50,-5), mar=c(0,0,0,0))
                               
# chatGPT
ggplot(sf_oz, aes(fill = NAME)) +
  geom_sf() +
  geom_rect(aes(xmin = 112.6, xmax = 167.96, ymin = -8.8, ymax = -44, linetype = "-9.88°N, -43.46°S, \n167.97°E, 113.17°W"),
            color = "gray25",
            fill = NA,
            size = 0.7,
            show.legend = TRUE) +
  coord_sf(crs = "+proj=lonlat +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") +
  xlim(c(110,170)) +
  ylim(c(-45,-8)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.spacing.y = unit(0.5, 'cm'),
        legend.text = element_text(size = 14, face= "bold"),
        legend.title = element_text(size = 14, face= "bold"),
        legend.box.background = element_rect(color = "white"),
        axis.title.x = element_text(size = 18, face= "bold"),
        axis.title.y = element_text(size = 18, face= "bold"),
        axis.text.x = element_text(size = 14, face= "bold"),
        axis.text.y = element_text(size = 14, face= "bold"),
        axis.ticks.length=unit(.25, "cm"),
        axis.line = element_line(linewidth = 0.8)) +
  scale_fill_manual(name = "States",
                    values = c("white", "#D55E00", "#F0E442", "#000000", "#009E73", "#0072B2","#56B4E9", "#CC79A7", "#E69F00")) +
  scale_linetype_manual(name = "Study area",
                        values = "dashed") +
  ggsn::scalebar(sf_oz, dist_unit = "km", 
                 dist = 100, st.size=5, 
                 height=0.04, model = 'WGS84', transform = TRUE)


