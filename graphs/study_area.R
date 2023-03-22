# PLOT MAPS WITH STUDY AREA, AUSTRALIAN STATES, AND SURVEY SITES

# clean memory
rm(list=ls())

# set working directory
setwd("~/Documents/MSc_thesis/Figures")

# read study sites
coords_30 = readr::read_delim("/media/mari/Crucial X8/coords_30km.csv", delim = ",")

# libraries
library(sf) # '1.0.9'
library(ozmaps) # ‘0.4.5’
library(ggsn) # ‘0.5.0’

# this is the original area
# -43.53
# -9.88
# 113.17
# 167.99

# get states of Australia
sf_oz = ozmap_data("states", xlim=c(110,170), ylim=c(-45,-8), mar=c(0,0,0,0))
sf_oz_nostates = ozmap_data("country", xlim=c(110,170), ylim=c(-45,-8), mar=c(0,0,0,0))
                               
# map with coloured states and the study area
ggplot(sf_oz, aes(fill = NAME)) +
  theme_bw() +
  geom_sf(colour = "gray25", linewidth = 0.7) +
  geom_rect(aes(xmin = 112.5, xmax = 167.99, ymin = -8.8, ymax = -44, linetype = "-9.88°N, -43.53°S, \n167.99°E, 113.17°W"),
            color = "gray25",
            fill = NA,
            size = 0.7,
            show.legend = TRUE) +
  coord_sf(crs = "+proj=lonlat +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") +
  xlim(c(112,168.5)) +
  ylim(c(-44.5,-8.4)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(panel.grid = element_line(colour = "gray70", linetype = "solid"),
        panel.border = element_rect(colour = "gray30", linewidth = 1),
        legend.spacing.y = unit(0.5, 'cm'),
        legend.text = element_text(size = 16, face= "bold"),
        legend.title = element_text(size = 16, face= "bold"),
        legend.box.background = element_rect(color = "white"),
        axis.title.x = element_text(size = 18, face= "bold"),
        axis.title.y = element_text(size = 18, face= "bold"),
        axis.text.x = element_text(size = 14, face= "bold"),
        axis.text.y = element_text(size = 14, face= "bold"),
        axis.ticks.length=unit(.25, "cm")) +
  scale_fill_manual(name = "States",
                    values = c("white", "#D55E00", "#F0E442", "gray20", "#009E73", "#0072B2","#56B4E9", "#CC79A7", "#E69F00")) +
  scale_linetype_manual(name = "Study area",
                        values = "dashed") +
  ggsn::scalebar(sf_oz, dist_unit = "km", 
                 dist = 400, # units of scale bar
                 st.size = 5, # general size
                 height = 0.04, # height of the scale bar relative to y-axis
                 model = 'WGS84', 
                 transform = TRUE, # input as dec deg
                 location = "bottomright", # proximate location
                 anchor = c(x = 164, y = -41)) # percise position

# map with final survey sites
ggplot(sf_oz) +
  theme_bw() +
  geom_sf(colour = "gray25", linewidth = 0.6, fill = "gray50") +
  coord_sf(crs = "+proj=lonlat +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") +
  #theme_dark() +
  geom_point(data = coords_30,
             aes(x = longitude, y = latitude, fill = "#1A85FF"),
             pch=21,
             size = 3) +
  xlim(c(112,168.5)) +
  ylim(c(-44.5,-8.4)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(panel.grid = element_line(colour = "gray70", linetype = "dashed"),
        panel.border = element_rect(colour = "gray30", linewidth = 1),
        legend.position = c(.8,.925),
        legend.title = element_blank(),
        legend.key=element_blank(),
        legend.box.margin = margin(1, 1, 6, 1),
        legend.text = element_text(size = 16, face= "bold"),
        legend.background = element_rect(fill = "white"),
        legend.box.background = element_rect(colour = "black", linewidth = 0.6),
        axis.title.x = element_text(size = 20, face= "bold"),
        axis.title.y = element_text(size = 20, face= "bold"),
        axis.text.x = element_text(size = 16, face= "bold"),
        axis.text.y = element_text(size = 16, face= "bold"),
        axis.ticks.length=unit(.25, "cm")) +
  scale_fill_manual(labels = "RLS & ATRC survey sites \nwith pairwise distance ≥ 30km", 
                      values = "steelblue1") +
  ggsn::scalebar(sf_oz, dist_unit = "km", 
                 dist = 400, # units of scale bar
                 st.size = 5, # general size
                 height = 0.04, # height of the scale bar relative to y-axis
                 model = 'WGS84', 
                 transform = TRUE, # input as dec deg
                 location = "bottomright", # proximate location
                 anchor = c(x = 164, y = -41)) # percise position


# map with coloured states together with the study area and sites
ggplot(sf_oz) +
  theme_bw() +
  geom_sf(aes(fill = NAME), colour = "gray25", linewidth = 0.6) +
  geom_rect(aes(xmin = 112.5, xmax = 168.2, ymin = -8.8, ymax = -44, linetype = "-9.88°N, -43.53°S, \n167.99°E, 113.17°W"),
            color = "gray25",
            fill = NA,
            linewidth = 0.6,
            show.legend = TRUE) +
  geom_point(data = coords_30, 
             aes(x = longitude, y = latitude, color = "black"),
             size = 2
             ) +
  geom_point(data = coords_30, 
             aes(x = longitude, y = latitude),
             size = 2, pch = 21
             ) +
  coord_sf(crs = "+proj=lonlat +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") +
  xlim(c(112,168.5)) +
  ylim(c(-44.5,-8.4)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(panel.grid = element_line(colour = "white", linetype = "solid"),
        panel.border = element_rect(colour = "gray30", linewidth = 1),
        legend.spacing.y = unit(0.5, 'cm'),
        legend.text = element_text(size = 14, face= "bold"),
        legend.title = element_text(size = 14, face= "bold"),
        legend.box.background = element_rect(color = "white"),
        axis.title.x = element_text(size = 14, face= "bold"),
        axis.title.y = element_text(size = 14, face= "bold"),
        axis.text.x = element_text(size = 12, face= "bold"),
        axis.text.y = element_text(size = 12, face= "bold"),
        axis.ticks.length=unit(.25, "cm")) +
  scale_fill_manual(name = "States",
                    values = c("white", "#F3E855", "#E69F00", "gray20", "#549A87", "#7D124E","#A05112", "#3383AF", "#8FCFF3")) +
  scale_linetype_manual(name = "Study area",
                        values = "dashed") +
  scale_colour_manual(name = "Survey sites",
                      labels = "RLS & ATRC survey sites \nwith pairwise distance ≥ 30km", 
                      values = "firebrick1") +
  ggsn::scalebar(sf_oz, dist_unit = "km", 
                 dist = 400, # units of scale bar
                 st.size = 4, # general size
                 height = 0.04, # height of the scale bar relative to y-axis
                 model = 'WGS84', 
                 transform = TRUE, # input as dec deg
                 location = "bottomright", # proximate location
                 anchor = c(x = 164, y = -41)) # percise position


# map for thesis defense presentation
ggplot(sf_oz_nostates) +
  theme_bw() +
  geom_sf(colour = "gray25", linewidth = 0.6, fill = "gray50") +
  coord_sf(crs = "+proj=lonlat +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") +
  #theme_dark() +
  geom_point(data = coords_30,
             aes(x = longitude, y = latitude, fill = "deepskyblue1"), # #1A85FF
             pch=21,
             size = 3) +
  xlim(c(112,168.5)) +
  ylim(c(-44.5,-8.4)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(panel.grid = element_line(colour = "gray80", linetype = "dashed"),
        panel.border = element_rect(colour = "gray30", linewidth = 1),
        legend.position = c(.8,.925),
        legend.title = element_blank(),
        legend.key=element_blank(),
        legend.box.margin = margin(1, 1, 6, 1),
        legend.text = element_text(size = 16, face= "bold"),
        legend.background = element_rect(fill = "white"),
        legend.box.background = element_rect(colour = "gray30", linewidth = 0.6),
        axis.title.x = element_text(size = 20, face= "bold"),
        axis.title.y = element_text(size = 20, face= "bold"),
        axis.text.x = element_text(size = 16, face= "bold"),
        axis.text.y = element_text(size = 16, face= "bold"),
        axis.ticks.length=unit(.25, "cm")) +
  scale_fill_manual(labels = "RLS & ATRC survey sites \nwith pairwise distance ≥ 30km", 
                    values = "deepskyblue1") +
  ggsn::scalebar(sf_oz, dist_unit = "km", 
                 dist = 400, # units of scale bar
                 st.size = 5, # general size
                 height = 0.04, # height of the scale bar relative to y-axis
                 model = 'WGS84', 
                 transform = TRUE, # input as dec deg
                 location = "bottomright", # proximate location
                 anchor = c(x = 167, y = -43)) # percise position
