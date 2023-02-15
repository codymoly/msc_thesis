# DESCRIPTIVE RESULTS

# read libs
library(tidyverse)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggpubr)

# clean memory
rm(list=ls())

# set working directory
setwd("~/Documents/MSc_thesis/Figures")

# conditional code
plot_survey_map = FALSE

# import datasets
final_dataset = readr::read_delim("/media/mari/Crucial X8/sst_trait_data.csv", delim = ",")

final_dataset = final_dataset %>% dplyr::select(-c("new_survey_id", "area"))

###### data exploration

# kelvin into deg celsius
final_dataset["sst_raw_mean"] = final_dataset["sst_raw_mean"] - 273.15

summary(final_dataset)



###### plot spatial variability of traits etc.

# plot map with sites
if (plot_survey_map == TRUE) {
  library(sf)
  library(maps)
  library(mapdata)
  library(viridis)
  
  aussi = st_as_sf(map("worldHires", "Australia", fill=TRUE, xlim=c(110,160), ylim=c(-45,-5), mar=c(0,0,0,0)))
  
  ggplot(data = aussi) + 
    geom_sf() + 
    geom_point(data = eco_env, aes(x = longitude, y = latitude, colour = bodysize_cwm_total), size = 3) +
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
  
  ggplot(data = aussi) + 
    geom_sf() + 
    geom_point(data = eco_env, aes(x = longitude, y = latitude, colour = bodysize_cwv_total), size = 3) +
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
    scale_colour_viridis(name = "Body size CWV")
  
  ggplot(data = aussi) + 
    geom_sf() + 
    geom_point(data = eco_env, aes(x = longitude, y = latitude, colour = sp_richness), size = 3) +
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
    scale_colour_viridis(name = "Species richness")
  
  # ggarrange(cwm_map, cwv_map, rich_map,
  #                           ncol = 2, nrow = 2)
  
} else {
  print("No map!")
}