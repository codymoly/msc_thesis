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
plot_survey_map = TRUE

# import datasets
final_dataset = readr::read_delim("/media/mari/Crucial X8/sst_trait_data.csv", delim = ",")
sites30 = readr::read_delim("/media/mari/Crucial X8/sst_trait_data.csv", delim = ",")
rls_area_raw = readr::read_delim("/media/mari/Crucial X8/rls_2019_2022_avg.csv", delim = ",")
final_dataset = final_dataset %>% dplyr::select(-c("new_survey_id", "area"))

###### data exploration

# kelvin into deg celsius
final_dataset["sst_raw_mean"] = final_dataset["sst_raw_mean"] - 273.15

summary(final_dataset)

# remove outlier from csv to make it plotable
final_dataset_outl = final_dataset %>% 
  dplyr::filter(size_class_cwv < 800)


###### plot sites
library(sf)
library(maps)
library(mapdata)

aussi = st_as_sf(map("worldHires", "Australia", fill=TRUE, xlim=c(110,160), ylim=c(-45,-5), mar=c(0,0,0,0)))

ggplot(data = aussi) + 
  geom_sf() + 
  geom_point(data = final_dataset, aes(x = longitude, y = latitude, colour = "steelblue3"), size = 3) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_classic() +
  theme(legend.position = c(.84,.88),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.text = element_text(size = 12, face= "bold"),
        legend.title = element_blank(),
        axis.title.x = element_text(size = 14, face= "bold"),
        axis.title.y = element_text(size = 14, face= "bold"),
        axis.text.x = element_text(size = 10, face= "bold"),
        axis.text.y = element_text(size = 10, face= "bold"),
        axis.ticks.length=unit(.25, "cm"),
        axis.line = element_line(linewidth = 0.8)) +
  scale_colour_manual(labels = "Survey sites with \npairwise distance ≥ 30km", 
                      values = "steelblue3")


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
    geom_point(data = final_dataset, aes(x = longitude, y = latitude, colour = size_class_cwm), size = 3) +
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
    scale_colour_viridis(name = "Body size CWM (cm)")
  
  ggplot(data = aussi) + 
    geom_sf() + 
    geom_point(data = final_dataset_outl, aes(x = longitude, y = latitude, colour = size_class_cwv), size = 3) +
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
    scale_colour_viridis(name = "Body size CWV (cm²)")
  
  ggplot(data = aussi) + 
    geom_sf() + 
    geom_point(data = final_dataset, aes(x = longitude, y = latitude, colour = sp_richness), size = 3) +
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

##### taxa
# filter sites between 2020 and 2022 (for sites < 2020, the predictability contained missing months)
sites30_coords = sites30 %>% 
  dplyr::select(latitude, longitude, survey_date)

# join both datasets
rls_30 = dplyr::left_join(sites30_coords, rls_area_raw, by = c("latitude", "longitude", "survey_date"))

# explore
length(unique(rls_30[["family"]])) # 74
length(unique(rls_30[["valid_name"]])) # 834
rls_30 %>% select(valid_name) %>% filter(!is.na(valid_name)) %>% n_distinct() # 833 on species level
on_fam_lev = rls_30 %>% select(family, valid_name) %>% filter(is.na(valid_name)) # 52 individuals on fam level...
length(unique(on_fam_lev[["family"]])) # ... in 18 families
