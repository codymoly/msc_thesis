# nMDS FOR ASSESSING THE EFFECT OF DEPTH AND DAY TIME ON THE COMMUNITY COMPOSITION

# load libraries
library(tidyverse)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggpubr)
library(vegan)
library(corrplot)

# clean memory
rm(list=ls())

# set working directory
setwd("~/Documents/MSc_thesis/Figures")

# read raw rls data
rls_raw = read_delim("/media/mari/Crucial X8/rls_2019_2022_clean.csv", delim = ",")
coords_30 = read_delim("/media/mari/Crucial X8/coords_30km.csv", delim = ",")
traities = read_delim("/media/mari/Crucial X8/species_bodysize_imputed.csv", delim = ",") 


###### subset RLS and body size data

# subset and average blocks in RLS dataset
names(rls_raw)
rls_subset = rls_raw %>%
  dplyr::filter(survey_date > "2019-12-31") %>% 
  dplyr::select(-c("FID", "survey_id", "country", "ecoregion", "realm", "location",
                   "site_code", "site_name", "program", "visibility", "survey_latitude", "survey_longitude",
                   "method", "reporting_name", "biomass", "geom", 
                   "aphia_id", "valid_AphiaID", "valid_name")) %>% 
  dplyr::group_by(area, latitude, longitude, survey_date, depth, hour, species_name) %>% 
  dplyr::summarise(total_avg = round(mean(total), digits = 1),
                   sizeclass_avg = round(mean(size_class), digits = 1)) %>% 
  dplyr::ungroup()

# create bins
range(rls_subset$depth)
rls_subset$hour = as.POSIXct(rls_subset$hour, format="%H:%M:%S", tz="UTC")
rls_subset = rls_subset %>% 
  dplyr::mutate(depth_bin = cut(depth, breaks = c(0,5,10,15,20,25,30)),
                hour_bin = cut(hour, breaks = "4 hours"),
                lat_bin = cut(latitude, breaks = c(-5,-15,-25,-35,-45))
  ) %>% 
  dplyr:: select(area, latitude, longitude, survey_date, depth, hour,
                 lat_bin, depth_bin, hour_bin, species_name, total_avg, sizeclass_avg)

###### join data and create bins

# subset sites with minimum distance of 30km
rls_30 = dplyr::left_join(coords_30, rls_subset, by = c("latitude", "longitude"))


###### preparing the data for the nMDS

# tranform bodysize list into columns
rls_wide_trait = rls_30 %>%
  dplyr::select(-total_avg) %>% 
  tidyr::pivot_wider(names_from = species_name,
              values_from = sizeclass_avg,
              names_sep = "_",
              values_fill = list(sizeclass_avg = 0),
              values_fn = list(sizeclass_avg = mean)
  ) 

# tranform total list into columns
rls_wide_species = rls_30 %>%
  dplyr::select(-sizeclass_avg) %>%
  tidyr::pivot_wider(names_from = species_name,
              values_from = total_avg,
              names_sep = "_",
              values_fill = list(total_avg = 0),
              values_fn = list(total_avg = mean)
              )


###### nMDS with total numbers and body size

# univsersal data
## list species names
rls_names =
  dplyr::select(rls_wide_species, -(latitude:hour_bin)) %>% 
  dplyr::select(where(~ any(. != 0))) %>%
  names() 
rls_names

## site data
site_dat = select(rls_wide_species,
                  latitude, longitude,
                  survey_date, depth, hour,
                  lat_bin, depth_bin, hour_bin)

# nMDS with total numbers
## select abundance data
rls_species =
  rls_wide_species %>%
  dplyr::select(all_of(rls_names))

## replace by species weights
row_sums <- rowSums(rls_species)

# divide each value by the row sum and replace it in the dataframe
rls_species[] <- t(t(rls_species) / row_sums)

## calculate dissimilarities between all rows
vegdist(x = rls_species, method = "bray", diag = TRUE, upper = TRUE)

## perform nMDS on species-only data
nmds_total = metaMDS(comm = rls_species,
                    distance = "bray",
                    k = 2,
                    autotransform = FALSE)

## check stress values
nmds_total$stress # 0.1094204

## stress plot
stressplot(nmds_total)

## extract nMDS scores and create a dataframe
nmds_total_dat = tibble::as_tibble(nmds_total$points)

## add site specific data
nmds_total_dat = dplyr::bind_cols(nmds_total_dat, site_dat)

## check correlation between nMDS axes, latitude, longitude, and depth
nmds_total_dat %>%
  select(MDS1, MDS2, latitude, longitude, depth) %>%
  cor(x = ., method = c("pearson")) %>%
  corrplot(method = "number")

## plot nMDS
### latitude
nmds_lati = ggplot(data = nmds_total_dat,
       mapping = aes(x = MDS1, y = MDS2, colour = lat_bin)) +
  geom_point(size = 2.5) +
  theme_classic() +
  ggtitle("Latitude bins") +
  theme(legend.position="right") +
  theme(plot.title = element_text(size = 16, face = "bold")) +
  theme(text = element_text(size=16)) +
  grid() +
  scale_colour_manual(name = "Latitude bins (°)",
                      values = c("#56B4E9", "#E69F00", "#000000", "#009E73"),
                      labels = c("04:00 - 07:59", "08:00 - 11:59", "12:00 - 15:59", "16:00 - 19:59"))

### day time
nmds_time = ggplot(data = nmds_total_dat,
       mapping = aes(x = MDS1, y = MDS2, colour = hour_bin)) +
  geom_point(size = 2.5) +
  theme_classic() +
  ggtitle("Daytime") +
  theme(legend.position="right") +
  theme(plot.title = element_text(size = 16, face = "bold")) +
  theme(text = element_text(size=16)) +
  grid() +
  scale_colour_manual(name = "Time bins \n(24-h clock)",
                      values = c("#474AE2", "#C70363", "#FFCF00", "#43C540"),
                      labels = c("04:00 - 07:59", "08:00 - 11:59", "12:00 - 15:59", "16:00 - 19:59"))

### depth 
nmds_depth = ggplot(data = nmds_total_dat,
       mapping = aes(x = MDS1, y = MDS2, colour = depth_bin)) +
  geom_point(size = 2.5) +
  theme_classic() +
  ggtitle("Depth") +
  theme(legend.position="right") +
  theme(plot.title = element_text(size = 16, face = "bold")) +
  theme(text = element_text(size=16)) +
  grid() +
  scale_colour_manual(name = "Depth bins (m)",
                      values = c("#474AE2", "#C70363", "#FFCF00", "#43C540", "#000000"),
                      labels = c("(0-5]", "(5-10]", "(10-15]", "(15-20]", "(20-25]"))

nmds_plots = ggarrange(nmds_depth, nmds_time,
                          ncol = 1, nrow = 2)
annotate_figure(nmds_plots,
                top = text_grob("nMDS: community composition based on species weights along depth and time bins",
                                color = "black", face = "bold", size = 14))

##################
# univsersal data
## list species names
rls_names =
  dplyr::select(rls_wide_trait, -(latitude:hour_bin)) %>% 
  dplyr::select(where(~ any(. != 0))) %>%
  names() 
rls_names

## site data
site_dat = select(rls_wide_trait,
                  latitude, longitude,
                  survey_date, depth, hour,
                  lat_bin, depth_bin, hour_bin)

# nMDS with total numbers
## select abundance data
rls_species =
  rls_wide_trait %>%
  dplyr::select(all_of(rls_names))

## calculate dissimilarities between all rows
vegdist(x = rls_species, method = "bray", diag = TRUE, upper = TRUE)

## perform nMDS on species-only data
nmds_total = metaMDS(comm = rls_species,
                     distance = "bray",
                     k = 2,
                     autotransform = FALSE)

## check stress values
nmds_total$stress # 0.09086759

## stress plot
stressplot(nmds_total)

## extract nMDS scores and create a dataframe
nmds_total_dat = tibble::as_tibble(nmds_total$points)

## add site specific data
nmds_total_dat = dplyr::bind_cols(nmds_total_dat, site_dat)

## check correlation between nMDS axes, latitude, longitude, and depth
nmds_total_dat %>%
  select(MDS1, MDS2, latitude, longitude, depth) %>%
  cor(x = ., method = c("pearson")) %>%
  corrplot(method = "number")

## plot nMDS
### latitude
nmds_lati = ggplot(data = nmds_total_dat,
                   mapping = aes(x = MDS1, y = MDS2, colour = lat_bin)) +
  geom_point(size = 2.5) +
  theme_classic() +
  ggtitle("Latitude bins") +
  theme(legend.position="right") +
  theme(plot.title = element_text(size = 16, face = "bold")) +
  theme(text = element_text(size=16)) +
  grid() +
  scale_colour_manual(name = "Latitude bins (°)",
                      values = c("#56B4E9", "#E69F00", "#000000", "#009E73"),
                      labels = c("04:00 - 07:59", "08:00 - 11:59", "12:00 - 15:59", "16:00 - 19:59"))

### day time
nmds_time = ggplot(data = nmds_total_dat,
                   mapping = aes(x = MDS1, y = MDS2, colour = hour_bin)) +
  geom_point(size = 2.5) +
  theme_classic() +
  ggtitle("Daytime") +
  theme(legend.position="right") +
  theme(plot.title = element_text(size = 16, face = "bold")) +
  theme(text = element_text(size=16)) +
  grid() +
  scale_colour_manual(name = "Time bins \n(24-h clock)",
                      values = c("#474AE2", "#C70363", "#FFCF00", "#43C540"),
                      labels = c("04:00 - 07:59", "08:00 - 11:59", "12:00 - 15:59", "16:00 - 19:59"))

### depth
nmds_depth = ggplot(data = nmds_total_dat,
                    mapping = aes(x = MDS1, y = MDS2, colour = depth_bin)) +
  geom_point(size = 2.5) +
  theme_classic() +
  ggtitle("Depth") +
  theme(legend.position="right") +
  theme(plot.title = element_text(size = 16, face = "bold")) +
  theme(text = element_text(size=16)) +
  grid() +
  scale_colour_manual(name = "Depth bins (m)",
                      values = c("#474AE2", "#C70363", "#FFCF00", "#43C540", "#000000"),
                      labels = c("(0-5]", "(5-10]", "(10-15]", "(15-20]", "(20-25]"))

nmds_plots = ggarrange(nmds_depth, nmds_time,
                       ncol = 1, nrow = 2)
annotate_figure(nmds_plots,
                top = text_grob("nMDS: community composition based on species size classes along depth and time bins",
                                color = "black", face = "bold", size = 14))


