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
setwd("~/projects/msc_thesis")

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
                   "method", "reporting_name", "size_class", "biomass", "geom", 
                   "aphia_id", "valid_AphiaID", "valid_name")) %>% 
  dplyr::group_by(area, latitude, longitude, survey_date, depth, hour, species_name) %>% 
  dplyr::summarise(total_avg = round(mean(total), digits = 1)) %>% 
  dplyr::ungroup()

# create bins
range(rls_subset$depth)
rls_subset$hour = as.POSIXct(rls_subset$hour, format="%H:%M:%S", tz="UTC")
rls_subset = rls_subset %>% 
  dplyr::mutate(depth_bin = cut(depth, breaks = c(0,10,20,30)),
                hour_bin = cut(hour, breaks = "4 hours"),
                lat_bin = cut(latitude, breaks = c(-5,-15,-25,-35,-45))
  ) %>% 
  dplyr:: select(area, latitude, longitude, survey_date, depth, hour,
                 lat_bin, depth_bin, hour_bin, species_name, total_avg)

# subset traities
bodysize = traities %>% 
  dplyr::mutate(species_name = paste(genus, species, sep = " ")) %>% 
  dplyr:: select(species_name, bodySize)


###### join data and create bins

# subset sites with minimum distance of 30km
rls_30 = dplyr::left_join(coords_30, rls_subset, by = c("latitude", "longitude"))

# choose only one random observation per coordinate pair (BODY SIZE)
# set.seed(300)
# rls_30 = rls_30 %>%
#   dplyr::group_by(latitude, longitude) %>%
#   dplyr::slice_sample(n = 1) %>%
#   dplyr::ungroup()
### copy this code for the other scripts to make sure that we always sample the same sites

# join body size data
rls_bodysize = left_join(rls_30, bodysize, by = "species_name")

###### check latitudinal distribution grouped by time and depth
bodysize_depth = ggplot(data = rls_bodysize, aes(x = latitude, y = bodySize, colour = depth_bin)) + 
  geom_point() +
  xlab("Latitude") +
  ylab("Body size (cm)") +
  theme_classic() +
  scale_colour_manual(name = "Depth bins",
                      values = c("#56B4E9", "#E69F00", "#009E73"),
                        labels = c("≤ 10m", "10 - 20m", "20 - 30m")) +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.text = element_text(size = 8, face= "bold"),
        legend.title = element_text(size = 8, face= "bold"),
        axis.title.x = element_text(size = 10, face= "bold"),
        axis.title.y = element_text(size = 10, face= "bold"),
        axis.text.x = element_text(size = 10, face= "bold"),
        axis.text.y = element_text(size = 10, face= "bold"),
        axis.ticks.length=unit(.25, "cm"),
        axis.line = element_line(colour = 'black', size = 1)) 

bodysize_time = ggplot(data = rls_bodysize, aes(x = latitude, y = bodySize, colour = hour_bin)) + 
  geom_point() +
  xlab("Latitude") +
  ylab("Body size (cm)") +
  theme_classic() +
  scale_colour_manual(name = "Time bins (24-h clock)",
                      values = c("#000000", "#56B4E9", "#E69F00", "#009E73"),
                      labels = c("04:00 - 07:59", "08:00 - 11:59", "12:00 - 15:59", "16:00 - 19:59")) +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.text = element_text(size = 8, face= "bold"),
        legend.title = element_text(size = 8, face= "bold"),
        axis.title.x = element_text(size = 10, face= "bold"),
        axis.title.y = element_text(size = 10, face= "bold"),
        axis.text.x = element_text(size = 10, face= "bold"),
        axis.text.y = element_text(size = 10, face= "bold"),
        axis.ticks.length=unit(.25, "cm"),
        axis.line = element_line(colour = 'black', size = 1))

total_depth = ggplot(data = rls_bodysize, aes(x = latitude, y = total_avg, colour = depth_bin)) + 
  geom_point() +
  xlab("Latitude") +
  ylab("Number of species") +
  ylim(c(0,2200)) +
  theme_classic() +
  scale_colour_manual(name = "Depth bins",
                      values = c("#56B4E9", "#E69F00", "#009E73"),
                      labels = c("≤ 10m", "10 - 20m", "20 - 30m")) +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.text = element_text(size = 8, face= "bold"),
        legend.title = element_text(size = 8, face= "bold"),
        axis.title.x = element_text(size = 10, face= "bold"),
        axis.title.y = element_text(size = 10, face= "bold"),
        axis.text.x = element_text(size = 10, face= "bold"),
        axis.text.y = element_text(size = 10, face= "bold"),
        axis.ticks.length=unit(.25, "cm"),
        axis.line = element_line(colour = 'black', size = 1)) 

total_time = ggplot(data = rls_bodysize, aes(x = latitude, y = total_avg, colour = hour_bin)) + 
  geom_point() +
  xlab("Latitude") +
  ylab("Number of species") +
  ylim(c(0,2200)) +
  theme_classic() +
  scale_colour_manual(name = "Time bins (24-h clock)",
                      values = c("#000000", "#56B4E9", "#E69F00", "#009E73"),
                      labels = c("04:00 - 07:59", "08:00 - 11:59", "12:00 - 15:59", "16:00 - 19:59")) +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.text = element_text(size = 8, face= "bold"),
        legend.title = element_text(size = 8, face= "bold"),
        axis.title.x = element_text(size = 10, face= "bold"),
        axis.title.y = element_text(size = 10, face= "bold"),
        axis.text.x = element_text(size = 10, face= "bold"),
        axis.text.y = element_text(size = 10, face= "bold"),
        axis.ticks.length=unit(.25, "cm"),
        axis.line = element_line(colour = 'black', size = 1))

bodysize_plot = ggarrange(bodysize_depth, bodysize_time, total_depth, total_time,
                      ncol = 2, nrow = 2)
annotate_figure(bodysize_plot,
                top = text_grob("Body size and species numbers over latitude grouped by depth or time",
                                color = "black", face = "bold", size = 14))

# trait over depth
size_depth = ggplot(data = rls_bodysize, aes(x = depth, y = bodySize)) + 
  geom_point() +
  ggtitle("Body size vs. depth") +
  xlab("Depth (m)") +
  ylab("Body size (cm)") +
  theme_classic() +
  theme(title = element_text(size = 12, face= "bold"),
        axis.title.x = element_text(size = 12, face= "bold"),
        axis.title.y = element_text(size = 12, face= "bold"),
        axis.text.x = element_text(size = 10, face= "bold"),
        axis.text.y = element_text(size = 10, face= "bold"),
        axis.ticks.length=unit(.25, "cm"),
        axis.line = element_line(colour = 'black', size = 1))

size_time = ggplot(data = rls_bodysize, aes(x = hour, y = bodySize)) + 
  geom_point() +
  ggtitle("Body size vs. day time of survey") +
  xlab("Time (24-h clock)") +
  ylab("Body size (cm)") +
  theme_classic() +
  theme(title = element_text(size = 12, face= "bold"),
        axis.title.x = element_text(size = 12, face= "bold"),
        axis.title.y = element_text(size = 12, face= "bold"),
        axis.text.x = element_text(size = 10, face= "bold"),
        axis.text.y = element_text(size = 10, face= "bold"),
        axis.ticks.length=unit(.25, "cm"),
        axis.line = element_line(colour = 'black', size = 1))

num_depth = ggplot(data = rls_bodysize, aes(x = depth, y = bodySize)) + 
  geom_point() +
  ggtitle("Number of species vs. depth") +
  xlab("Depth (m)") +
  ylab("Number of species") +
  theme_classic() +
  theme(title = element_text(size = 12, face= "bold"),
        axis.title.x = element_text(size = 12, face= "bold"),
        axis.title.y = element_text(size = 12, face= "bold"),
        axis.text.x = element_text(size = 10, face= "bold"),
        axis.text.y = element_text(size = 10, face= "bold"),
        axis.ticks.length=unit(.25, "cm"),
        axis.line = element_line(colour = 'black', size = 1))

num_time = ggplot(data = rls_bodysize, aes(x = hour, y = bodySize)) + 
  geom_point() +
  ggtitle("Number of species vs. day time of survey") +
  xlab("Time (24-h clock)") +
  ylab("Number of species") +
  theme_classic() +
  theme(title = element_text(size = 12, face= "bold"),
        axis.title.x = element_text(size = 12, face= "bold"),
        axis.title.y = element_text(size = 12, face= "bold"),
        axis.text.x = element_text(size = 10, face= "bold"),
        axis.text.y = element_text(size = 10, face= "bold"),
        axis.ticks.length=unit(.25, "cm"),
        axis.line = element_line(colour = 'black', size = 1))

ggarrange(size_depth, size_time, num_depth, num_time,
                          ncol = 2, nrow = 2)

# linear model
mod1 = lm(bodySize ~ depth, data = rls_bodysize)
summary(mod1)
mod2 = lm(bodySize ~ latitude, data = rls_bodysize)
summary(mod2)
mod3 = lm(log(bodySize) ~ latitude + depth, data = rls_bodysize)
summary(mod3)
plot(mod3)


###### preparing the data for the nMDS

# tranform bodysize list into columns
rls_wide_trait = rls_bodysize %>%
  dplyr::select(-total_avg) %>% 
  tidyr::pivot_wider(names_from = species_name,
              values_from = bodySize,
              names_sep = "_",
              values_fill = list(bodySize = 0),
              values_fn = list(bodySize = mean)
  ) 

# tranform total list into columns
rls_wide_species = rls_bodysize %>%
  dplyr::select(-bodySize) %>%
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

## calculate dissimilarities between all rows
vegdist(x = rls_species, method = "bray", diag = TRUE, upper = TRUE)

## perform nMDS on species-only data
nmds_total = metaMDS(comm = rls_species,
                    distance = "bray",
                    k = 2,
                    autotransform = FALSE)

## check stress values
nmds_total$stress

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
ggplot(data = nmds_total_dat,
       mapping = aes(x = MDS1, y = MDS2, colour = lat_bin)) +
  geom_point(size = 2.5) +
  scale_colour_viridis_d() +
  theme_classic() +
  ggtitle("Latitude bins") +
  theme(legend.position="right") +
  theme(plot.title = element_text(size = 16, face = "bold")) +
  theme(text = element_text(size=16)) +
  grid()

### day time
ggplot(data = nmds_total_dat,
       mapping = aes(x = MDS1, y = MDS2, colour = hour_bin)) +
  geom_point(size = 2.5) +
  scale_colour_viridis_d() +
  theme_classic() +
  ggtitle("Daytime bins") +
  theme(legend.position="right") +
  theme(plot.title = element_text(size = 16, face = "bold")) +
  theme(text = element_text(size=16)) +
  grid()

### depth
ggplot(data = nmds_total_dat,
       mapping = aes(x = MDS1, y = MDS2, colour = depth_bin)) +
  geom_point(size = 2.5) +
  scale_colour_viridis_d() +
  theme_classic() +
  ggtitle("Depth bins") +
  theme(legend.position="right") +
  theme(plot.title = element_text(size = 16, face = "bold")) +
  theme(text = element_text(size=16)) +
  grid()


