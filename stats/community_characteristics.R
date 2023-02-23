# DESCRIPTIVE RESULTS

# read libs
library(tidyverse)
library(ggplot2)
library(ggbreak)
library(grid)
library(gridExtra)
library(ggpubr)
library(sf)
library(maps)
library(mapdata)
library(viridis)

# clean memory
rm(list=ls())

# set working directory
setwd("~/Documents/MSc_thesis/Figures")

# import datasets
final_dataset = readr::read_delim("/media/mari/Crucial X8/sst_trait_data.csv", delim = ",")
rls_area_raw = readr::read_delim("/media/mari/Crucial X8/rls_2019_2022_avg.csv", delim = ",")


###### preparation

# kelvin into deg celsius
final_dataset["sst_raw_mean"] = final_dataset["sst_raw_mean"] - 273.15

# filter sites between 2020 and 2022 (for sites < 2020, the predictability contained missing months)
sites30_coords = final_dataset %>% 
  dplyr::select(new_survey_id, latitude, longitude, survey_date, area)

# join both datasets
rls_30 = dplyr::left_join(sites30_coords, rls_area_raw, by = c("latitude", "longitude", "survey_date"))


###### taxonomy

# how many fish families does the dataset report?
length(unique(rls_30[["family"]])) # 74

# how many fish "species" does the dataset report?
length(unique(rls_30[["valid_name"]])) # 834

# how many individuals were identified/ not identified on species level?
## select unique species observations
all_species = rls_30 %>% select(family, species_name) %>% distinct(species_name, .keep_all = TRUE)

## how many unique species observations are there?
all_species %>% select(family, species_name) %>% distinct(species_name, .keep_all = TRUE) %>% n_distinct() # 869

## how many individuals were identified not identified on species level?
no_sp_level = all_species %>% filter(str_detect(species_name, "p\\."))
all_species %>% filter(str_detect(species_name, "p\\.")) %>% n_distinct() # 35

## how many individuals were identified identified on family level?
all_species %>% filter(str_detect(species_name, "ae ")) %>% n_distinct() # 9

## in how many families are those individuals identified on a low level?
length(unique(no_sp_level[["family"]])) # 18

## hence: 35 - 9 = 26 individuals were identified on genus level, 9 only on family level
## hence: 869 - 35 = 834 individuals were identified on species level