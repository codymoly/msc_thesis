# DESCRIPTIVE RESULTS OF CWM AND CWV

# read libraries
library(tidyverse)

# clean memory
rm(list=ls())

# set working directory
setwd("~/Documents/MSc_thesis/Figures")

# import datasets
final_dataset = readr::read_delim("/media/mari/Crucial X8/sst_trait_data.csv", delim = ",")
rls_area_raw = readr::read_delim("/media/mari/Crucial X8/rls_2019_2022_avg.csv", delim = ",")
rls_original = readr::read_delim("/media/mari/Crucial X8/rls_2019_2022_clean.csv", delim = ",")


###### preparation

# kelvin into deg celsius
final_dataset["sst_raw_mean"] = final_dataset["sst_raw_mean"] - 273.15

# filter sites between 2020 and 2022 (for sites < 2020, the predictability contained missing months)
sites30_coords = final_dataset %>% 
  dplyr::select(new_survey_id, latitude, longitude, survey_date, area)

# join both datasets
rls_30 = dplyr::left_join(sites30_coords, rls_area_raw, by = c("latitude", "longitude", "survey_date"))
rls_30_original = dplyr::left_join(sites30_coords, rls_original, by = c("latitude", "longitude", "survey_date"))


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

## how many groups were identified not identified on species level?
no_sp_level = all_species %>% filter(str_detect(species_name, "p\\."))
all_species %>% filter(str_detect(species_name, "p\\.")) %>% n_distinct() # 35

## how many groups were identified identified on family level?
all_species %>% filter(str_detect(species_name, "ae ")) %>% n_distinct() # 9

## in how many families are those individuals identified on a low level?
length(unique(no_sp_level[["family"]])) # 18

## hence: 35 - 9 = 26 groups were identified on genus level, 9 only on family level
## hence: 869 - 35 = 834 groups were identified on species level

# identification of individual level
## how many size class entries are missing?
summary(rls_30_original$size_class) # no

## how many fish in total were counted?
length(rls_30_original$species_name) # 23683 individuals

## select individuals
individuals = rls_30_original %>% select(new_survey_id, family, species_name) %>% filter(str_detect(species_name, "p\\."))

## on how many sites where unidentifiable species found?
length(unique(individuals[["new_survey_id"]])) # 33

## how many individuals were not identified on the species level?
length(individuals[["species_name"]]) # 95 
95/23683 # 0.004011316, < 1%

## how many individuals were only identified on family level?
individuals %>% filter(str_detect(species_name, "ae ")) %>% n_distinct() # 12


###### size
summary(rls_30_original$size_class)
summary(final_dataset$size_class_cwm)
summary(final_dataset$size_class_cwv)

plot(final_dataset$area, final_dataset$size_class_cwm)

ggplot(data = final_dataset, aes(x = new_survey_id, y = size_class_cwm, fill = area)) +
  geom_col()

ggplot(data = final_dataset, aes(x = new_survey_id, y = size_class_cwv, fill = area)) +
  geom_col()
