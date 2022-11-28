# REEF LIFE SURVEY - Script 1: get survey coordinates
## settings for subset data on https://portal.aodn.org.au/search: 
## geographical range:
## 2019-2022: -6.5°N, 170.9°E, -47.37°S, 102.26°W
## 2021-2022: -8.09°N, 169.23°E, -45.26°S, 107.53°W

# load required libraries
library(tidyverse)

# clear memory
rm(list=ls())

# set working directory
setwd("~/projects/msc_thesis")

# read RLS dataset, skip rows containing metadata (1-71)
#rls_raw = read_delim("/media/mari/Crucial X8/RLS_20190101_20221120.csv", skip = 71, delim = ",")
rls_2021_2022 = read_delim("/media/mari/Crucial X8/RLS_2021_2022.csv", skip = 71, delim = ",")

# extract coordinates for survey sites
## we need this to subset the sst & chlA data based on the RLS sites
## subset unique coordinate pairs

# for 2019 to 2022
#survey_coordinates = rls_raw %>% 
#  select(latitude, longitude) %>%
#  distinct

# for 2021 and 2022 only
survey_coordinates_2021_2022 = rls_2021_2022 %>% 
  select(latitude, longitude) %>%
  distinct

## write coordinates in csv
#write.csv(survey_coordinates,"/media/mari/Crucial X8/survey_coordinates.csv", row.names = FALSE) # comma as sep.
write.csv(survey_coordinates_2021_2022,"~/projects/msc_thesis/data/survey_coordinates_2021_2022.csv", row.names = FALSE)
write.csv(survey_coordinates_2021_2022,"/media/mari/Crucial X8/survey_coordinates_2021_2022.csv", row.names = FALSE) # comma as sep.
