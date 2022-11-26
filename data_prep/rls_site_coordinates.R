# REEF LIFE SURVEY - Script 1: get survey coordinates
## settings for subset data on https://portal.aodn.org.au/search: 
## geographical range: -6.5°N, 170.9°E, -47.37°S, 102.26°W
## period: 2019-01-01 - 2022-11-20

# load required libraries
library(tidyverse) # v. 1.3.2

# clear memory
rm(list=ls())

# set working directory
setwd("~/Downloads")

# read RLS dataset, skip rows containing metadata (1-71)
rls_raw = read_delim("/media/mari/Crucial X8/RLS_20190101_20221120.csv", skip = 71, delim = ",")

# extract coordinates for survey sites
## we need this to subset the sst & chlA data based on the RLS sites
## subset unique coordinate pairs
survey_coordinates = rls_raw %>% select(latitude, longitude) %>% distinct

## write coordinates in csv
write.csv(survey_coordinates,"/media/mari/Crucial X8/survey_coordinates.csv", row.names = FALSE) # comma as sep.
