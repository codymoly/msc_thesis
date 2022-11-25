# REEF LIFE SURVEY - Script 1: get survey coordinates
## settings for subset data on https://portal.aodn.org.au/search: 
## geogr. range: -8.17°N, 170.02°E, -45.62°S, 108.15°W
## period: 2020-01-01 - 2021-12-31

# load required libraries
library(tidyverse) # v. 1.3.2

# clear memory
rm(list=ls())

# set working directory
setwd("~/Downloads")

# read RLS dataset, skip rows containing metadata (1-71)
rls_raw = read_delim("/media/mari/Crucial X8/RLS_20190101_20221120.csv", skip = 71, delim = ",")

# subset data by year, note: we might use only surveys from one year
# str(rls_raw) # date seems to have the correct format...
# rls_2020 = rls_raw %>% filter(survey_date < "2021-01-01")
#rls_2021 = rls_raw %>% filter(survey_date > "2020-12-31")

# extract coordinates for survey sites
## we need this to subset the sst & chlA data based on the RLS sites
## subset unique coordinate pairs
survey_coordinates = rls_raw %>% select(latitude, longitude) %>% distinct

## write coordinates in csv
write.csv(survey_coordinates,"/media/mari/Crucial X8/survey_coordinates.csv", row.names = FALSE) # comma as sep.
