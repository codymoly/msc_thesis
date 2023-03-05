# REEF LIFE SURVEY - get survey coordinates
## settings for subset data on https://portal.aodn.org.au/search:
## temporal range: 2019-01-01 - 2022-12-31
## geographical range: -8.09°N, 169.23°E, -45.26°S, 107.53°W

# load required libraries
library(tidyverse)

# clear memory
rm(list=ls())

# set working directory
setwd("~/projects/msc_thesis")

# save coordinate data?
save_my_data = FALSE

# read RLS dataset, skip rows containing metadata (1-71)
#rls_raw = read_delim("/media/mari/Crucial X8/RLS_20190101_20221120.csv", skip = 71, delim = ",")
rls_2019_2022 = read_delim("/media/mari/Crucial X8/rls_2019_2022_upd.csv", skip = 71, delim = ",")

# extract coordinates for survey sites
survey_coordinates_2019_2022 = rls_2019_2022 %>% 
  select(latitude, longitude) %>%
  distinct()

## write coordinates in csv
if (save_my_data == TRUE) {
  write.csv(survey_coordinates_2019_2022,"~/projects/msc_thesis/data/survey_coordinates_2019_2022.csv", row.names = FALSE)
  write.csv(survey_coordinates_2019_2022,"/media/mari/Crucial X8/survey_coordinates_2019_2022.csv", row.names = FALSE)
} else {
  print("Data not saved!")
}

rls_2020_2022 = rls_2019_2022 %>% filter(class == "Actinopterygii", survey_date > "2019-12-31")
min(rls_2020_2022$latitude) # -43.53
max(rls_2020_2022$latitude) # -9.88
min(rls_2020_2022$longitude) # 113.17
max(rls_2020_2022$longitude) # 167.99
