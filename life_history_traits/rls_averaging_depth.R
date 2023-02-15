# RLS DATA
## average abundance data of each species over depth and block per survey

# load libraries
library(tidyverse)

# clean memory
rm(list=ls())

# set working directory
setwd("~/projects/msc_thesis")

# conditional code
save_my_data = TRUE

# read RLS dataset
rls_2019_2022 = read_delim("/media/mari/Crucial X8/rls_2019_2022_clean.csv", delim = ",")

# subset data 
rls_sub = rls_2019_2022 %>% 
  select(
    latitude, longitude, area, survey_date, depth,
    class, order, family, species_name, valid_name, aphia_id,
    size_class, total, biomass
  ) %>%
  filter(survey_date > "2019-12-31")

# depth bins
rls_sub_binned = rls_sub %>%
  mutate(depth_bin = cut(depth, breaks = c(0,15,30))
  )

# average size_class, total, biomass for each species per survey, i.e., unique combinations of latitude, longitude, and survey_date
rls_avg_depth = rls_sub_binned %>% 
  group_by(latitude, longitude, survey_date, depth_bin, species_name) %>% 
  summarise(
    size_class_mean = round(mean(size_class), digits = 2),
    total_mean = round(mean(total), digits = 2),
    biomass_mean = round(mean(biomass), digits = 2)
  ) %>%
  ungroup()

# extract taxonomic information for each species from original file
taxonomy = rls_2019_2022 %>% 
  select(class, order, family, aphia_id, species_name, valid_name) %>% 
  distinct(species_name, .keep_all = TRUE)

# add class, order, family to averaged rls dataframe
rls_avg_depth = left_join(rls_avg_depth, taxonomy, by = "species_name")

# write into csv
if (save_my_data ==TRUE) {
  write.csv(rls_avg_depth,"~/projects/msc_thesis/data/rls_2020_2022_avg_depth.csv", row.names = FALSE)
  write.csv(rls_avg_depth,"/media/mari/Crucial X8/rls_2020_2022_avg_depth.csv", row.names = FALSE)
} else {
  print("Data not saved!")
}

