####### RLS DATA AVERAGING
## average abundance data of each species over depth and block per survey

# load libraries
library(tidyverse)

# clean memory
rm(list=ls())

# set working directory
setwd("~/projects/msc_thesis")

# conditional code
save_my_data = FALSE

# read RLS dataset
rls_2019_2022 = read_delim("/media/mari/Crucial X8/rls_2019_2022_clean.csv", delim = ",")

# subset data 
rls_sub = rls_2019_2022 %>% 
  select(
    latitude, longitude, survey_date, 
    class, order, family, species_name, valid_name, aphia_id,
    size_class, total, biomass
  )

# check zeros in size class
summary(rls_sub$size_class)
rls_sub["size_class"] = lapply(rls_sub["size_class"], function(x) 
  replace(x, rls_sub$size_class == 0.0, NA)) # replace values <= 0 with NAs in sst data
summary(rls_sub$size_class) # 92 NAs

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#     2.5     7.5    10.0    13.5    20.0   300.0      92

# average size_class, total, biomass for each species per survey, i.e., unique combinations of latitude, longitude, and survey_date
rls_avg = rls_sub %>% 
  group_by(latitude, longitude, survey_date, species_name) %>% 
  summarise(
    size_class_mean = round(mean(size_class, na.rm = TRUE), digits = 1),
    total_mean = round(mean(total), digits = 1),
    biomass_mean = round(mean(biomass), digits = 1)
    ) %>% 
  #mutate(ID = cur_group_id()) %>% # assign unique id number to each group
  ungroup()

# replace nans
rls_avg_backup = rls_avg
rls_avg$size_class_mean[is.nan(rls_avg$size_class_mean)] = NA

# extract taxonomic information for each species from original file
taxonomy = rls_2019_2022 %>% 
  select(class, order, family, aphia_id, species_name, valid_name) %>% 
  distinct(species_name, .keep_all = TRUE)

# add class, order, family to averaged rls dataframe
rls_avg = left_join(rls_avg, taxonomy, by = "species_name")

# write into csv
if (save_my_data ==TRUE) {
  write.csv(rls_avg,"~/projects/msc_thesis/data/rls_2019_2022_avg.csv", row.names = FALSE)
  write.csv(rls_avg,"/media/mari/Crucial X8/rls_2019_2022_avg.csv", row.names = FALSE)
} else {
  print("Data not saved!")
}

