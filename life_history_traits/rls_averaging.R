# RLS DATA
## average abundance data of each species over depth and block per survey

# load libraries
library(tidyverse)

# clean memory
rm(list=ls())

# set working directory
setwd("~/projects/msc_thesis")

# read RLS dataset
rls_2021_2022 = read_delim("/media/mari/Crucial X8/RLS_2021_2022.csv", skip = 71, delim = ",")

# data exploration
# n_distinct(rls_2021_2022$survey_id) # 1276, identifier for each survey, note that we don't differ between depths
# n_distinct(rls_2021_2022$site_code) # 475, identifier for each location
# n_distinct(rls_2021_2022$FID) # 91410, identifier for each line
# # unique(rls_2021_2022$ecoregion)
# nrow(unique(rls_2021_2022[c('latitude', 'longitude', 'survey_date')])) # 490 observations

## remove brackets from species name, e.g., change Pomacentrus sp. [rhodonotus] to Pomacentrus rhodonotus
## write function that explaces the different signs
rmBrackets = function(spname){
  return(str_replace_all(spname, "sp\\. \\[([^\\\\]*)\\]", "\\1"))
}
## apply function on species column in original file
rls_2021_2022["species_name"] <- lapply(rls_2021_2022["species_name"], rmBrackets)

# subset data 
rls_sub = rls_2021_2022 %>%
  select(
    latitude, longitude, survey_date, 
    class, order, family, species_name, 
    size_class, total, biomass
  )

# average size_class, total, biomass for each species per survey, i.e., unique combinations of latitude, longitude, and survey_date
rls_avg = rls_sub %>% 
  group_by(latitude, longitude, survey_date, species_name) %>% 
  summarise(
    size_class_mean = round(mean(size_class), digits = 1),
    total_mean = round(mean(total), digits = 1),
    biomass_mean = round(mean(biomass), digits = 1)
    ) %>% 
  mutate(ID = cur_group_id()) %>% # assign unique id number to each group
  ungroup()

# extract taxonomic information for each species from original file
taxonomy = rls_2021_2022 %>% 
  select(class, order, family, species_name) %>% 
  distinct(species_name, .keep_all = TRUE)

# add class, order, family to averaged rls dataframe
rls_avg = left_join(rls_avg, taxonomy, by = "species_name")

# write into csv
write.csv(rls_avg,"~/projects/msc_thesis/data/rls_2021_2022_avg.csv", row.names = FALSE)
write.csv(rls_avg,"/media/mari/Crucial X8/rls_2021_2022_avg.csv", row.names = FALSE)
