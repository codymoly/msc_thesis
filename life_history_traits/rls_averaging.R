# CLEAN-UP OF RLS DATA
## average abundance data over depth and block, we want only one survey per time and locations

# load libraries
library(tidyverse)

# clean memory
rm(list=ls())

# set working directory
setwd("~/projects/msc_thesis")

# read RLS dataset
rls_2021_2022 = read_delim("/media/mari/Crucial X8/RLS_2021_2022.csv", skip = 71, delim = ",")

## remove brackets from species name
rmBrackets = function(spname){
  return(str_replace_all(spname, "sp\\. \\[([^\\\\]*)\\]", "\\1"))
}
rls_2021_2022["species_name"] <- lapply(rls_2021_2022["species_name"], rmBrackets)

# select relevant columns
rls_sub = rls_2021_2022 %>%
  select(
    latitude, longitude, survey_date, 
    class, order, family, species_name, 
    size_class, total, biomass
  )

# average size_class, total, biomass
rls_avg = rls_sub %>% 
  group_by(latitude, longitude, survey_date, species_name) %>% 
  summarise(
    size_class_mean = round(mean(size_class), digits = 1),
    total_mean = round(mean(total), digits = 1),
    biomass_mean = round(mean(biomass), digits = 1)
    ) %>% 
  arrange(survey_date) %>% 
  ungroup()

# write into csv
write.csv(rls_avg,"~/projects/msc_thesis/data/rls_2021_2022_avg.csv", row.names = FALSE)
write.csv(rls_avg,"/media/mari/Crucial X8/rls_2021_2022_avg.csv", row.names = FALSE)
