# CREATE A UNIQUE IDENTIFIER FOR EACH SITE AND DEPTH
## we need this to calculate the environmental measures for each site and depth relative to the survey date

# required libraries
library(tidyverse)

# clear memory
rm(list=ls())

# set working directory
setwd("~/projects/msc_thesis")

# read rls data
rls_raw = read_delim("/media/mari/Crucial X8/RLS_2021_2022.csv", skip = 71, delim = ",")

# drop irrelevant columns
rls_sub = rls_raw %>%
  select(area,
         ecoregion, location, site_code, site_name,
         latitude, longitude, survey_date, depth, block,
         class, order, family, species_name,
         size_class, total, biomass
        )

# create new column by merging site_code and depth
## why? now we get a unique identifier per site AND depth
rls_sub$site_code_depth = paste(rls_sub$site_code, rls_sub$depth, sep = "_")

# subset the data with the information required to calculate the predictability (i.e, location, depth, survey time)
rls_envpred = rls_sub %>%
  select(site_code_depth, latitude, longitude, survey_date) %>%
  distinct(site_code_depth, .keep_all = TRUE) %>% 
  arrange(site_code_depth)

# write csv
write.csv(rls_envpred,"~/projects/msc_thesis/data/rls_site_depth_ID.csv", row.names = FALSE)
write.csv(rls_envpred,"/media/mari/Crucial X8/rls_site_depth_ID.csv", row.names = FALSE)

#sstFiles = list.files(pattern="data*.csv")
#chlaFiles = list.files(pattern="data*.csv")