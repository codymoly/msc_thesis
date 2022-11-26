# CALCULATE ENVIRONMENTAL MEASURES 

# required libraries
library(tidyverse)

# read rls data
rls_raw = read_delim("/media/mari/Crucial X8/RLS_20190101_20221120.csv", skip = 71, delim = ",")

# select relevant columns
rls_sub = rls_raw %>%
  select(area,
         ecoregion, location, site_code, site_name,
         latitude, longitude, survey_date, depth, block,
         class, order, family, species_name,
         size_class, total, biomass
        )

# create new column by merging site_code and depth
## why? now we get a unique identifier per site AND depth, which we need for calculating our env. measures
rls_sub$site_code_depth = paste(rls_sub$site_code, rls_sub$depth, sep = "_")

# subset the data with the information required to calculate the predictability etc for each location, depth, and survey time
rls_envpred = rls_sub %>%
  select(site_code_depth, latitude, longitude, survey_date) %>%
  distinct(site_code_depth, .keep_all = TRUE) %>% 
  arrange(site_code_depth)

#sstFiles = list.files(pattern="data*.csv")
#chlaFiles = list.files(pattern="data*.csv")