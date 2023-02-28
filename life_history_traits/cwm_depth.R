# CALCULATION OF CWMs

# read libs
library(tidyverse)
library(vegan)
library(modi) # weighted.var function

# clean memory
rm(list=ls())

# set working directory
setwd("~/Documents/MSc_thesis")

# import data
rls_avg = readr::read_delim("/media/mari/Crucial X8/rls_2019_2022_avg_10m.csv", delim = ",")


###### data preparation

# site data
## select columns of survey data
rls_avg_subset = rls_avg %>% 
  dplyr::select(latitude, longitude, survey_date, species_name, valid_name, biomass_mean, total_mean, size_class_mean)

## count sites
nrow(rls_avg_subset 
     %>% dplyr::select(latitude, longitude, survey_date) 
     %>% dplyr::distinct()
) # we have 1249 survey sites

# copy merged dataset
cwm_input = rls_avg_subset

###### community measures

# caluclate community measures
trait_cwm = cwm_input %>%
  dplyr::group_by(latitude, longitude, survey_date) %>%  
  dplyr::summarise(
    shannon = diversity(total_mean,index = "shannon"),
    number_total = sum(na.omit(total_mean)),
    sp_richness = vegan::specnumber(species_name),
    even_total = shannon/log(number_total),
    size_class_cwm = stats::weighted.mean(size_class_mean, total_mean),
    size_class_cwv = modi::weighted.var(size_class_mean, total_mean),
    total_biomass = sum(na.omit(biomass_mean))
    #bodysize_cwm_biomass = weighted.mean(bodySize, biomass_mean, na.rm = TRUE),
    #bodysize_cwv_biomass = weighted.var(bodySize, biomass_mean, na.rm = TRUE)
  ) %>% 
  dplyr::ungroup()

# round numeric values
trait_cwm = data.frame(lapply(trait_cwm, function(x) if(is.numeric(x)) round(x, 2) else x))

# check if data is missing
nrow(trait_cwm[!complete.cases(trait_cwm),]) # nice, no missing data


###### save data

write.csv(trait_cwm,"~/projects/msc_thesis/data/cwm_10m_data.csv", row.names = FALSE)
write.csv(trait_cwm,"/media/mari/Crucial X8/cwm_10m_data.csv", row.names = FALSE)



