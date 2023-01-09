# CALCULATION OF CWMs

# read libs
library(tidyverse)
library(vegan)

# clean memory
rm(list=ls())

# set working directory
setwd("~/Documents/MSc_thesis")

# import data
rls_avg = read_delim("/media/mari/Crucial X8/rls_2021_2022_avg.csv", delim = ",")
species_traits = read_delim("/media/mari/Crucial X8/species_traits_imputed.csv", delim = ",")

# prepare site data
## select columns of survey data
rls_avg_subset = rls_avg %>% 
  select(latitude, longitude, survey_date, species_name, biomass_mean, total_mean)

# prepare trait data
## copdy df
species_traits_copy = species_traits

## merge genus and species
species_traits_copy$species_name <- paste(species_traits_copy$genus, species_traits_copy$species, sep = " ")

## select columns of trait data
species_traits_subset = species_traits_copy %>% 
  select(species_name, bodySize, PLD, rangeSize)

## merge datasets
complete_trait_data = rls_avg_subset %>% 
  left_join(species_traits_subset, by = c("species_name"))

## copy df
cwm_input = complete_trait_data

# caluclate cwms
trait_cwm = cwm_input %>%
  group_by(latitude, longitude, survey_date) %>%   # Groups the summary file by Plot number
  summarise(           # Coding for how we want our CWMs summarized
    bodysize_cwm_biomass = weighted.mean(bodySize, biomass_mean),   # Actual calculation of CWMs
    PLD_cwm_biomass = weighted.mean(PLD, biomass_mean),
    rangesize_cwm_biomass = weighted.mean(rangeSize, biomass_mean),
    bodysize_cwm_total = weighted.mean(bodySize, total_mean),
    PLD_cwm_total = weighted.mean(PLD, total_mean),
    rangesize_cwm_total = weighted.mean(rangeSize, total_mean),
    sp_richness = specnumber(total_mean),
    shannon = diversity(total_mean,index = "shannon"),
    simpson = diversity(total_mean, index = "simpson"),
    inv_simpson = diversity(total_mean, index = "invsimpson")
  ) %>% 
  ungroup()

# check if data is missing
data_with_na = trait_cwm[!complete.cases(trait_cwm),]
# rls_avg_subset[,!is.na("total_mean")] --> we have missing biomass data, but total counts are complete, so rather use those?

# round numeric values, but this needs to be checked, maybe it's redundant
trait_cwm = data.frame(lapply(trait_cwm, function(x) if(is.numeric(x)) round(x, 2) else x))

# summarise stats
summary(trait_cwm)

# save data
write.csv(trait_cwm,"~/projects/msc_thesis/data/cwm_data.csv", row.names = FALSE)
write.csv(trait_cwm,"/media/mari/Crucial X8/cwm_data.csv", row.names = FALSE)
