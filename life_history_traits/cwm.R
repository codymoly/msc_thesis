# CALCULATION OF CWMs

# read libs
library(tidyverse)
library(vegan)

# clean memory
rm(list=ls())

# set working directory
setwd("~/Documents/MSc_thesis")

# import data
## from hard drive
survey_traits = read_delim("/media/mari/Crucial X8/species_traits_per_survey.csv", delim = ",")
rls_avg = read_delim("/media/mari/Crucial X8/rls_2021_2022_avg.csv", delim = ",")
species_traits = read_delim("/media/mari/Crucial X8/species_traits_imputed.csv", delim = ",")

# select columns of survey data
rls_avg_ordered = survey_traits[,c(
  "latitude", "longitude", "survey_date",
  "class", "order", "family", "genus", "species",
  "valid_genus", "valid_species",
  "biomass_mean", "size_class_mean", "total_mean"      
)]

# round numeric values, but this needs to be checked, maybe it's redundant
rls_avg_ordered = data.frame(lapply(rls_avg_ordered, function(x) if(is.numeric(x)) round(x, 1) else x))

# select columns of trait data
species_traits_subset = species_traits %>% 
  select(genus, species, bodySize, PLD, rangeSize, ref_bodysize, ref_pld, ref_rangesize)

# merge datasets
complete_trait_data = rls_avg_ordered %>% 
  left_join(species_traits_subset, by = c("genus", "species"))

# copy df
cwm_input = complete_trait_data

# merge genus and species
cwm_input$species_name <- paste(cwm_imput$genus, cwm_imput$species, sep = " ")

# select columns
cwm_input = cwm_input %>% 
  select(latitude, longitude, survey_date, species_name,
         biomass_mean, total_mean, bodySize, PLD, rangeSize)

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

# round numeric values, but this needs to be checked, maybe it's redundant
trait_cwm = data.frame(lapply(trait_cwm, function(x) if(is.numeric(x)) round(x, 2) else x))

# summarise stats
summary(trait_cwm)

# save data
write.csv(trait_cwm,"~/projects/msc_thesis/data/cwm_data.csv", row.names = FALSE)
write.csv(trait_cwm,"/media/mari/Crucial X8/cwm_data.csv", row.names = FALSE)
