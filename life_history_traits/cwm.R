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
  dplyr::filter(class == "Actinopterygii") %>% 
  select(latitude, longitude, survey_date, species_name, biomass_mean, total_mean)

nrow(rls_avg_subset 
     %>% select(latitude, longitude, survey_date) 
     %>% distinct()
     ) # despite removing all rows with elasmobranchs, we have 490 survey sites

# prepare trait data
## copdy df
species_traits_copy = species_traits

## merge genus and species (as in the initial RLS dataset)
species_traits_copy$species_name = paste(species_traits_copy$genus, species_traits_copy$species, sep = " ")

## select columns of trait data
species_traits_subset = species_traits_copy %>% 
  select(species_name, bodySize, PLD)

# merge datasets
rls_trait_data = rls_avg_subset %>% 
  left_join(species_traits_subset, by = c("species_name"))

# data check-up, total cases: 20633
nrow(rls_trait_data[complete.cases(rls_trait_data),]) # 20260 complete cases
nrow(rls_trait_data[complete.cases(rls_trait_data$biomass_mean),]) # 20260 complete cases with biomass
nrow(rls_trait_data[complete.cases(rls_trait_data$total_mean),]) # 20633 complete cases with total counts
nrow(rls_trait_data[complete.cases(rls_trait_data$bodySize),]) # 20633 complete cases with bodysize
nrow(rls_trait_data[complete.cases(rls_trait_data$PLD),]) # 20633 complete cases with PLD

## copy merged dataset
cwm_input = rls_trait_data

## check if biomass and total counts are complete
nrow(cwm_input[complete.cases(cwm_input$biomass_mean),]) # 20260, incomplete
nrow(cwm_input[complete.cases(cwm_input$total_mean),]) # 20633, complete
## --> use total observations for weighted means

# caluclate cwms
trait_cwm = cwm_input %>%
  group_by(latitude, longitude, survey_date) %>%   # Groups the summary file by Plot number
  summarise(           # Coding for how we want our CWMs summarized
    bodysize_cwm_total = weighted.mean(bodySize, total_mean),
    PLD_cwm_total = weighted.mean(PLD, total_mean),
    total_biomass = sum(na.omit(biomass_mean)),
    sp_richness = specnumber(total_mean),
    shannon = diversity(total_mean,index = "shannon"),
    simpson = diversity(total_mean, index = "simpson"),
    inv_simpson = diversity(total_mean, index = "invsimpson")
  ) %>% 
  ungroup()

# check if data is missing
nrow(trait_cwm[!complete.cases(trait_cwm),]) # nice, no missing data

# round numeric values, but this needs to be checked, maybe it's redundant
trait_cwm = data.frame(lapply(trait_cwm, function(x) if(is.numeric(x)) round(x, 2) else x))

# summarise stats
summary(trait_cwm)

# save data
write.csv(trait_cwm,"~/projects/msc_thesis/data/cwm_data.csv", row.names = FALSE)
write.csv(trait_cwm,"/media/mari/Crucial X8/cwm_data.csv", row.names = FALSE)
