# COMBINE DATA FROM RLS (ABUNDANCE), LUIZ ET AL. (PLD), AND BARNECHE ET AL. (EGGSIZE)

# read libs
#library(rfishbase)
library(tidyverse)

# clean memory
rm(list=ls())

# set working directory
setwd("~/Documents/MSc_thesis")

# import data
## from hard drive
pld_raw = read_delim("/media/mari/Crucial X8/pld_luiz.csv", delim = ",")
pld_alz_raw = read_delim("/media/mari/Crucial X8/Alzateetal_2018_EE_data_final.csv", delim = ",")
rls_avg = read_delim("/media/mari/Crucial X8/rls_2021_2022_avg.csv", delim = ",")
## from Barneche's GitHub repo
eggsize_raw = read.csv("https://raw.githubusercontent.com/dbarneche/fishEggSize/master/data/fishEggsMSData.csv")

# RLS DATA
## separate species name in rls data
rls_avg_sep = rls_avg %>% 
  separate(species_name, c("Genus", "Species"), extra = "merge", fill = "left")

# PLD DATA
pld_subset = pld_raw %>% 
  select(-c("Region", "Reference")) %>% 
  mutate(Ref_pld = rep("Luiz2013", length(pld_raw$Family)))

# PLD2 DATA
pld_alz_subset = pld_alz_raw %>% 
  select(Species, PLD, Egg, BodySize) %>%
  separate(Species, c("Genus", "Species"), extra = "merge", fill = "left") %>% 
  mutate(Ref_pld = rep("Alzate2018", length(pld_alz_raw$Family))) %>% 
  mutate(Ref_bodysize = rep("Alzate2018", length(pld_alz_raw$Family)))

# EGGSIZE DATA
eggsize_subset = eggsize_raw %>% 
  select(Species, spawningMode, FemaleSize_mm, eggSize_mm) %>% 
  separate(Species, c("Genus", "Species"), extra = "merge", fill = "left") %>% 
  group_by(Genus, Species) %>% 
  summarise(mean_eggsize = mean(eggSize_mm)) %>% 
  ungroup() %>% 
  mutate(Ref_eggsize = rep("Barneche2018", length(mean_eggsize)))

# merge all datasets
df_list <- list(rls_avg_sep, pld_subset, eggsize_subset)      
trait_data = df_list %>% 
  reduce(left_join, by = c("Genus", "Species"))

# rename column names in trait data
trait_data = trait_data %>% 
  rename(RangeSize = `Range size (km)`,
         BodySize = `Body size (cm)`,
         PLD = `Mean PLD (days)`,
         DepthRange = `Depth range (m)`)

# replace pld NAs with data from pld,
# replace BodySize NAs with bodysize from pld,
# swap refs accordingly
x <- trait_data

for (i in 1:nrow(trait_data)) {
  if (is.na(trait_data[i, "PLD"]) | is.na(trait_data[i, "BodySize"])) {
    curr_genus = trait_data[[i, "Genus"]]
    curr_species = trait_data[[i, "Species"]]
    pld_rows = subset(pld_alz_subset,
                      pld_alz_subset[, "Genus"] == curr_genus &
                        pld_alz_subset[, "Species"] == curr_species)
    
    if (is.na(trait_data[i, "PLD"]) & nrow(pld_rows) > 0 & !is.na(pld_rows[1, "PLD"])) {
      trait_data[i, "PLD"] = pld_rows[1, "PLD"]
      trait_data[i, "Ref_pld"] = pld_rows[1, "Ref_pld"]
    }
    
    if (is.na(trait_data[i, "BodySize"]) & nrow(pld_rows) > 0 & !is.na(pld_rows[1, "BodySize"])) {
      trait_data[i, "BodySize"] = pld_rows[1, "BodySize"]
      trait_data[i, "Ref_bodysize"] = pld_rows[1, "Ref_bodysize"]
    }
  }
}

# # hehe let's see how many complete observations we have...
# length(unique(rls_avg$species_name)) # 1136
# nrow(na.omit(trait_data)) # ... well, well..
# sum(!is.na(trait_data$eggSize_mm))

# summary(trait_data %>%
#   select(Genus, Species, eggSize_mm, `Mean PLD (days)`) %>% 
#   distinct(Genus, Species, .keep_all = TRUE))
#   
  

# Data sources:
## RLS:
## Reef Life Survey (2022). Public Website, www.reeflifesurvey.com

## PLD: 
## Luiz, O. J., Allen, A. P., Robertsson, D. R. & Madin, J. S. (2013). 
## Adult and larval traits as determinants of geographic range size among tropical reef fishes. 
## Proceedings of the National Academy of Sciences of the United States of America, 110(41), 16498-16502. 
## https://doi.org/10.1073/pnas.1304074110

## eggsize: 
## Barneche, D. R., Burgess, S. C. & Marshall, D. J. (2018). 
## Global environmental drivers of marine fish egg size. 
## Global Ecology and Biogeography, 27(8), 890-898. 
## https://doi.org/10.1111/geb.12748
