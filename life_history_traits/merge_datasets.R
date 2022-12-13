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
rls_avg = read_delim("/media/mari/Crucial X8/rls_2021_2022_avg.csv", delim = ",")
## from Barneche's GitHub repo
eggsize_raw = read.csv("https://raw.githubusercontent.com/dbarneche/fishEggSize/master/data/fishEggsMSData.csv")

# # FishBase
# ## write unique species names into a list
# species_list = rls_avg %>%
#   distinct(species_name) %>%
#   pull(species_name) %>%
#   as.list
# 
# ## check for one species
# fish = "Aioliops novaeguineae"
# fb_tbl("species") %>% 
#   mutate(sci_name = paste(Genus, Species)) %>%
#   filter(sci_name %in% fish) %>% 
#   select(sci_name, FBname, Length)
# 
# ## check for all RLS species
# fb_tbl("species") %>% 
#   mutate(sci_name = paste(Genus, Species)) %>%
#   filter(sci_name %in% species_list) %>% 
#   select(sci_name, FBname, Length)

# RLS DATA
## separate species name in rls data
rls_avg_sep = rls_avg %>% 
  separate(species_name, c("Genus", "Species"), extra = "merge", fill = "left")

# PLD DATA
pld_subset = pld_raw %>% 
  select(-c("Region", "Reference")) %>% 
  mutate(Ref = rep("Luiz2013", length(pld_raw$Family)))

# EGGSIZE DATA
eggsize_subset = eggsize_raw %>% 
  select(Species, spawningMode, FemaleSize_mm, eggSize_mm) %>% 
  separate(Species, c("Genus", "Species"), extra = "merge", fill = "left") %>% 
  mutate(Ref = rep("Barneche2018", length(eggsize_raw$Family)))

# merge all datasets
df_list <- list(rls_avg_sep, pld_subset, eggsize_subset)      
trait_data = df_list %>% 
  reduce(left_join, by = c("Genus", "Species"))

# hehe let's see how many complete observations we have...
length(unique(rls_avg$species_name)) # 1136
nrow(na.omit(trait_data)) # ... well, well..
sum(!is.na(trait_data$eggSize_mm))

summary(trait_data %>%
  select(Genus, Species, eggSize_mm, `Mean PLD (days)`) %>% 
  distinct(Genus, Species, .keep_all = TRUE))
  
  

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
