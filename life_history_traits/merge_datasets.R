# COMBINE DATA FROM RLS (ABUNDANCE), LUIZ ET AL. (PLD), AND BARNECHE ET AL. (EGGSIZE)

# read libs
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

# RLS DATA
## separate species name in rls data
rls_avg_sep = rls_avg %>% 
  separate(species_name, c("Genus", "Species"), extra = "merge", fill = "left")

# PLD DATA
pld_subset = pld_raw %>% 
  select(-c("Region", "Reference"))

# EGGSIZE DATA
eggsize_subset = eggsize_raw %>% 
  select(Species, spawningMode, FemaleSize_mm, eggSize_mm) %>% 
  separate(Species, c("Genus", "Species"), extra = "merge", fill = "left")

# merge all datasets
df_list <- list(rls_avg_sep, pld_subset, eggsize_subset)      
trait_data = df_list %>% 
  reduce(full_join, by=c("Genus","Species"))

# hehe let's see how many complete observations we have...
nrow(na.omit(trait_data)) # ... well, well...


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
