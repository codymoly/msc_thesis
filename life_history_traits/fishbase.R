# FISHBASE: GET TRAIT DATA

# read libs
library(rfishbase)
library(tidyverse)

# clean memory
rm(list=ls())

# set working directory
setwd("~/Documents/MSc_thesis")

# import data
## from hard drive
rls_avg = read_delim("/media/mari/Crucial X8/rls_2021_2022_avg.csv", delim = ",")

# # select column with species names, only for uploading list on github
# species_names = select(rls_avg, species_name)
# write.csv(species_names,"~/projects/msc_thesis/data/rls_species_names.csv", row.names = FALSE)

# write unique species names into a list
species_list = rls_avg %>%
  distinct(valid_name) %>%
  pull(valid_name) %>%
  as.list

# get fb traits
## check available variables
names(fb_tbl("species"))
## download length/ bodysize data for all RLS species
fb_traits = fb_tbl("species", server = "fishbase") %>%
  mutate(valid_name = paste(Genus, Species)) %>%
  filter(valid_name %in% species_list) %>%
  select(valid_name, Length) %>% 
  mutate(Ref_bodysize = rep("Fishbase2022", length(Length)))

# check, because 99 species are missing in the fb trait dataframe
## write species list into dataframe
species_df = rls_avg %>% 
  distinct(valid_name) %>% 
  select(valid_name)
## check which species (missing species) are in the list but not in fishbase
## it seems that some species just have 
missing_species = setdiff(species_df, fb_traits["valid_name"])

# save data
## fb traits
write.csv(fb_traits,"~/projects/msc_thesis/data/fishbase_bodysize.csv", row.names = FALSE)
write.csv(fb_traits,"/media/mari/Crucial X8/fishbase_bodysize.csv", row.names = FALSE)
## missing species
write.csv(missing_species,"~/projects/msc_thesis/data/fb_missing_species.csv", row.names = FALSE)
write.csv(missing_species,"/media/mari/Crucial X8/fb_missing_species.csv", row.names = FALSE)
