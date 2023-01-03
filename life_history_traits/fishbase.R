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

## write unique species names into a list
# species_names = list(unique(rls_avg$species_name))
species_list = rls_avg %>%
  distinct(species_name) %>%
  pull(species_name) %>%
  as.list

## check for one species
fish = "Aioliops novaeguineae"
fb_tbl("species") %>%
  mutate(sci_name = paste(Genus, Species)) %>%
  filter(sci_name %in% fish) %>%
  select(sci_name, FBname, Length)

# ## check for all RLS species
# fb_tbl("species") %>% 
#   mutate(sci_name = paste(Genus, Species)) %>%
#   filter(sci_name %in% species_list) %>% 
#   select(sci_name, FBname, Length)