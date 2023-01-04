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
  distinct(species_name) %>%
  pull(species_name) %>%
  as.list

# download length/ bodysize data for all RLS species
fb_traits = fb_tbl("species") %>%
  mutate(species_name = paste(Genus, Species)) %>%
  filter(species_name %in% species_list) %>%
  select(species_name, Length) %>% 
  mutate(Ref_bodysize = rep("Fishbase2022", length(Length)))

write.csv(fb_traits,"~/projects/msc_thesis/data/fishbase_bodysize.csv", row.names = FALSE)
write.csv(fb_traits,"/media/mari/Crucial X8/fishbase_bodysize.csv", row.names = FALSE)
