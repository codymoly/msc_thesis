# FISHBASE: GET TRAIT DATA

# read libs
library(rfishbase)
library(tidyverse)

# clean memory
rm(list=ls())

# set working directory
# setwd("")

# import data
rls_species = read.csv("https://raw.githubusercontent.com/codymoly/msc_thesis/main/data/rls_species_names.csv")

## write unique species names into a list
species_list = rls_species %>%
  distinct(species_name) %>%
  pull(species_name) %>%
  as.list

## check for one species
fish = "Aioliops novaeguineae"
fb_tbl("species") %>%
  mutate(sci_name = paste(Genus, Species)) %>%
  filter(sci_name %in% fish) %>%
  select(sci_name, FBname, Length)

## get data for all RLS species
fb_tbl("species") %>%
  mutate(sci_name = paste(Genus, Species)) %>%
  filter(sci_name %in% species_list) %>%
  select(sci_name, FBname, Length)