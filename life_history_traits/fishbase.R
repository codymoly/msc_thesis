####### DEPRICATED: FISHBASE: GET TRAIT DATA

# read libs
library(rfishbase)
library(tidyverse)

# clean memory
rm(list=ls())

# set working directory
setwd("~/Documents/MSc_thesis")

# conditional code execution
save_my_data = FALSE

# import data
rls_avg = read_delim("/media/mari/Crucial X8/rls_2019_2022_avg.csv", delim = ",")

# write unique species names into a list
species_list = rls_avg %>%
  distinct(valid_name) %>%
  pull(valid_name) %>%
  as.list

## check available variables
names(fb_tbl("species"))

## download length/ bodysize data for all RLS species
fb_traits = fb_tbl("species", server = "fishbase") %>%
  mutate(valid_name = paste(Genus, Species)) %>%
  filter(valid_name %in% species_list) %>%
  select(valid_name, Length) %>% 
  mutate(Ref_bodysize = rep("Fishbase2022", length(Length)))

## write species list into dataframe
species_df = rls_avg %>% 
  distinct(valid_name) %>% 
  select(valid_name)

## check which species (missing species) are in the list but not in the fishbase dataframe
missing_species = setdiff(species_df, fb_traits["valid_name"]) # 22 are missing

## dump solution, manual search for the unaccepted species and sizes
## numbers are taken from fishbase with validation of the name in WORMS
## in some cases, fishbase does not deliver the valid name, thus the species couldn't be found
Length = c(10, 36, 6, 6, 6, 10.2, 7, 8, 7.5, 9, 10, 4.9, 12, 11, 15, 16.5, 15, 70, 9, 17, 10)
valid_name = c("Stegastes lacrymatus",
               "Scorpaenopsis oxycephalus",
               "Pycnochromis vanderbilti",
               "Pycnochromis retrofasciatus",
               "Pycnochromis nigrurus",
               "Pycnochromis margaritifer",
               "Pycnochromis lineatus",
               "Pycnochromis iomelas",
               "Pycnochromis caudalis",
               "Pycnochromis atripes",
               "Pycnochromis amboinensis",
               "Pycnochromis agilis",
               "Plectroglyphidodon obreptus",
               "Plectroglyphidodon insularis",
               "Plectroglyphidodon gascoynei",
               "Plectroglyphidodon fasciolatus",
               "Plectroglyphidodon apicalis",
               "Ferdauia orthogrammus", # Carangoides ferdau, unaccepted
               "Azurina lepidolepis",
               "Amphiprion biaculeatus", # Chaetodon biaculeatus, unaccepted
               "Amblyglyphidodon batunaorum")
Ref_bodysize = rep("Fishbase2022", 21)

manual_fb_traits = data.frame(valid_name, Length, Ref_bodysize)

fb_traits_final = rbind(fb_traits, manual_fb_traits)

# save data
## fb traits
if (save_my_data == TRUE) {
  write.csv(fb_traits_final,"~/projects/msc_thesis/data/fishbase_bodysize.csv", row.names = FALSE)
  write.csv(fb_traits_final,"/media/mari/Crucial X8/fishbase_bodysize.csv", row.names = FALSE)
} else {
  print("Data was not saved!")
}

# ## missing species
# write.csv(missing_species,"~/projects/msc_thesis/data/fb_missing_species.csv", row.names = FALSE)
# write.csv(missing_species,"/media/mari/Crucial X8/fb_missing_species.csv", row.names = FALSE)
