# FISHBASE: GET TRAIT DATA

# read libs
library(rfishbase)
library(tidyverse)

# clean memory
rm(list=ls())

# set working directory
setwd("~/Documents/MSc_thesis")

# conditional code execution
save_trait_data = FALSE

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

# # get fb larvae traits
# species_vec = rls_avg %>%
#   distinct(valid_name) %>% 
#   pull(valid_name)
#
# larvae_traits = larvae(
#   species_list = species_vec,
#   fields = NULL,
#   server = getOption("FISHBASE_API", "fishbase"))
# 
# larvae_traits_sub = larvae_traits %>% 
#   select(Species, starts_with("LarvalDurationMod")) %>% 
#   rename(valid_name = Species,
#          Ref_larvalduration = LarvalDurationModRef)

## check available variables
names(fb_tbl("species"))
## download length/ bodysize data for all RLS species
fb_traits = fb_tbl("species", server = "fishbase") %>%
  mutate(valid_name = paste(Genus, Species)) %>%
  filter(valid_name %in% species_list) %>%
  select(valid_name, Length) %>% 
  mutate(Ref_bodysize = rep("Fishbase2022", length(Length)))

# # merge larvae and length data
#fb_traits_compl = full_join(fb_traits, larvae_traits_sub, by = "valid_name")

# 10 species are missing
## write species list into dataframe
species_df = rls_avg %>% 
  distinct(valid_name) %>% 
  select(valid_name)
## check which species (missing species) are in the list but not in the fishbase dataframe
missing_species = setdiff(species_df, fb_traits["valid_name"])

## dump solution, manual search for the unaccepted species and sizes
Length = c(122, 35, 38, 30, 20, 16.5, 8, 10, 10)
valid_name = c("Pseudocaranx georgianus",
               "Cheilodactylus vestitus",
               "Thysanophrys cirronasa",
               "Cheilodactylus gibbosus",
               "Cheilodactylus ephippium",
               "Plectroglyphidodon fasciolatus",
               "Pycnochromis iomelas",
               "Stegastes lacrymatus",
               "Amblyglyphidodon batunaorum")
Ref_bodysize = rep("Fishbase2022", 9)

manual_fb_traits = data.frame(valid_name, Length, Ref_bodysize)

fb_traits_final = rbind(fb_traits, manual_fb_traits)

# Pseudocaranx georgianus 122
# Cheilodactylus vestitus 35
# Thysanophrys cirronasa 38
# Cheilodactylus gibbosus 30
# Cheilodactylus ephippium 20
# Plectroglyphidodon fasciolatus 16.5
# Pycnochromis iomelas 8
# Stegastes lacrymatus 10
# Amblyglyphidodon batunaorum 10

# save data
## fb traits
if (save_trait_data == TRUE) {
  write.csv(fb_traits_final,"~/projects/msc_thesis/data/fishbase_bodysize.csv", row.names = FALSE)
  write.csv(fb_traits_final,"/media/mari/Crucial X8/fishbase_bodysize.csv", row.names = FALSE)
} else {
  print("Data was not saved!")
}

# ## missing species
# write.csv(missing_species,"~/projects/msc_thesis/data/fb_missing_species.csv", row.names = FALSE)
# write.csv(missing_species,"/media/mari/Crucial X8/fb_missing_species.csv", row.names = FALSE)
