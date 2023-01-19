# RLS DATA
## cleaning

# load libraries
library(worms)
library(tidyverse)

# clean memory
rm(list=ls())

# set working directory
setwd("~/projects/msc_thesis")

# conditional stuff
save_date = FALSE

# read RLS dataset
rls_2021_2022 = read_delim("/media/mari/Crucial X8/RLS_2021_2022.csv", skip = 71, delim = ",")

# remove redundant animals
rls_2021_2022 = rls_2021_2022 %>% 
  dplyr::filter(rls_2021_2022$class == "Actinopterygii" | rls_2021_2022$class == "Elasmobranchii")

# data exploration
# n_distinct(rls_2021_2022$survey_id) # 1276, identifier for each survey, note that we don't differ between depths
# n_distinct(rls_2021_2022$site_code) # 475, identifier for each location
# n_distinct(rls_2021_2022$FID) # 91410, identifier for each line
# # unique(rls_2021_2022$ecoregion)
# nrow(unique(rls_2021_2022[c('latitude', 'longitude', 'survey_date')])) # 490 observations

# number of families and species incl. elasmos
nrow(unique(rls_2021_2022["family"])) # 104
nrow(unique(rls_2021_2022["species_name"])) # 1121

# number of families and species excl. elasmos
rls_no_elas = rls_2021_2022 %>% 
  dplyr::filter(rls_2021_2022$class == "Actinopterygii")

nrow(unique(rls_no_elas["family"])) # 89
nrow(unique(rls_no_elas["species_name"])) # 1081

## remove brackets from species name, e.g., change Pomacentrus sp. [rhodonotus] to Pomacentrus rhodonotus
## write function that explaces the different signs
rmBrackets = function(spname){
  return(str_replace_all(spname, "sp\\. \\[([^\\\\]*)\\]", "\\1"))
}
## apply function on species column in original file
rls_2021_2022["species_name"] <- lapply(rls_2021_2022["species_name"], rmBrackets)

# get accepted species names
## select unique species
unique_species_names = rls_2021_2022 %>%
  distinct(species_name)
## correct typo
unique_species_names$species_name[unique_species_names$species_name == "Tripterygiidae Ningaloo"] = "Tripterygiidae ningaloo"

## transform into vector
species_vector = unique_species_names %>% 
  #slice_head(n = 5) %>% # only to test how worms works for 5 species
  pull()
## get accepted names from worms database...
worms_data = wormsbynames(species_vector)
## ...and subset aphia_id, species name, and accepted name
worms_valid = worms_data %>% 
  select(AphiaID, scientificname, valid_name) %>% 
  dplyr::filter(!is.na(scientificname)) %>%
  dplyr::rename(aphia_id = AphiaID,
                species_name = scientificname)

clean_rls_data = left_join(rls_2021_2022, worms_valid, by = "species_name")

if (save_data == TRUE) {
  write.csv(clean_rls_data,"~/projects/msc_thesis/data/rls_2021_2022_clean.csv", row.names = FALSE)
  write.csv(clean_rls_data,"/media/mari/Crucial X8/rls_2021_2022_clean.csv", row.names = FALSE)
} else {
  print("Data not saved!")
}
