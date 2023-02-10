# RLS DATA
## cleaning, subset sites, retrieving accepted species names

# load libraries
library(worms)
library(tidyverse)
library(stringr)

# clean memory
rm(list=ls())

# set working directory
setwd("~/projects/msc_thesis")

# conditional stuff
find_suspect_worms = FALSE
save_species_data = FALSE
save_site_data = TRUE

# read RLS dataset
rls_2019_2022_raw = read_delim("/media/mari/Crucial X8/rls_2019_2022_upd.csv", skip = 71, delim = ",")


# data prep
## remove redundant animals
rls_2019_2022 = rls_2019_2022_raw %>% 
  dplyr::filter(rls_2019_2022_raw$class == "Actinopterygii")
## 223568-222330=1238 rows are gone

## quick check on how many sites we have left after removing irrelevant classes
nrow(unique(rls_2019_2022[c('latitude', 'longitude', 'survey_date')])) # 1249 observations
## alternative:
## rls_2019_2022_raw %>% select(latitude, longitude, survey_date) %>% n_distinct()

## number of families and species
nrow(unique(rls_2019_2022["family"])) # 97
nrow(unique(rls_2019_2022["species_name"])) # 1281

## remove brackets from species name, e.g., change Pomacentrus sp. [rhodonotus] to Pomacentrus rhodonotus
## write function that explaces the different signs
rmBrackets = function(spname){
  return(str_replace_all(spname, "sp\\. \\[([^\\\\]*)\\]", "\\1"))
}
## apply function on species column in original file
rls_2019_2022["species_name"] <- lapply(rls_2019_2022["species_name"], rmBrackets)

## select unique species
unique_species_names = rls_2019_2022 %>%
  distinct(species_name)
### number of species: 1280

# get accepted names from worms
## note, this part is conditional as it only exists to retrieve missing data
if (find_suspect_worms == TRUE) {
  ## transform unique species into vector
  species_vector = unique_species_names %>% 
    #slice_head(n = 5) %>% # only to test how worms works for 5 species
    pull()
  
  ## get worms data based on species name
  worms_data = wormsbynames(species_vector)
  
  ## ...and subset aphia_id, species name, and accepted name
  worms_valid = worms_data %>% 
    select(AphiaID, valid_AphiaID, scientificname, valid_name, status) %>% 
    dplyr::filter(!is.na(scientificname)) %>%
    dplyr::rename(aphia_id = AphiaID,
                  species_name = scientificname)
  
  ## create dataframe with initial species names and worms data
  species_data = left_join(unique_species_names, worms_valid, by = "species_name")
  
  
  # sanity check
  ## write rows with missing valid name into dataframe
  unid_species = dplyr::filter(species_data, is.na(species_data$valid_name))
  
  ## filter species that are not classified as spp. (those will be imputed)
  false_spname = unid_species %>%
    dplyr::filter(!str_detect(species_name, regex("spp.", ignore_case = TRUE)))
  ### some contain "cf", which means that these species were hard to identify
  ### for the thesis, we will assume that they were correctly identified
  ### some have still sp. as species level ID
  ### we will treat those as the ones for which we will impute the trait data
} else {
  print("Suspect worms are already known!")
} ###  false_spname contains the names that need to be replaced


# overwrite weird species names
## retrieve names for species with ambiguous names manually from WORMS and replace those in the data set
# Acanthurus grammoptilus (cf)
rls_2019_2022$species_name[rls_2019_2022$species_name == "Acanthurus grammoptilus (cf)"] = "Acanthurus grammoptilus"
# Glyptoparus Browse
rls_2019_2022$species_name[rls_2019_2022$species_name == "Glyptoparus Browse"] = "Glyptoparus sp."
# Nesogobius sp. 4 [groovedcheek]
rls_2019_2022$species_name[rls_2019_2022$species_name == "Nesogobius sp. 4 [groovedcheek]"] = "Nesogobius sp."
# Eviota green
rls_2019_2022$species_name[rls_2019_2022$species_name == "Eviota green"] = "Eviota sp."
# Corythoichthys 1 RK
rls_2019_2022$species_name[rls_2019_2022$species_name == "Corythoichthys 1 RK"] = "Corythoichthys sp."
# Ostorhinchus cf cyanosoma
rls_2019_2022$species_name[rls_2019_2022$species_name == "Ostorhinchus cf cyanosoma"] = "Ostorhinchus cyanosoma"
# Nesogobius sp. 3 [speckled]
rls_2019_2022$species_name[rls_2019_2022$species_name == "Nesogobius sp. 3 [speckled]"] = "Nesogobius sp."
# Trimma white nose
rls_2019_2022$species_name[rls_2019_2022$species_name == "Trimma white nose"] = "Trimma sp."
# Pleurosicya soft coral
rls_2019_2022$species_name[rls_2019_2022$species_name == "Pleurosicya soft coral"] = "Pleurosicya sp."
# Eviota sp. (fasciola)
rls_2019_2022$species_name[rls_2019_2022$species_name == "Eviota sp. (fasciola)"] = "Eviota fasciola"
# Eviota Snowflake
rls_2019_2022$species_name[rls_2019_2022$species_name == "Eviota Snowflake"] = "Eviota sp."
# Tripterygiidae Ningaloo
rls_2019_2022$species_name[rls_2019_2022$species_name == "Tripterygiidae Ningaloo"] = "Tripterygiidae sp."
# Ecsenius black eye
rls_2019_2022$species_name[rls_2019_2022$species_name == "Ecsenius black eye"] = "Ecsenius sp."
# Hypoplectrodes Lord Howe
rls_2019_2022$species_name[rls_2019_2022$species_name == "Hypoplectrodes Lord Howe"] = "Hypoplectrodes sp."
# Nesogobius sp. 2
rls_2019_2022$species_name[rls_2019_2022$species_name == "Nesogobius sp. 2"] = "Nesogobius sp."


# re-run worms script
## extract unique species with new names
unique_species_names_2 = rls_2019_2022 %>%
  distinct(species_name)

## species vector
species_vector_2 = unique_species_names_2 %>% 
  pull()

## get worms data based on species name
worms_data_2 = wormsbynames(species_vector_2)

## consolidate
# worms_data_cons_2 = wormsconsolidate(worms_data_2)

## retrieve accepted names
#worms_data_acc_2 = wormsaccepted(worms_data_cons_2)

## ...and subset aphia_id, species name, and accepted name
worms_valid_2 = worms_data_2 %>% 
  select(AphiaID, valid_AphiaID, scientificname, valid_name) %>% 
  dplyr::filter(!is.na(scientificname)) %>%
  dplyr::rename(aphia_id = AphiaID,
                species_name = scientificname)

## create dataframe with initial species names and worms data
species_data_2 = left_join(unique_species_names_2, worms_valid_2, by = "species_name")



# sanity check
## write rows with missing valid name into dataframe
unid_species_2 = dplyr::filter(species_data_2, is.na(species_data_2$valid_name))

## filter species that are not classified as sp. or spp. (those will be imputed)
false_spname_2 = unid_species_2 %>%
  dplyr::filter(!str_detect(species_name, regex("sp?p\\.", ignore_case = TRUE)))


# add cleaned worms data to original RLS dataframe
clean_rls_data = left_join(rls_2019_2022, species_data_2, by = "species_name")


# save species data only
if (save_species_data == TRUE) {
  write.csv(species_data_2,"~/projects/msc_thesis/data/species_clean.csv", row.names = FALSE)
  write.csv(species_data_2,"/media/mari/Crucial X8/species_clean.csv", row.names = FALSE)
} else {
  print("Data not saved!")
}

# save site-specific data
if (save_site_data == TRUE) {
  write.csv(clean_rls_data,"~/projects/msc_thesis/data/rls_2019_2022_clean.csv", row.names = FALSE)
  write.csv(clean_rls_data,"/media/mari/Crucial X8/rls_2019_2022_clean.csv", row.names = FALSE)
} else {
  print("Data not saved!")
}