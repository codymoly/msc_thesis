# COMBINE DATA FROM RLS (ABUNDANCE), LUIZ ET AL. (PLD), AND BARNECHE ET AL. (EGGSIZE)
######### test script
# read libs
library(tidyverse)

# clean memory
rm(list=ls())

# set working directory
setwd("~/Documents/MSc_thesis")

# conditional code
save_my_data = FALSE
save_my_data_species_only = FALSE

# import data
rls_avg = read_delim("/media/mari/Crucial X8/rls_2019_2022_avg.csv", delim = ",")
fb_raw = read_delim("/media/mari/Crucial X8/fishbase_bodysize.csv", delim = ",")


# RLS DATA
## separate species name in rls data
rls_avg_sep = rls_avg %>% 
  separate(species_name, c("Genus", "Species"), extra = "merge", fill = "left") %>% 
  separate(valid_name, c("valid_genus", "valid_species"), extra = "merge", fill = "left")

## backup dataset if loop kills it later
backup_rls_avg_sep = rls_avg_sep


# FISHBASE DATA
fb_bodysize = fb_raw %>%
  #select(-c("entry")) %>% 
  separate(valid_name, c("valid_genus", "valid_species"), extra = "merge", fill = "left") %>% 
  dplyr::rename(BodySize = Length,
                ref_bodysize = Ref_bodysize)


# MERGE RLS AND BODY SIZE DATA
df_list_1 = list(rls_avg_sep, 
                 fb_bodysize)      
trait_data_only_bs = df_list_1 %>%
  reduce(left_join, by = c("valid_genus", "valid_species"))


# DATA CLEANING
## rename column names
rls_only_bodysize = trait_data_only_bs %>% 
  dplyr::rename(
    genus = Genus,
    species = Species,
    bodySize = BodySize
  )

## change order of columns
final_only_bodysize = rls_only_bodysize[,c(
  "latitude", "longitude", "survey_date",
  "class", "order", "family", "genus", "species",
  "valid_genus", "valid_species",
  "biomass_mean", "size_class_mean", "total_mean", "bodySize"      
)]

## round values
final_only_bodysize = data.frame(lapply(final_only_bodysize, function(x) if(is.numeric(x)) round(x, 1) else x))

## subset data with unique species
unique_species_bs = final_only_bodysize %>% 
  select(-c("latitude", "longitude", "survey_date", "order", "biomass_mean", "size_class_mean", "total_mean")) %>% 
  distinct(genus, species, .keep_all = TRUE) %>% 
  arrange(class, family, genus, species)


# save data
## full dataset with traits and survey sites
if (save_my_data == TRUE) {
  write.csv(final_only_bodysize,"~/projects/msc_thesis/data/species_traits_per_survey.csv", row.names = FALSE)
  write.csv(final_only_bodysize,"/media/mari/Crucial X8/species_traits_per_survey.csv", row.names = FALSE)
} else {
  print("Data not saved!")
}

## only species and trait data
if (save_my_data_species_only == TRUE) {
  write.csv(unique_species_bs,"~/projects/msc_thesis/data/species_traits.csv", row.names = FALSE)
  write.csv(unique_species_bs,"/media/mari/Crucial X8/species_traits.csv", row.names = FALSE)
} else {
  print("Data not saved (species only)!")
}


