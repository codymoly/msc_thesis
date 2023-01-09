# COMBINE DATA FROM RLS (ABUNDANCE), LUIZ ET AL. (PLD), AND BARNECHE ET AL. (EGGSIZE)
######### test script
# read libs
library(tidyverse)

# clean memory
rm(list=ls())

# set working directory
setwd("~/Documents/MSc_thesis")

# import data
## from hard drive
rls_avg = read_delim("/media/mari/Crucial X8/rls_2021_2022_avg.csv", delim = ",")
pld_raw = read_delim("/media/mari/Crucial X8/pld_luiz.csv", delim = ",")
pld_alz_raw = read_delim("/media/mari/Crucial X8/Alzateetal_2018_EE_data_final.csv", delim = ",")
fb_raw = read_delim("/media/mari/Crucial X8/fishbase_bodysize.csv", delim = ",")
## from Barneche's GitHub repo
eggsize_raw = read.csv("https://raw.githubusercontent.com/dbarneche/fishEggSize/master/data/fishEggsMSData.csv")

# RLS DATA
## separate species name in rls data
rls_avg_sep = rls_avg %>% 
  separate(species_name, c("Genus", "Species"), extra = "merge", fill = "left") %>% 
  separate(valid_name, c("valid_genus", "valid_species"), extra = "merge", fill = "left")

## backup dataset if loop kills it later
backup_rls_avg_sep = rls_avg_sep

# PLD DATA
pld_subset = pld_raw %>% 
  select(-c("Family", "Region", "Multi-Habitat", "Transpacific", "Reference")) %>% 
  mutate(ref_pld = rep("Luiz2013", length(pld_raw$Genus)),
         ref_bodysize = rep("Luiz2013", length(pld_raw$Genus))) %>% 
  rename(RangeSize = `Range size (km)`,
         BodySize = `Body size (cm)`,
         PLD = `Mean PLD (days)`,
         DepthRange = `Depth range (m)`,
         spawningMode = `Spawn`,
         nocturnal = `Nocturnal`)

## replace spawning classes by strings used in the eggsize dataset
pld_subset$spawningMode = gsub('PEL', 'pelagic', pld_subset$spawningMode)
pld_subset$spawningMode = gsub('DEM', 'demersal', pld_subset$spawningMode)

# PLD ALZATE et al. DATA
pld_alz_subset = pld_alz_raw %>% 
  select(Species, nocturnal, PLD, Egg, BodySize) %>%
  separate(Species, c("Genus", "Species"), extra = "merge", fill = "left") %>% 
  mutate(ref_pld = rep("Alzate2018", length(pld_alz_raw$Genus)),
         ref_bodysize = rep("Alzate2018", length(pld_alz_raw$Genus)))

## replace nocturnal logicals by strings used in the other pld dataset
pld_alz_subset$nocturnal = gsub('yes', 'YES', pld_alz_subset$nocturnal)
pld_alz_subset$nocturnal = gsub('no', 'NO', pld_alz_subset$nocturnal)

# FISHBASE DATA
fb_bodysize = fb_raw %>%
  #select(-c("entry")) %>% 
  separate(valid_name, c("valid_genus", "valid_species"), extra = "merge", fill = "left") %>% 
  rename(BodySize = Length) %>% 
  rename(ref_bodysize = Ref_bodysize)

# EGGSIZE DATA
eggsize_subset = eggsize_raw %>% 
  select(Species, spawningMode, FemaleSize_mm, eggSize_mm) %>% 
  separate(Species, c("Genus", "Species"), extra = "merge", fill = "left") %>% 
  group_by(Genus, Species) %>% 
  summarise(eggsize_mean = mean(eggSize_mm)) %>% 
  ungroup() %>% 
  mutate(ref_eggsize = rep("Barneche2018", length(eggsize_mean)))

# merge all datasets
df_list <- list(rls_avg_sep, pld_subset, eggsize_subset, pld_alz_subset[c("Genus", "Species", "Egg")])      
trait_data = df_list %>% 
  reduce(left_join, by = c("Genus", "Species"))

# complement pld and bodysize data from Alzate
# replace pld NAs with data from pld,
# replace BodySize NAs with bodysize from pld,
# swap refs accordingly

for (i in 1:nrow(trait_data)) {
  if (is.na(trait_data[i, "PLD"]) | is.na(trait_data[i, "BodySize"])) {
    curr_genus = trait_data[[i, "Genus"]]
    curr_species = trait_data[[i, "Species"]]
    pld_rows = subset(pld_alz_subset,
                      pld_alz_subset[, "Genus"] == curr_genus &
                        pld_alz_subset[, "Species"] == curr_species)
    
    if (is.na(trait_data[i, "PLD"]) & nrow(pld_rows) > 0 & !is.na(pld_rows[1, "PLD"])) {
      trait_data[i, "PLD"] = pld_rows[1, "PLD"]
      trait_data[i, "ref_pld"] = pld_rows[1, "ref_pld"]
    }
    
    if (is.na(trait_data[i, "BodySize"]) & nrow(pld_rows) > 0 & !is.na(pld_rows[1, "BodySize"])) {
      trait_data[i, "BodySize"] = pld_rows[1, "BodySize"]
      trait_data[i, "ref_bodysize"] = pld_rows[1, "ref_bodysize"]
    }
    
    if (is.na(trait_data[i, "nocturnal"]) & nrow(pld_rows) > 0 & !is.na(pld_rows[1, "nocturnal"])) {
      trait_data[i, "nocturnal"] = pld_rows[1, "nocturnal"]
    }
  }
}

# make copy of df
trait_data_2 = trait_data

# complement bodysize data from fishbase
for (i in 1:nrow(trait_data_2)) {
  if (is.na(trait_data_2[i, "BodySize"])) {
    curr_genus = trait_data_2[[i, "valid_genus"]]
    curr_species = trait_data_2[[i, "valid_species"]]
    bodysize_rows = subset(fb_bodysize,
                           fb_bodysize[, "valid_genus"] == curr_genus &
                             fb_bodysize[, "valid_species"] == curr_species)
    
    if (is.na(trait_data_2[i, "BodySize"]) & nrow(bodysize_rows) > 0 & !is.na(bodysize_rows[1, "BodySize"])) {
      trait_data_2[i, "BodySize"] = bodysize_rows[1, "BodySize"]
      trait_data_2[i, "ref_bodysize"] = bodysize_rows[1, "ref_bodysize"]
    }
  }
}

# clean data
## rename column names
trait_data_2 = trait_data_2 %>% 
  rename(
    genus = Genus,
    species = Species,
    rangeSize = RangeSize,
    bodySize = BodySize,
    depthRange = DepthRange,
    schooler = Schooler,
    egg = Egg
  )

## change order of columns
final_trait_data = trait_data_2[,c(
  "latitude", "longitude", "survey_date",
  "class", "order", "family", "genus", "species",
  "valid_genus", "valid_species",
  "biomass_mean", "size_class_mean", "total_mean",
  "bodySize", "PLD", "rangeSize", "depthRange", "eggsize_mean", "egg",
  "spawningMode", "schooler", "nocturnal",
  "ref_bodysize", "ref_pld", "ref_eggsize"      
)]

# round values
final_trait_data <- data.frame(lapply(final_trait_data, function(x) if(is.numeric(x)) round(x, 1) else x))

# subset data with unique species
unique_species = final_trait_data %>% 
  select(-c("latitude", "longitude", "survey_date", "order", "biomass_mean", "size_class_mean", "total_mean")) %>% 
  distinct(genus, species, .keep_all = TRUE) %>% 
  arrange(class, family, genus, species)

# write data
## full dataset with traits and survey sites
write.csv(final_trait_data,"~/projects/msc_thesis/data/species_traits_per_survey.csv", row.names = FALSE)
write.csv(final_trait_data,"/media/mari/Crucial X8/species_traits_per_survey.csv", row.names = FALSE)

## only species and trait data
write.csv(unique_species,"~/projects/msc_thesis/data/species_traits.csv", row.names = FALSE)
write.csv(unique_species,"/media/mari/Crucial X8/species_traits.csv", row.names = FALSE)
