# IMPUTATION OF BODYSIZE (AND PLD)

# load libraries
library(tidyverse)
library(simputation)

# clean memory
rm(list=ls())

# set working directory
setwd("~/Documents/MSc_thesis")

# conditional code
save_my_data = FALSE

# import data
trait_dat_raw = read_delim("/media/mari/Crucial X8/species_traits.csv", delim = ",")
rls_dat_raw = read_delim("/media/mari/Crucial X8/rls_2019_2022_clean.csv", delim = ",")

###### data exploration

## create empty dataframe to track our stuff
qualitiy_check = data.frame(matrix(ncol = 5, nrow = 1))
columns = c("trait", "nrow_complete", "nrow_miss", "families_total", "families_without_NA")
colnames(qualitiy_check) <- columns
qualitiy_check$trait = "bodysize"
## checking missing observations
### bodysize
qualitiy_check[1,2] = nrow(trait_dat_raw[complete.cases(trait_dat_raw$bodySize),]) # number of species with bodysize
qualitiy_check[1,3] = nrow(trait_dat_raw[!complete.cases(trait_dat_raw$bodySize),]) # number of species without bodysize
qualitiy_check[1,4] = n_distinct(trait_dat_raw$family) # number of families in raw data
bodysize_true = trait_dat_raw %>% dplyr::filter(!is.na(trait_dat_raw$bodySize))
qualitiy_check[1,5] = n_distinct(bodysize_true$family) # number of families in data with bodysize data

# how many individuals were not identified on species level?
species_no_id = trait_dat_raw %>% 
  dplyr::filter(is.na(bodySize)) %>% 
  dplyr::filter(substr(species,1,2) == "sp") # 95
# how many not identified species are there per family
sp_number_per_family = species_no_id %>%
  group_by(family) %>%
  summarise(count = n_distinct(genus, species))
## between 1 and 12 species per family, mostly 1 or 2

view(qualitiy_check)

# conlusion: 
## 111 of 1275 species don't have bodysize data, but existing data is present in most of the families (96 of 97)
## 95 of the 111 species were not identified on species level
## hence, bodysize may be imputed by replacing missing values by each family's mean


###### examining relations between variables to identify predictors

# bodysize model
summary(lm(trait_dat_raw$bodySize ~ trait_dat_raw$family))
## R-squared:  0.5844,	Adjusted R-squared:  0.5474 --> predict missing bodysize by family
# prepare rls data
rls_sub = rls_dat_raw %>% 
  select(valid_name, size_class) %>% 
  distinct(valid_name, .keep_all = TRUE) %>% 
  separate(valid_name, c("valid_genus", "valid_species"), extra = "merge", fill = "left")

# merge trait and rls data
int_bs_rls = left_join(trait_dat_raw, rls_sub, by = c("valid_genus", "valid_species"))

# size class explains a lot
summary(lm(int_bs_rls$bodySize ~ int_bs_rls$size_class))

# together with family, baaammm
summary(lm(int_bs_rls$bodySize ~ int_bs_rls$size_class + int_bs_rls$family))

###### data imputation
## we use size class and family as predictors
imputed_data = impute_lm(int_bs_rls, bodySize ~ size_class + family)
# there's one species that is not even identified on family level, we will therefore use size class as predictor
imputed_data_2 = impute_lm(imputed_data, bodySize ~ size_class)

# sanity check
nrow(imputed_data_2[complete.cases(imputed_data_2$bodySize),]) # 1275, no missing bodysize data anymore

# save results
if (save_my_data == TRUE) {
  write.csv(imputed_data_2,"~/projects/msc_thesis/data/species_bodysize_imputed.csv", row.names = FALSE)
  write.csv(imputed_data_2,"/media/mari/Crucial X8/species_bodysize_imputed.csv", row.names = FALSE)
} else {
  print("Data not saved!")
}

