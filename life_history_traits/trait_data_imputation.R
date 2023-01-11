# IMPUTATION OF BODYSIZE (AND PLD)

# load libraries
library(mice)
library(ranger)
library(tidyverse)

# clean memory
rm(list=ls())

# set working directory
setwd("~/Documents/MSc_thesis")

# import data
trait_dat_raw = read_delim("/media/mari/Crucial X8/species_traits.csv", delim = ",")

# drop all elasmobranchs
without_elasmo = trait_dat_raw %>% 
  dplyr::filter(trait_dat_raw$class == "Actinopterygii")
## after dropping elasmobranchs, 1080 of 1120 species remain


###### data exploration

## create empty dataframe to track our stuff
qualitiy_check = data.frame(matrix(ncol = 5, nrow = 2))
columns = c("trait", "nrow_complete", "nrow_miss", "families_total", "families_without_NA")
colnames(qualitiy_check) <- columns
qualitiy_check$trait = c("bodysize", "pld")
## checking missing observations
### bodysize
qualitiy_check[1,2] = nrow(without_elasmo[complete.cases(without_elasmo$bodySize),]) # number of species with bodysize
qualitiy_check[1,3] = nrow(without_elasmo[!complete.cases(without_elasmo$bodySize),]) # number of species without bodysize
qualitiy_check[1,4] = n_distinct(without_elasmo$family) # number of families in raw data
bodysize_true = without_elasmo %>% dplyr::filter(!is.na(without_elasmo$bodySize))
qualitiy_check[1,5] = n_distinct(bodysize_true$family) # number of families in data with bodysize data
### pld
qualitiy_check[2,2] = nrow(without_elasmo[complete.cases(without_elasmo$PLD),]) # number of species with pld
qualitiy_check[2,3] = nrow(without_elasmo[!complete.cases(without_elasmo$PLD),]) # number of species without pld
qualitiy_check[2,4] = n_distinct(without_elasmo$family) # number of families in raw data
pld_true = without_elasmo %>% dplyr::filter(!is.na(without_elasmo$PLD))
qualitiy_check[2,5] = n_distinct(pld_true$family) # number of families in data with pld data

# how many individuals were not identified on species level?
species_no_id = without_elasmo %>% 
  dplyr::filter(is.na(bodySize)) %>% 
  dplyr::filter(substr(species,1,2) != "sp")
## 23 species in 13 families (run: unique(species_notfound$family)) with no documented bodysize
# how many not identified species are there per family
sp_number_per_family = species_no_id %>%
  group_by(family) %>%
  summarise(count = n_distinct(genus, species))
## between 1 and 4 species per family, mostly 1 or 2

view(qualitiy_check)

# conlusion: 
## 87 of 1080 species don't have bodysize data, but existing data is present in most of the families (88 of 89)
## 808 of 1080 species don't have pld data, existing data is present in few families (33 of 89)
## hence, bodysize may be imputed by replacing missing values by each family's mean
## the imputation of pld


###### examining relations between variables to identify predictors

# bodysize model
mod_bs = lm(without_elasmo$bodySize ~ without_elasmo$family)
summary(mod_bs)
## R-squared:  0.5581 and 0.5156,	Adjusted R-squared:  0.5156 --> predict missing bodysize by family

# PLD model
# Model 1
mod1 = lm(without_elasmo$PLD ~ without_elasmo$family)
summary(mod1)
## R squared = 0.5144 --> > 50% in PLD are explained by family

# Model 2
mod2 = lm(without_elasmo$PLD ~ without_elasmo$bodySize)
summary(mod2) 

# model 3
mod3 = lm(without_elasmo$PLD ~ without_elasmo$family + without_elasmo$bodySize)
summary(mod3)
## Multiple R-squared: 0.5793, Adjusted R-squared: 0.521

# rangesize model
mod_rs_1 = lm(without_elasmo$rangeSize ~ without_elasmo$family)
summary(mod_rs_1) ## 0.3302, 0.2419

mod_rs_2 = lm(without_elasmo$rangeSize ~ without_elasmo$bodySize)
summary(mod_rs_2) ## 0.0929, 0.08948 .... impute no rangeSize


###### data imputation

# subset data
trait_subset = without_elasmo %>%
  select (class, family, genus, species, bodySize, PLD)
## we drop rangesize, because we don't have good predictors for it

# bodysize
## impute bodysize for all the remaining species
imputed_bodysize = trait_subset %>%
  group_by(family) %>%
  mutate(imp_bodySize = ifelse(is.na(bodySize), mean(bodySize, na.rm = TRUE), bodySize)) %>% 
  ungroup()
## impute bodySize for one individual of which only class is known
imputed_bodysize = imputed_bodysize %>% 
  mutate(imp_bodySize_2 = ifelse(is.na(family) & is.na(imp_bodySize), 
                               mean(bodySize, na.rm = TRUE), 
                               NA))
## create column with merged values
imputed_bodysize = imputed_bodysize %>%
  mutate(impute_bodysize_final = ifelse(!is.na(imp_bodySize), imp_bodySize, imp_bodySize_2))
## round values
imputed_bodysize$impute_bodysize_final = round(imputed_bodysize$impute_bodysize_final, digits = 1)
## select columns
imputed_bodysize = imputed_bodysize %>%
  select(-c("bodySize", "imp_bodySize", "imp_bodySize_2")) %>%
  rename(bodySize = impute_bodysize_final) %>%
  mutate(ref_bodysize = rep("Imputed", length(imputed_bodysize$species)))


# PLD
# identify predictors for PLD
# model --> r2
# correlation between fitted and original values
# residuals-fitted values, QQnorm plot --> norm distribution, residuals-leverage 
# plot(na.omit(trait_dat$PLD), mod3$fitted.values)
# plot(log(trait_dat$bodySize), log(trait_dat$PLD))

## check methods of mice
methods(mice)
## calculate imputed values
test_pred = mice(imputed_bodysize, 
                 m =1, 
                 method = "rf"
)

# write imputed values into df
imputed_data = complete(test_pred)

# add columns to document which values are imputed
imputed_data = imputed_data %>% 
  mutate(ref_pld = rep("Imputed", length(imputed_data$PLD)))

# create new identifier
temp_data = without_elasmo %>% 
  mutate(sp_number = row_number())

# transform id into character
temp_data$sp_number = as.character(temp_data$sp_number)

# add id to original df
imputed_data = imputed_data %>% 
  left_join(select(temp_data, genus, species, sp_number), by = c("genus","species"))

# replace NAs
for (i in 1:nrow(temp_data)) {
  if (is.na(temp_data[i, "PLD"]) | is.na(temp_data[i, "bodySize"])) {
    curr_number = temp_data[[i, "sp_number"]]
    pld_rows = subset(imputed_data,
                      imputed_data[, "sp_number"] == curr_number)
    
    if (is.na(temp_data[i, "PLD"]) & nrow(pld_rows) > 0 & !is.na(pld_rows[1, "PLD"])) {
      temp_data[i, "PLD"] = pld_rows[1, "PLD"]
      temp_data[i, "ref_pld"] = pld_rows[1, "ref_pld"]
    }
    
    if (is.na(temp_data[i, "bodySize"]) & nrow(pld_rows) > 0 & !is.na(pld_rows[1, "bodySize"])) {
      temp_data[i, "bodySize"] = pld_rows[1, "bodySize"]
      temp_data[i, "ref_bodysize"] = pld_rows[1, "ref_bodysize"]
    }
  }
}

# sanity check
nrow(temp_data[complete.cases(temp_data$bodySize),]) # 1080, no missing bodysize data anymore
nrow(temp_data[complete.cases(temp_data$PLD),]) # 1080, no missing PLD data anymore
# alt: summary(temp_data)
unique(temp_data$ref_bodysize)
unique(temp_data$ref_pld)
## all references seem to fit

# select relevant columns
final_species_traits = temp_data %>% 
  select(class, family, genus, species, valid_genus, valid_species, 
         bodySize, PLD, ref_bodysize, ref_pld)

# save results
write.csv(final_species_traits,"~/projects/msc_thesis/data/species_traits_imputed.csv", row.names = FALSE)
write.csv(final_species_traits,"/media/mari/Crucial X8/species_traits_imputed.csv", row.names = FALSE)
