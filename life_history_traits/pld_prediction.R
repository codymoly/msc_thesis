# IMPUTE BODYSIZE (AND PLD)

# read libs
library(mice)
library(ranger)
library(tidyverse)

# clean memory
rm(list=ls())

# set working directory
setwd("~/Documents/MSc_thesis")

# import data
trait_dat_raw = read_delim("/media/mari/Crucial X8/species_traits.csv", delim = ",")

trait_dat_raw = trait_dat_raw %>% 
  dplyr::filter(trait_dat_raw$class == "Actinopterygii")

# data exploration
# subset data, here, we drop all sharkilies
trait_subset = trait_dat_raw %>%  # drop elasmobranchs
  select (class, family, genus, species, valid_genus, valid_species, bodySize, PLD, larvalDuration, rangeSize)

# we don't have bodysize for these species:
missing_bodysize = trait_subset %>% 
  filter(is.na(bodySize)) 
## in total, data for 87 species is missing

species_notfound = missing_bodysize %>% 
  dplyr::filter(substr(species,1,2) != "sp")
## 23 species in 13 families (run: unique(species_notfound$family)) with no documented bodysize
sp_number_per_family = species_notfound %>%
  group_by(family) %>%
  summarise(count = n_distinct(genus))
## between 1 and 3 species per family, mostly 1 or 2

no_species_level = missing_bodysize %>% 
  dplyr::filter(substr(species,1,2) == "sp")
## 64 individuals in 31+1 (one NA) families could not be identified on species level
sp_number_per_family = no_species_level %>%
  group_by(family) %>%
  summarise(count = n_distinct(genus))
## between 1 and 8 species per family, mostly 1 or 2

# we have PLD for these species:
PLD_complete = trait_subset %>% 
  filter(!is.na(PLD)) 
unique(PLD_complete$family) # 33 of 89 families have species with PLD data --> 56 families have no PLD data, upsi...

# we have rangesize for these species:
rs_complete = trait_subset %>% 
  filter(!is.na(rangeSize)) 
unique(rs_complete$family) # same as for PLD

summary(trait_subset) # 87 bodysize, 808 PLD values, and 813 rangeSize values are missing
 
# bodysize model
mod_bs = lm(trait_subset$bodySize ~ trait_subset$family)
summary(mod_bs)
## R-squared:  0.5581 and 0.5156,	Adjusted R-squared:  0.5156 --> predict missing bodysize by family

# PLD model
# Model 1
mod1 = lm(trait_subset$PLD ~ trait_subset$family)
summary(mod1)
## R squared = 0.5144 --> > 50% in PLD are explained by family

# Model 2
mod2 = lm(trait_subset$PLD ~ trait_subset$bodySize)
summary(mod2) 

# model 3
mod3 = lm(trait_subset$PLD ~ trait_subset$family + trait_subset$bodySize)
summary(mod3)
## Multiple R-squared: 0.5793, Adjusted R-squared: 0.521

# rangesize model
mod_rs_1 = lm(trait_subset$rangeSize ~ trait_subset$family)
summary(mod_rs_1) ## 0.3302, 0.2419

mod_rs_2 = lm(trait_subset$rangeSize ~ trait_subset$bodySize)
summary(mod_rs_2) ## 0.0929, 0.08948 .... impute no rangeSize

##########
# identify predictors for PLD
# model --> r2
# correlation between fitted and original values
# residuals-fitted values, QQnorm plot --> norm distribution, residuals-leverage 
# plot(na.omit(trait_dat$PLD), mod3$fitted.values)
# plot(log(trait_dat$bodySize), log(trait_dat$PLD))

# bodysize imputation
## impute bodySize for one individual of which only class is known
impute_bodysize = trait_subset %>% 
  mutate(imp_bodySize = ifelse(is.na(family), 
                               mean(bodySize, na.rm = TRUE), 
                               NA))
## impute bodysize for all the remaining species
impute_bodysize = impute_bodysize %>%
  group_by(family) %>%
  mutate(imp_bodySize_2 = ifelse(is.na(bodySize) & is.na(imp_bodySize), mean(bodySize, na.rm = TRUE), bodySize)) %>% 
  ungroup()
## create column with merged values
impute_bodysize = impute_bodysize %>%
  mutate(impute_bodysize_final = ifelse(!is.na(imp_bodySize_2), imp_bodySize_2, imp_bodySize))
## round values
impute_bodysize$impute_bodysize_final = round(impute_bodysize$impute_bodysize_final, digits = 1)
## select columns
impute_bodysize = impute_bodysize %>% 
  select(-c("valid_genus", "valid_species",
            "bodySize", "rangeSize",
            "imp_bodySize", "imp_bodySize_2")
         ) %>% 
  rename(bodySize = impute_bodysize_final)

# goal: impute PLD and bodysize from family
## check methods of mice
methods(mice)
## make sure that bodySize is calculated from family, and PLD from both, so probably successively
# predMatr = list(family = trait_dat$family, bodySize = trait_dat$bodySize)
# blocks = list(family = trait_dat$bodySize, bodySize = trait_dat$PLD)
# calculate imputed values
test_pred = mice(impute_bodysize, 
                 m =1, 
                 method = "rf"
                 )

# write imputed values into df
imputed_data = complete(test_pred)

# add columns to document which values are imputed
imputed_data = imputed_data %>% 
  mutate(ref_pld = rep("Imputed", length(imputed_data$PLD)),
         ref_bodysize = rep("Imputed", length(imputed_data$bodySize)),
         ref_larvalduration = rep("Imputed", length(imputed_data$larvalDuration))
         )

# create new identifier
temp_data = trait_dat_raw %>% 
  mutate(sp_number = row_number())

# transform id into character
temp_data$sp_number = as.character(temp_data$sp_number)

# convert ref column for ld into character
temp_data$ref_larvalduration = as.character(temp_data$ref_larvalduration)

# add id to original df
imputed_data = imputed_data %>% 
  left_join(select(temp_data, genus, species, sp_number), by = c("genus","species"))

# replace NAs
for (i in 1:nrow(temp_data)) {
  if (is.na(temp_data[i, "PLD"]) | is.na(temp_data[i, "bodySize"]) | is.na(temp_data[i, "larvalDuration"])) {
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
    
    if (is.na(temp_data[i, "larvalDuration"]) & nrow(pld_rows) > 0 & !is.na(pld_rows[1, "larvalDuration"])) {
      temp_data[i, "larvalDuration"] = pld_rows[1, "larvalDuration"]
      temp_data[i, "ref_larvalduration"] = pld_rows[1, "ref_larvalduration"]
    }
  }
}

# select relevant columns
final_species_traits = temp_data %>% 
  select(class, family, genus, species, valid_genus, valid_species, 
         bodySize, PLD, larvalDuration, ref_bodysize, ref_pld, ref_larvalduration)

# save results
write.csv(final_species_traits,"~/projects/msc_thesis/data/species_traits_imputed.csv", row.names = FALSE)
write.csv(final_species_traits,"/media/mari/Crucial X8/species_traits_imputed.csv", row.names = FALSE)
