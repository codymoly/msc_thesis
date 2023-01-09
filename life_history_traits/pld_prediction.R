# PREDICT PLD

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

##### now: do we want to include sharkies as well?

# subset data, here, we drop all sharkilies
trait_subset = trait_dat_raw %>% 
  select (class, family, genus, species, valid_genus, valid_species, bodySize, PLD, rangeSize)# %>%
  #dplyr::filter(trait_dat_raw$class == "Actinopterygii") %>% 
  #filter(!is.na(bodySize)) 

# we don't have bodysize and PLD data for these species:
missing_bodysize = trait_subset %>% 
  filter(is.na(bodySize)) 

# data exploration
summary(trait_dat_raw) # 90 bodysize and 848 PLD values are missing
 
# Model 1
mod1 = lm(trait_dat$PLD ~ trait_dat$family)
summary(mod1)
# R squared = 0.5104 --> around 50% in PLD are explained by family

# Model 2
mod2 = lm(trait_dat$PLD ~ trait_dat$bodySize)
summary(mod2) 

# model 3
mod3 = lm(trait_dat$PLD ~ trait_dat$family + (trait_dat$bodySize))
summary(mod3) # not sure if that makes sense...

mod4 = lm(trait_dat$bodySize ~ trait_dat$family)
summary(mod4)

mod3$fitted.values

predict(mod3)

##########
# identify predictors for PLD
# model --> r2
# correlation between fitted and original values
# residuals-fitted values, QQnorm plot --> norm distribution, residuals-leverage 
plot(na.omit(trait_dat$PLD), mod3$fitted.values)
plot(log(trait_dat$bodySize), log(trait_dat$PLD))

# goal: impute PLD and bodysize from family
# is the data missing at random or
## check methods of mice
methods(mice)
## make sure that bodySize is calculated from family, and PLD from both, so probably successively
# predMatr = list(family = trait_dat$family, bodySize = trait_dat$bodySize)
# blocks = list(family = trait_dat$bodySize, bodySize = trait_dat$PLD)
# calculate imputed values
test_pred = mice(temp_data, 
                 m =1, 
                 method = c("","rf","","","rf","rf","rf","rf","rf"),
                 maxit = 20)

# write imputed values into df
imputed_data = complete(test_pred)

# add columns to document which values are imputed
imputed_data = imputed_data %>% 
  mutate(ref_pld = rep("Imputed", length(imputed_data$PLD)),
         ref_bodysize = rep("Imputed", length(imputed_data$bodySize)),
         ref_rangesize = rep("Imputed", length(imputed_data$rangeSize))
         )

# create new identifier
temp_data = trait_dat_raw %>% 
  mutate(sp_number = row_number(),
         ref_rangesize = rep("Luiz2013", length(temp_data$rangeSize))
         )

# transform id into character
temp_data$sp_number = as.character(temp_data$sp_number)

# add id to original df
imputed_data = imputed_data %>% 
  left_join(select(temp_data, genus, species, sp_number), by = c("genus","species"))

# replace NAs
for (i in 1:nrow(temp_data)) {
  if (is.na(temp_data[i, "PLD"]) | is.na(temp_data[i, "bodySize"]) | is.na(temp_data[i, "rangeSize"])) {
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
    
    if (is.na(temp_data[i, "rangeSize"]) & nrow(pld_rows) > 0 & !is.na(pld_rows[1, "rangeSize"])) {
      temp_data[i, "rangeSize"] = pld_rows[1, "rangeSize"]
      temp_data[i, "ref_rangesize"] = pld_rows[1, "ref_rangesize"]
    }
  }
}

# select relevant columns
final_species_traits = temp_data %>% 
  select(class, family, genus, species, valid_genus, valid_species, 
         bodySize, PLD, rangeSize, ref_bodysize, ref_pld, ref_rangesize)

# save results
write.csv(final_species_traits,"~/projects/msc_thesis/data/species_traits_imputed.csv", row.names = FALSE)
write.csv(final_species_traits,"/media/mari/Crucial X8/species_traits_imputed.csv", row.names = FALSE)


