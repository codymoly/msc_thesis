# PREDICT PLD

# read libs
library(dplyr)

# clean memory
rm(list=ls())

# set working directory
setwd("~/Documents/MSc_thesis")

# import data
trait_dat_raw = read_delim("/media/mari/Crucial X8/species_traits.csv", delim = ",")

# we don't have data for these species:
missing_bodysize = trait_dat_raw %>% 
  filter(is.na(bodySize)) # 109 are missing

##### now: do we want to include sharkies as well?

# subset data, here, we drop all sharkilies
trait_dat = trait_dat_raw %>% 
  select (class, family, genus, species, bodySize, PLD) %>%
  dplyr::filter(trait_dat_raw$class == "Actinopterygii") %>% 
  filter(!is.na(bodySize)) 
 
# Model 1
mod1 = lm(trait_dat$PLD ~ trait_dat$family)
summary(mod1)
# R squared = 0.5104 --> around 50% in PLD are explained by family

# Model 2
mod2 = lm(trait_dat$PLD ~ trait_dat$bodySize)
summary(mod2)

# model 3
mod3 = lm(trait_dat$PLD ~ trait_dat$genus * trait_dat$bodySize)
summary(mod3)

# Model 4
mod4 = lm(trait_dat$PLD ~ trait_dat$genus)
summary(mod4)

#
family = trait_dat %>%
  filter(is.na(PLD)) %>% 
  select(family)

# prediction based on genus
testo = predict(mod4)
