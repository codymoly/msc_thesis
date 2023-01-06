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

# we don't have data for these species:
missing_bodysize = trait_dat_raw %>% 
  filter(is.na(bodySize)) # 109 are missing

##### now: do we want to include sharkies as well?

# subset data, here, we drop all sharkilies
trait_dat = trait_dat_raw %>% 
  select (class, family, genus, species, bodySize, PLD)# %>%
  #dplyr::filter(trait_dat_raw$class == "Actinopterygii") %>% 
  #filter(!is.na(bodySize)) 
 
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
## make sure that bodySize is calculated from family, and PLD from both, so probably successively
predMatr = list(family = trait_dat$family, bodySize = trait_dat$bodySize)
blocks = list(family = trait_dat$bodySize, bodySize = trait_dat$PLD)
test_pred = mice(trait_dat, 
                 m =1, method = "rf", 
                 predictorMatrix = matrix(predMatr), 
                 blocks = blocks)
complete(test_pred)
