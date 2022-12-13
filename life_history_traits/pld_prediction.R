# PREDICT PLD

# read libs
#library(rfishbase)
library(tidyverse)

# clean memory
rm(list=ls())

# set working directory
setwd("~/Documents/MSc_thesis")

# import data
## from hard drive
pld_raw = read_delim("/media/mari/Crucial X8/pld_luiz.csv", delim = ",")

# Model 1

mod1 = lm(pld_raw$`Mean PLD (days)` ~ pld_raw$Family)
summary(mod1)
# R squared = 0.5104 --> around 50% in PLD are explained by family

# Model 2
mod2 = lm(pld_raw$`Mean PLD (days)` ~ pld_raw$`Body size (cm)`)
summary(mod2)

# midel 3
mod3 = lm(pld_raw$`Mean PLD (days)` ~ pld_raw$Genus * pld_raw$`Body size (cm)`)
summary(mod3)
