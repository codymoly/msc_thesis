# ENVIRONMENTAL PREDICTABILITY
## envPred package by dbarneche (https://github.com/dbarneche)

# load libraries
#library(devtools)
#install_github("dbarneche/envPred")
library(envPred)

# create dummy data to test the package
set.seed(400)
dummy_sst = runif(n=365*10+3, min=3, max=20)
date = seq(as.Date("2013-1-1"), as.Date("2023-1-1"), by = "days")

# calculate stats delivered by the envpred package
env_stats(time_series = dummy_sst,
          date = date,
          delta = 1,
          noise_method = 'spectrum'
          )
