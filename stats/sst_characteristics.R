# DESCRIPTIVE RESULTS OF SST

# read libraries
library(tidyverse)

# clean memory
rm(list=ls())

# set working directory
setwd("~/Documents/MSc_thesis/Figures")

# import datasets
final_dataset = readr::read_delim("/media/mari/Crucial X8/sst_trait_data.csv", delim = ",")


###### preparation

# kelvin into deg celsius
final_dataset["sst_raw_mean"] = final_dataset["sst_raw_mean"] - 273.15


###### descriptive results
final_dataset %>% 
  select(sst_raw_mean, sst_raw_var, sst_env_col, sst_bounded_seasonality) %>% 
  summary()
