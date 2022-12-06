# ENVIRONMENTAL PREDICTABILITY OF SST
## envPred package by dbarneche (https://github.com/dbarneche)
## installation: library(devtools), install_github("dbarneche/envPred")

# load libraries
library(tidyverse)
library(envPred)

# clean memory
rm(list=ls())

# set working directory
setwd("~/projects/msc_thesis")

# test envPred package with one time series
## read file
test_csv = read_delim("/media/mari/Crucial X8/sst_csv/-14.10_123.55.csv", delim = ",")
## select column with temp measurements and a time span of 11 years
test_csv = test_csv %>%
  select(date, analysed_sst) %>%
  dplyr::filter(date >= "2009-01-01" & date <= "2020-01-01")
## read names of the statistics into variable (we need this later...)
env_stats_cols = names(env_stats(
  time_series = test_csv$analysed_sst,
  dates = test_csv$date,
  delta = 1,
  noise_method = 'spectrum'
))

# read RLS dataset
rls_avg = read_delim("/media/mari/Crucial X8/rls_2021_2022_avg.csv", delim = ",")

# select distinct combinations of lat, long, AND survey date
rls_unique = rls_avg %>% 
  select(longitude, latitude, survey_date) %>% 
  distinct() %>% 
  mutate(ts_startdate = survey_date - 365.25*10)

# write new column with latitude, longitude, and .csv
rls_unique$sst_filename = paste(rls_unique$latitude, "_", rls_unique$longitude, ".csv", sep = "")

# create "empty" columns with NAs, one for each env_stats variable
for (statname in env_stats_cols) {
  rls_unique[statname] = NA
}

#
for (i in 1:nrow(rls_unique)) {
  temp_filename = paste("/media/mari/Crucial X8/sst_csv/", as.character(rls_unique[i,"sst_filename"]), sep = "")
  
  if (!file.exists(temp_filename)) {
    print(paste(temp_filename, "not found", sep = " "))
    next
  }
  
  temp_sst_data = read_delim(temp_filename, delim = ',') # read respective file
  pred_range = temp_sst_data %>% 
    filter(temp_sst_data$date >= rls_unique[[i, "ts_startdate"]] &
           temp_sst_data$date <= rls_unique[[i, "survey_date"]]
           ) # subset time series data based on relative time range
  envpred_statistics = env_stats(time_series = pred_range$analysed_sst,
                                 dates = pred_range$date,
                                 delta = 1,
                                 noise_method = 'spectrum'
                                 ) # write environmental statistics into df
  
  for (statname in env_stats_cols) { # write each statistic into a new column in rls_unique
    rls_unique[i, statname] = envpred_statistics[, statname]
  }
}
