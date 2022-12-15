# ENVIRONMENTAL PREDICTABILITY OF SST
## envPred package by dbarneche (https://github.com/dbarneche)
## installation: library(devtools), install_github("dbarneche/envPred")

# load libraries
library(tidyverse)
library(envPred)
library(stringr)

# clean memory
rm(list=ls())

# set working directory
setwd("~/projects/msc_thesis")

# use envPred package with one time series to retrieve variables
## read file
test_csv = read_delim("/media/mari/Crucial X8/sst_csv/-14.10_123.55.csv", delim = ",")
## select column with temperature measurements and the minimum time period
test_csv = test_csv %>%
  select(date, analysed_sst) %>%
  dplyr::filter(date >= "2009-01-01" & date <= "2020-01-01")
## read names/ variables of the statistics into vector
env_stats_cols = names(env_stats(
  time_series = test_csv$analysed_sst,
  dates = test_csv$date,
  delta = 1,
  noise_method = 'spectrum'
))

# read averaged RLS dataset
rls_avg = read_delim("/media/mari/Crucial X8/rls_2021_2022_avg.csv", delim = ",")

# select distinct combinations of lat, long, AND survey date
rls_unique = rls_avg %>% 
  select(longitude, latitude, survey_date) %>% 
  distinct() %>% 
  mutate(ts_startdate = survey_date - 365.25*10) # create new column with start date for envPred (survey date - 10y)

# write new column with latitude, longitude, and .csv
## write function that pastes all components
CompFilename = function(row){
  return(str_squish(paste(row[2], "_", row[1], ".csv", sep = "")))
}
## apply function and write output into new column
rls_unique$sst_filename = apply(rls_unique, 1, CompFilename)

# create "empty" columns with NAs, one for each env_stats variable
for (statname in env_stats_cols) {
  rls_unique[statname] = NA
}

# iteratre through the time series CSVs to calculate the predictability of sst
for (i in 1:nrow(rls_unique)) {
  #if (rls_unique[i,2] != -9.99) {next}
  temp_filename = paste("/media/mari/Crucial X8/sst_csv/", as.character(rls_unique[i,"sst_filename"]), sep = "")
  # create new column in the rls dataset with string containing the file names of the time series data
  if (!file.exists(temp_filename)) {
    print(paste(temp_filename, "not found", sep = " "))
    next
  } # write "not found" if a file doesn't exist
  
  temp_sst_data = read_delim(temp_filename, delim = ',') # read respective file
  pred_range = temp_sst_data %>% 
    filter(temp_sst_data$date >= rls_unique[[i, "ts_startdate"]] &
           temp_sst_data$date <= rls_unique[[i, "survey_date"]]
           ) # subset each time series based on relative time range
  envpred_statistics = env_stats(time_series = pred_range$analysed_sst,
                                 dates = pred_range$date,
                                 delta = 1,
                                 noise_method = 'spectrum'
                                 ) # calculate and write environmental statistics of envPred into a dataframe
  
  for (statname in env_stats_cols) { # write each statistic into a new column in rls_unique
    rls_unique[i, statname] = envpred_statistics[, statname]
  }
}

# add postfix to statnames
colnames(rls_unique)[6:24] = paste("sst", colnames(rls_unique)[6:24], sep = "_")

# sanity check
## count rows that have missing values
nrow(na.omit(rls_unique))
## write missing rows into dataframe
NAS = rls_unique[!complete.cases(rls_unique), ]
NAS_coord = NAS %>% select(longitude, latitude)

# write new files
#write.csv(rls_unique,"~/projects/msc_thesis/data/rls_2021_2022_avg.csv", row.names = FALSE)
#write.csv(rls_unique,"/media/mari/Crucial X8/rls_2021_2022_avg.csv", row.names = FALSE)