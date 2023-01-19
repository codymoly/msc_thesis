# ENVIRONMENTAL PREDICTABILITY OF CHLOROPHYLL ALPHA
## envPred package by dbarneche (https://github.com/dbarneche)
## installation: library(devtools), install_github("dbarneche/envPred")

# load libraries
library(tidyverse)
library(envPred)
library(stringr)
library(lubridate)

# clean memory
rm(list=ls())

# set working directory
setwd("~/projects/msc_thesis")

# save data in the end
save_chla_predictability = FALSE

# use envPred package with one time series to retrieve variables
## read file
test_csv = read_delim("/media/mari/Crucial X8/chla_csv/-9.88_129.29.csv", delim = ",")
## select column with temperature measurements and the minimum time period
test_csv = test_csv %>%
  select(date, chloro_a) %>%
  dplyr::filter(date >= "2009-01-01" & date <= "2020-01-01")
## read names/ variables of the statistics into vector
env_stats_cols = names(env_stats(
  time_series = test_csv$chloro_a,
  dates = test_csv$date,
  delta = 1,
  is_uneven = FALSE,
  interpolate = FALSE,
  show_warns = TRUE, 
  noise_method = 'spectrum'
))

# read averaged RLS dataset
rls_avg = read_delim("/media/mari/Crucial X8/rls_2021_2022_avg.csv", delim = ",")

# select distinct combinations of lat, long, AND survey date
rls_unique = rls_avg %>% 
  select(longitude, latitude, survey_date) %>% 
  distinct() %>% 
  mutate(ts_startdate = survey_date - years(10)) # create new column with start date for envPred (survey date - 10y)

# write new column with latitude, longitude, and .csv
## write function that pastes all components
CompFilename = function(row){
  return(str_squish(paste(row[2], "_", row[1], ".csv", sep = "")))
}
## apply function and write output into new column
rls_unique$chla_filename = apply(rls_unique, 1, CompFilename)

# create "empty" columns with NAs, one for each env_stats variable
for (statname in env_stats_cols) {
  rls_unique[statname] = NA
}

# iterate over the time series CSVs to calculate the predictability of chla
chloro_date_limit = as.Date("2021-07-31")
for (i in 1:nrow(rls_unique)) {
  # skip all rows where survey date > 2021-07-31
  if (rls_unique[i, "survey_date"] > chloro_date_limit)
    next
  
  temp_filename = paste("/media/mari/Crucial X8/chla_csv/", as.character(rls_unique[i,"chla_filename"]), sep = "")
  # create new column in the rls dataset with string containing the file names of the time series data
  if (!file.exists(temp_filename)) {
    print(paste(temp_filename, "not found", sep = " "))
    next
  } # write "not found" if a file doesn't exist
  
  temp_chla_data = read_delim(temp_filename, delim = ',') # read respective file
  pred_range = temp_chla_data %>% 
    filter(temp_chla_data$date >= rls_unique[[i, "ts_startdate"]] &
             temp_chla_data$date <= rls_unique[[i, "survey_date"]]
    ) # subset each time series based on relative time range
  envpred_statistics = env_stats(time_series = pred_range$chloro_a,
                                 dates = pred_range$date,
                                 is_uneven = FALSE,
                                 delta = 1,
                                 noise_method = 'spectrum'
  ) # calculate and write environmental statistics of envPred into a dataframe
  
  for (statname in env_stats_cols) { # write each statistic into a new column in rls_unique
    rls_unique[i, statname] = envpred_statistics[, statname]
  }
}

# complete cases
nrow(rls_unique[complete.cases(rls_unique),])

# add postfix to stat names
names(rls_unique)
colnames(rls_unique)[6:24] = paste("chla", colnames(rls_unique)[6:24], sep = "_")

# remove start date and filename
final_chla = rls_unique %>% 
  select(-c("ts_startdate", "chla_filename", "chla_series_n", "chla_n_na", "chla_prop_na",
            "chla_n_yrs", "chla_n_months", "chla_n_days", "chla_frequency", "chla_nyquist_freq"))

# sanity check
## count number of observations with missing data
nrow(final_chla[!complete.cases(final_chla),]) # or nrow(na.omit(final_chla))

# write new files
if (save_chla_predictability == TRUE) {
  write.csv(final_chla,"~/projects/msc_thesis/data/env_stats_chla.csv", row.names = FALSE)
  write.csv(final_chla,"/media/mari/Crucial X8/env_stats_chla.csv", row.names = FALSE)
} else {
  print("Data not saved!")
}
