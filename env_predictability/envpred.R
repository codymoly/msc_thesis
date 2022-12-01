# ENVIRONMENTAL PREDICTABILITY
## envPred package by dbarneche (https://github.com/dbarneche)
## installation: library(devtools), install_github("dbarneche/envPred")

# load libraries
library(tidyverse)
library(envPred)

# clean memory
rm(list=ls())

# set working directory
setwd("~/projects/msc_thesis")

# # test envPred package with dummy data
# ## create dummy data to test the package
# set.seed(400)
# dummy_sst = runif(n=365*10+3, min=3, max=20)
# date = seq(as.Date("2013-1-1"), as.Date("2023-1-1"), by = "days")
# 
# ## calculate stats delivered by the envpred package
# env_stats(time_series = dummy_sst,
#           date = date,
#           delta = 1,
#           noise_method = 'spectrum'
#           )

# I have a folder with one time series per site
# read the file with converting filenames into variable names
# apply the following function relative to the survey time in rls_site_depth_ID.csv
# calculate envPred statistics -12.24_122.98.csv

# list files in vector
list_csv_files <- list.files(path = "/media/mari/Crucial X8/sst_csv/", pattern="*.csv")

# remove .csv from filenames
file_names = substring(list_csv_files, 1, nchar(list_csv_files)-4)

test_csv = read.table("/media/mari/Crucial X8/sst_csv/-11.98_123.38.csv", quote="\"", fill = TRUE)

rls_2021_2022 = read_delim("/media/mari/Crucial X8/RLS_2021_2022.csv", skip = 71, delim = ",")


# 
# average over depth grouped by survey date
# unique coordinate AND survey date
# stringr, write coords from string in columns

# distinct lat, long, survey date
rls_summary = rls_2021_2022 %>% 
  select(longitude, latitude, survey_date) %>% 
  distinct()

rls_summary$sst_filename = paste(rls_summary$latitude, "_", rls_summary$longitude, ".csv", sep = "")
list_csv_files

rls_summary$sst_mean = NA

for (i in 1:nrow(rls_summary)){
  temp_filename = paste("/media/mari/Crucial X8/sst_csv/", as.character(rls_summary[i,4]), sep = "")
  if(file.exists(temp_filename)){
    print(i)
    
    temp_sst_data = read_delim(temp_filename, delim = ',')
    print(mean(temp_sst_data$analysed_sst))
    
    # rls_summary$sst_mean[i] = mean(temp_sst_data$analysed_sst)
  }
}







# read data from github
rls_unique = read_delim("/media/mari/Crucial X8/rls_site_depth_ID.csv", delim = ",")
rls_coords = read_delim("/media/mari/Crucial X8/survey_coordinates_2021_2022.csv", skip = 71, delim = ",")

# write filenames into vector
# raw_files <- tibble(filename = list.files('/media/mari/Crucial X8/sst_csv/'))

# rename column names
colnames(test_csv) <- c('dates','value','variable')

# write each variable into one column
test_csv_wide = test_csv %>% 
  distinct() %>% 
  pivot_wider(names_from = variable,
              values_from = value
              )

# change format of dates column
test_csv_wide$dates = as.Date(test_csv_wide$dates, "%Y/%m/%d")

# select sst and subset a period of 10 years between 2010 and 2020
test_ts = test_csv_wide %>%
  select(dates, analysed_sst) %>% 
  dplyr::filter(dates > "2009-12-31" & dates < "2021-01-02")

# envPred
env_stats = env_stats(time_series = test_ts$analysed_sst,
                      dates = test_ts$dates,
                      delta = 1,
                      noise_method = 'spectrum'
                      )
