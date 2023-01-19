# FIRST EXAMINATION OF RELATION BETWEEN TRAITS AND ENVIRONMENTAL STUFF

# read libs
library(tidyverse)

# clean memory
rm(list=ls())

# set working directory
setwd("~/Documents/MSc_thesis")

# conditional executions
save_completed_dataset = FALSE

# import data
eco_data = read_delim("/media/mari/Crucial X8/cwm_data.csv", delim = ",")
sst_data = read_delim("/media/mari/Crucial X8/env_stats_sst.csv", delim = ",")
chla_data = read_delim("/media/mari/Crucial X8/env_stats_chla.csv", delim = ",")

###### data preparation

# merge data for analysis
eco_env = full_join(eco_data, sst_data, by = c("latitude", "longitude", "survey_date"))
eco_env = full_join(eco_env, chla_data, by = c("latitude", "longitude", "survey_date"))
## baaaaaam perfect match
# double-check if we have incomplete cases...
nrow(eco_env[complete.cases(eco_env),]) # 201
nrow(eco_env[complete.cases(eco_env$chla_raw_mean),]) # 201
## chla is missing for some sites 

# create unique survey id
## arrange by site
eco_env_copy = eco_env %>% 
  arrange(survey_date, latitude, longitude)
## take row number as numbering
eco_env_copy = eco_env_copy %>%
  dplyr::mutate(new_survey_id = row_number())
## transform number into character
eco_env_copy$new_survey_id = as.character(eco_env_copy$new_survey_id)
## add s for survey to each element
eco_env_copy$new_survey_id = paste("S", eco_env_copy$new_survey_id, sep="")
## move column with survey name to start
eco_env_copy = eco_env_copy %>%
  select(new_survey_id, everything())

# save dataset
if (save_completed_dataset == TRUE) {
  write.csv(eco_env_copy,"~/projects/msc_thesis/data/survey_cwm_envpred_data.csv", row.names = FALSE)
  write.csv(eco_env_copy,"/media/mari/Crucial X8/survey_cwm_envpred_data.csv", row.names = FALSE)
} else {
  print("Data not saved!")
}


##### data exploration

# get variable names
names(eco_env_copy)
summary(eco_env_copy)
## some sites have sst values <= 0, this might be due to the fill values in the cds dataset
nas_replaced = eco_env_copy # save copy
nm1 = grep('sst', names(nas_replaced)) # choose columns
nas_replaced[nm1] = lapply(nas_replaced[nm1], function(x) 
  replace(x, nas_replaced$sst_raw_mean <= 0, NA)) # replace values <= 0 with NAs in sst data
## sanity check
summary(nas_replaced)

# remove outliers from "sst_bounded_seasonality"
## calculate quantiles
Q1 <- quantile(nas_replaced$chla_env_col, .25, na.rm = TRUE)
Q3 <- quantile(nas_replaced$chla_env_col, .75, na.rm = TRUE)
IQR <- IQR(nas_replaced$chla_env_col, na.rm = TRUE)
## replace outliers in chla_env_col
nas_replaced$chla_env_col[nas_replaced$chla_env_col > (Q3 + 2*IQR)] = NA


##### visual inspection

# sst
pairs(~ sst_raw_mean + 
        sst_raw_var + 
        sst_raw_cv + 
        sst_predicted_var + 
        sst_unpredicted_var + 
        sst_unbounded_seasonality +
        sst_bounded_seasonality +
        sst_env_col, 
      data = nas_replaced)

# chla
pairs(~ chla_raw_mean + 
        chla_raw_var + 
        chla_raw_cv + 
        chla_predicted_var + 
        chla_unpredicted_var + 
        chla_unbounded_seasonality +
        chla_bounded_seasonality +
        chla_env_col, 
      data = nas_replaced)

# all
pairs(~ sst_raw_mean + 
        sst_unbounded_seasonality +
        sst_env_col +
        chla_raw_mean + 
        chla_unbounded_seasonality +
        chla_env_col +
        bodysize_cwm_total +
        sp_richness,
      data = nas_replaced)

