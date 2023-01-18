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


summary(sst_data)
summary(chla_data)

###### data preparation

# merge data for analysis
eco_env = full_join(eco_data, sst_data, by = c("latitude", "longitude", "survey_date"))
eco_env = full_join(eco_env, chla_data, by = c("latitude", "longitude", "survey_date"))
## baaaaaam perfect match
# double-check if we have incomplete cases...
nrow(eco_env[complete.cases(eco_env),]) # 201
nrow(eco_env[complete.cases(eco_env$chla_raw_mean),]) # 201
## chla is missing for some sites 

# # check date of sites that have chla values
# years_with_chla = eco_env %>% 
#   filter(!is.na(chla_raw_mean)) %>% 
#   select(survey_date) %>% 
#   arrange(desc(survey_date))

# arrange by
eco_env_copy = eco_env %>% 
  arrange(survey_date, latitude, longitude)

# create unique survey id
## take row number as numbering
eco_env_copy = eco_env_copy %>%
  mutate(new_survey_id = row_number())
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
  print("Protego!")
}

###### data visualisation
boxplot(eco_env$sst_bounded_seasonality)

# get variable names
names(eco_env_copy)

# quick correlation check
pairs(~ chla_env_col + chla_raw_mean + chla_bounded_seasonality + bodysize_cwm_total + PLD_cwm_total + inv_simpson, data = eco_env_copy)

# remove outliers from "sst_bounded_seasonality"
## calculate quantiles
Q1 <- quantile(eco_env_copy$chla_env_col, .25, na.rm = TRUE)
Q3 <- quantile(eco_env_copy$chla_env_col, .75, na.rm = TRUE)
IQR <- IQR(eco_env_copy$chla_env_col, na.rm = TRUE)

## only keep rows in dataframe that have values within 1.5*IQR of Q1 and Q3
no_outliers <- subset(eco_env_copy, eco_env_copy$chla_env_col> (Q1 - 1.5*IQR) & 
                        eco_env_copy$chla_env_col< (Q3 + 1.5*IQR))

# correlation check without outliers from "sst_bounded_seasonality"
pairs(~ chla_env_col + chla_raw_mean + chla_bounded_seasonality + bodysize_cwm_total + inv_simpson, data = no_outliers)

pairs(~ chla_env_col + chla_bounded_seasonality + chla_raw_var + chla_predicted_var + chla_unpredicted_var + bodysize_cwm_total, data = no_outliers)
