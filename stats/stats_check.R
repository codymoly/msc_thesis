# FIRST EXAMINATION OF RELATION BETWEEN TRAITS AND ENVIRONMENTAL STUFF

# read libs
library(tidyverse)
library(corrplot)

# clean memory
rm(list=ls())

# set working directory
setwd("~/Documents/MSc_thesis")

# import data
eco_data = read_delim("/media/mari/Crucial X8/cwm_data.csv", delim = ",")
sst_data = read_delim("/media/mari/Crucial X8/env_stats_sst.csv", delim = ",")

# merge data for analysis
eco_sst = full_join(eco_data, sst_data, by = c("latitude", "longitude", "survey_date"))
## baaaaaam perfect match
# arrange by
eco_sst = eco_sst %>% 
  arrange(survey_date, latitude, longitude)

# create unique survey id
## take row number as numbering
eco_sst = eco_sst %>% mutate(new_survey_id = row_number())
## transform number into character
eco_sst$new_survey_id = as.character(eco_sst$new_survey_id)
## add s for survey to each element
eco_sst$new_survey_id = paste("S", eco_sst$new_survey_id, sep="")

# quick correlation check
## with a formula
pairs(~ bodysize_cwm_biomass + PLD_cwm_biomass + sst_bounded_seasonality + sst_env_col, data = eco_sst)
## remove survey data
var_data = eco_sst %>% 
  select(-c("latitude", "longitude", "survey_date", "new_survey_id",
            "bodysize_cwm_biomass", "PLD_cwm_biomass",
            "inv_simpson", "sst_raw_mean", "sst_raw_var", "sst_raw_cv",
            "sst_predicted_var", "sst_unpredicted_var", "sst_unbounded_seasonality",
            "sst_colwell_c", "sst_colwell_m", "sst_colwell_p"))

# Plot correlation matrix
pairs(var_data)
# corrplot(var_data, method = "number")

Q1 <- quantile(eco_sst$sst_bounded_seasonality, .25)
Q3 <- quantile(eco_sst$sst_bounded_seasonality, .75)
IQR <- IQR(eco_sst$sst_bounded_seasonality)

#only keep rows in dataframe that have values within 1.5*IQR of Q1 and Q3
no_outliers <- subset(eco_sst, eco_sst$sst_bounded_seasonality> (Q1 - 1.5*IQR) & 
                        eco_sst$sst_bounded_seasonality< (Q3 + 1.5*IQR))

var_data_2 = no_outliers %>% 
  select(-c("latitude", "longitude", "survey_date", "new_survey_id",
            "bodysize_cwm_biomass", "PLD_cwm_biomass",
            "inv_simpson", "sst_raw_mean", "sst_raw_var", "sst_raw_cv",
            "sst_predicted_var", "sst_unpredicted_var", "sst_unbounded_seasonality",
            "sst_colwell_c", "sst_colwell_m", "sst_colwell_p"))

# Plot correlation matrix
pairs(var_data_2)
