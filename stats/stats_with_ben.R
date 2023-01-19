# STATS WITH BEN

# read libs
library(tidyverse)

# clean memory
rm(list=ls())

# set working directory
#setwd("~/Documents/MSc_thesis")

# conditional executions
save_survey_data = FALSE

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
## chla is missing for some sites which is equal to the complete missing data

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

# save complete site data
if (save_survey_data == TRUE) {
  write.csv(eco_env_copy,"~/projects/msc_thesis/data/survey_cwm_envpred_data.csv", row.names = FALSE)
  write.csv(eco_env_copy,"/media/mari/Crucial X8/survey_cwm_envpred_data.csv", row.names = FALSE)
} else {
  print("Data not saved!")
}

# remove outliers
outliers <- function(x) {
  
  Q1 <- quantile(x, probs=.25, na.rm = TRUE)
  Q3 <- quantile(x, probs=.75, na.rm = TRUE)
  iqr = Q3-Q1
  
  upper_limit = Q3 + (iqr*1.5)
  lower_limit = Q1 - (iqr*1.5)
  
  x > upper_limit | x < lower_limit
}

remove_outliers <- function(df, cols = names(df)) {
  for (col in cols) {
    df <- df[!outliers(df[[col]]),]
  }
  df
}

# remove outliers from our df
ecoenv_outl = remove_outliers(eco_env_copy, c("bodysize_cwm_total", "total_biomass",
                                              "sp_richness", "shannon", "simpson", "inv_simpson",
                                              "sst_raw_mean", "sst_raw_var", "sst_raw_cv", "sst_predicted_var", "sst_unpredicted_var",
                                              "sst_unbounded_seasonality", "sst_bounded_seasonality", "sst_env_col",
                                              "sst_colwell_c", "sst_colwell_m", "sst_colwell_p",
                                              "chla_raw_mean", "chla_raw_var", "chla_raw_cv", "chla_predicted_var", "chla_unpredicted_var",
                                              "chla_unbounded_seasonality", "chla_bounded_seasonality", "chla_env_col",
                                              "chla_colwell_c", "chla_colwell_m", "chla_colwell_p"))

ecoenv_outl = ecoenv_outl %>% 
  select(-c("new_survey_id", "survey_date")) %>% 
  dplyr::filter(complete.cases(.))

#
names(ecoenv_outl)
plot(ecoenv_outl$bodysize_cwm_total ~ ecoenv_outl$sst_raw_mean)
summary(lm(ecoenv_outl$bodysize_cwm_total ~ ecoenv_outl$sst_raw_mean))
boxplot(eco_env_copy$bodysize_cwm_total)

# mediation analysis
# assumption: environmental variables are related to eachother



hist(eco_env_copy$sst_raw_mean)

eco_env_copy=eco_env_copy[eco_env_copy$sst_raw_mean>0,]

hist(eco_env_copy$sst_raw_mean)
summary(lm(eco_env_copy$bodysize_cwm_total ~ eco_env_copy$sst_raw_mean))
plot(eco_env_copy$bodysize_cwm_total ~ eco_env_copy$sst_raw_mean)

hist(eco_env_copy$sst_env_col)
summary(lm(eco_env_copy$bodysize_cwm_total ~ eco_env_copy$sst_env_col))
plot(eco_env_copy$bodysize_cwm_total ~ eco_env_copy$sst_env_col)

hist(eco_env_copy$sst_unbounded_seasonality)
summary(lm(eco_env_copy$bodysize_cwm_total ~ eco_env_copy$sst_bounded_seasonality))
plot(eco_env_copy$bodysize_cwm_total ~ eco_env_copy$sst_bounded_seasonality)

pairs(eco_env_copy %>% select(contains("sst") & !contains("colwell"),"bodysize_cwm_total","latitude","longitude"))




hist(eco_env_copy$sst_unbounded_seasonality)
lm(eco_env_copy$bodysize_cwm_total ~ eco_env_copy$sst_bounded_seasonality)
plot(eco_env_copy$bodysize_cwm_total ~ eco_env_copy$sst_bounded_seasonality)

eco_env_copy %>% ggplot(aes(x=bodysize_cwm_total, y=sst_unpredicted_var,colour=sst_raw_mean))+
  geom_point()


eco_env_copy %>% ggplot(aes(x=longitude, y=latitude,colour=sst_raw_mean))+
  geom_point()
eco_env_copy %>% ggplot(aes(x=longitude, y=latitude,colour=sst_raw_var))+
  geom_point()
eco_env_copy %>% ggplot(aes(x=longitude, y=latitude,colour=sst_bounded_seasonality))+
  geom_point()
eco_env_copy %>% ggplot(aes(x=longitude, y=latitude,colour=sst_env_col))+
  geom_point()


mod1 = lm(eco_env_copy$bodysize_cwm_total ~ eco_env_copy$sst_unpredicted_var + eco_env_copy$sst_raw_mean)
summary(mod1)

mod1 = lm(eco_env_copy$bodysize_cwm_total ~ eco_env_copy$sst_env_col)
summary(mod1)


#mit seasonality
mod1 = lm(bodysize_cwm_total ~  sst_raw_mean*sst_bounded_seasonality+
            sst_raw_mean*sst_env_col,
          data=eco_env_copy)

hist(mod1$residuals)
library(interactions)
interact_plot(model = mod1, pred = sst_bounded_seasonality, modx = sst_raw_mean,interval = T)
interact_plot(model = mod1, pred = sst_env_col, modx = sst_raw_mean,interval = T)

summary(mod1)
plot(mod1)

#standardized vals
mod1 = lm(scale(bodysize_cwm_total) ~  scale(sst_raw_mean)*scale(sst_bounded_seasonality)+
            scale(sst_raw_mean)*scale(sst_env_col),
          data=eco_env_copy)
summary(mod1)
summary(aov(mod1))

#seasonality and environmental color do a bit of the same thing to bodysize, but not equally
#
mod1 = lm(bodysize_cwm_total ~  sst_raw_mean,
          data=eco_env_copy)


#not a lot of values for SST
eco_env_copy %>% ggplot(aes(x=longitude, y=latitude,colour=chla_raw_mean))+
  geom_point()
eco_env_copy %>% ggplot(aes(x=longitude, y=latitude,colour=chla_raw_var))+
  geom_point()
eco_env_copy %>% ggplot(aes(x=longitude, y=latitude,colour=chla_bounded_seasonality))+
  geom_point()
eco_env_copy %>% ggplot(aes(x=longitude, y=latitude,colour=chla_env_col))+
  geom_point()





