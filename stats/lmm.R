# BINNING, OUTLIERS, AND LINEAR MODEL

# read libs
library(tidyverse)
library(grid)
library(gridExtra)
library(ggpubr)
library(broom)
library(corrplot)

# plot Australia map and sites
library(sf)
library(ggplot2)
library(maps)
library(mapdata)

## vif
library(car)

## dredging
library(MuMIn)
## lmer
library(lme4)
## standardise model
library(arm)

# clean memory
rm(list=ls())

# set working directory
setwd("~/Documents/MSc_thesis")

# conditional executions
save_my_data = FALSE

# import data
eco_data = readr::read_delim("/media/mari/Crucial X8/cwm_data.csv", delim = ",")
sst_data = readr::read_delim("/media/mari/Crucial X8/env_stats_sst.csv", delim = ",")
coords_30 = readr::read_delim("/media/mari/Crucial X8/coords_30km.csv", delim = ",")
rls_area = coords_30 = readr::read_delim("/media/mari/Crucial X8/rls_2019_2022_upd.csv", skip = 71, delim = ",")


###### joining data

# select area
rls_area_2 = rls_area %>% 
  dplyr::select(longitude, latitude, area) %>% 
  distinct(longitude, latitude, .keep_all = TRUE)

# merge environmental and biological data 
eco_sst = full_join(eco_data, sst_data, by = c("latitude", "longitude", "survey_date"))

# filter sites between 2020 and 2022 (for sites < 2020, the predictability contained missing months)
eco_sst = eco_sst %>% 
  dplyr::filter(survey_date > "2019-12-31")

# add area
eco_sst = left_join(eco_sst, rls_area_2, by = c("latitude", "longitude"))


###### sample one site per coordinate pair and assign new ID

# choose only one random observation per coordinate pair
set.seed(300)
eco_env = eco_sst %>% 
  dplyr::group_by(latitude, longitude) %>% 
  dplyr::slice_sample(n = 1) %>% 
  dplyr::ungroup()


###### get rid of NAs and outliers

# fill values
eco_env_out = eco_env # save copy
sst_columns = grep('sst', names(eco_env_out)) # choose columns
eco_env_out[sst_columns] = lapply(eco_env_out[sst_columns], function(x) 
  replace(x, eco_env_out$sst_raw_mean <= 0, NA)) # replace values <= 0 with NAs in sst data
summary(eco_env_out$sst_raw_mean) # check number of NA's 

# check distribution
boxplot(eco_env_out$sst_bounded_seasonality)

# remove outliers
## number_total
Q1 <- quantile(eco_env_out$number_total, .25, na.rm = TRUE)
Q3 <- quantile(eco_env_out$number_total, .75, na.rm = TRUE)
IQR <- IQR(eco_env_out$number_total, na.rm = TRUE)
eco_env_out$number_total[eco_env_out$number_total < (Q1 - 2*IQR)] = NA
eco_env_out$number_total[eco_env_out$number_total > (Q3 + 2*IQR)] = NA

## bodysize_cwv_total
Q1 <- quantile(eco_env_out$bodysize_cwv_total, .25, na.rm = TRUE)
Q3 <- quantile(eco_env_out$bodysize_cwv_total, .75, na.rm = TRUE)
IQR <- IQR(eco_env_out$bodysize_cwv_total, na.rm = TRUE)
eco_env_out$bodysize_cwv_total[eco_env_out$bodysize_cwv_total < (Q1 - 2*IQR)] = NA
eco_env_out$bodysize_cwv_total[eco_env_out$bodysize_cwv_total > (Q3 + 2*IQR)] = NA

## total_biomass
Q1 <- quantile(eco_env_out$total_biomass, .25, na.rm = TRUE)
Q3 <- quantile(eco_env_out$total_biomass, .75, na.rm = TRUE)
IQR <- IQR(eco_env_out$total_biomass, na.rm = TRUE)
eco_env_out$total_biomass[eco_env_out$total_biomass < (Q1 - 2*IQR)] = NA
eco_env_out$total_biomass[eco_env_out$total_biomass > (Q3 + 2*IQR)] = NA

## sst_bounded_seasonality
Q1 <- quantile(eco_env_out$sst_bounded_seasonality, .25, na.rm = TRUE)
Q3 <- quantile(eco_env_out$sst_bounded_seasonality, .75, na.rm = TRUE)
IQR <- IQR(eco_env_out$sst_bounded_seasonality, na.rm = TRUE)
eco_env_out$sst_bounded_seasonality[eco_env_out$sst_bounded_seasonality < (Q1 - 2*IQR)] = NA
eco_env_out$sst_bounded_seasonality[eco_env_out$sst_bounded_seasonality > (Q3 + 2*IQR)] = NA

# summarise again
summary(eco_env_out) # check number of NA's 
nrow(eco_env_out[complete.cases(eco_env_out),]) # 498

# overwrite dataframe with complete cases
eco_env_out = eco_env_out[complete.cases(eco_env_out),]

# create unique survey id
## arrange by site and add row number as new ID
eco_env_out = eco_env_out %>% 
  dplyr::arrange(survey_date, latitude, longitude) %>%
  dplyr::mutate(new_survey_id = row_number())

## transform number into character
eco_env_out$new_survey_id = as.character(eco_env_out$new_survey_id)

## add S for survey to each element in new_survey_id
eco_env_out$new_survey_id = paste("S", eco_env_out$new_survey_id, sep="")

## move column with survey name to start of the dataframe
eco_env_out = eco_env_out %>%
  dplyr::select(new_survey_id, everything())


###### get know your data

# how many sites do we have now?
## defining which subset will be the final one: subset_num, subset_deg, subset_km
final_sites = eco_env_out

## count
nrow(final_sites)

# plot map with sites
aussi = st_as_sf(map("worldHires", "Australia", fill=TRUE, xlim=c(110,160), ylim=c(-45,-5), mar=c(0,0,0,0)))
ggplot(data = aussi) + 
  geom_sf() + 
  geom_point(data = as_tibble(final_sites), aes(x = longitude, y = latitude, colour = sst_env_col), size = 3)
### replace data with respective binned dataframe


###### scaling and data normalisation

# if necessary, log-transform data
final_sites_log = final_sites %>%
  mutate(
    number_total = log(number_total),
    bodysize_cwm_total = log(bodysize_cwm_total),
    bodysize_cwv_total = log(bodysize_cwv_total),
    total_biomass = log(total_biomass),
    sst_raw_mean = log(sst_raw_mean),
    sst_raw_var = log(sst_raw_var)
  ) 

# scale our variables
final_sites_scaled = final_sites 
final_sites_scaled[5:22] <- lapply(final_sites_scaled[5:22], function(x) c(scale(x)))

# which transformation will it be?
final_sites = final_sites_log # or final_sites_log


###### exploratory stats

# quick overview how the variables are related
pairs(~ sst_raw_mean +
        sst_raw_var +
        sst_bounded_seasonality +
        sst_env_col +
        bodysize_cwm_total +
        bodysize_cwv_total +
        shannon +
        even_total +
        sp_richness +
        total_biomass,
      data = final_sites)

# correlation between predictors
final_sites %>%
  na.omit() %>% 
  select(latitude, longitude, sst_raw_mean, sst_raw_var, sst_env_col, sst_bounded_seasonality) %>%
  cor(x = ., method = c("spearman")) %>%
  corrplot::corrplot(method = "number")

# variance inflation factor to assess multicollinearity
vif_values = vif(lm(bodysize_cwm_total ~ sst_raw_mean + sst_raw_var + sst_env_col + sst_bounded_seasonality, 
                    data = final_sites_scaled))
vif_values


###### linear models

# linear model BODY SIZE CWM ***************************************************
## build potentially interesting models
global.model = lmer(data = final_sites,
                    log(bodysize_cwm_total) ~ sst_raw_mean + sst_raw_var + sst_env_col + sst_bounded_seasonality + (1 | area))

## qqplot
plot(global.model)
qqnorm(resid(global.model))
qqline(resid(global.model))

## standardize model
stdz.model = standardize(global.model, standardize.y = FALSE)

## dredging
options(na.action = "na.fail")
dredged_cwm_object = MuMIn::dredge(global.model)

## subset top models
best.models <- get.models(dredged_cwm_object, subset = delta < 3)

## average subset
avg.model = model.avg(best.models)
summary(avg.model)

