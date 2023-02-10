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


###### joining data

# merge environmental and biological data 
eco_sst = full_join(eco_data, sst_data, by = c("latitude", "longitude", "survey_date"))

# filter sites between 2020 and 2022 (for sites < 2020, the predictability contained missing months)
eco_sst = eco_sst %>% 
  dplyr::filter(survey_date > "2019-12-31")

# join both datasets
eco_sst_30 = dplyr::left_join(coords_30, eco_sst, by = c("latitude", "longitude"))


###### get rid of NAs and outliers

# fill values
eco_env_out = eco_sst_30 # save copy
sst_columns = grep('sst', names(eco_env_out)) # choose columns
eco_env_out[sst_columns] = lapply(eco_env_out[sst_columns], function(x) 
  replace(x, eco_env_out$sst_raw_mean <= 0, NA)) # replace values <= 0 with NAs in sst data
summary(eco_env_out$sst_raw_mean) # check number of NA's 

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

## sst_raw_mean
Q1 <- quantile(eco_env_out$sst_raw_mean, .25, na.rm = TRUE)
Q3 <- quantile(eco_env_out$sst_raw_mean, .75, na.rm = TRUE)
IQR <- IQR(eco_env_out$sst_raw_mean, na.rm = TRUE)
eco_env_out$sst_raw_mean[eco_env_out$sst_raw_mean < (Q1 - 2*IQR)] = NA
eco_env_out$sst_raw_mean[eco_env_out$sst_raw_mean > (Q3 + 2*IQR)] = NA

## sst_bounded_seasonality
Q1 <- quantile(eco_env_out$sst_bounded_seasonality, .25, na.rm = TRUE)
Q3 <- quantile(eco_env_out$sst_bounded_seasonality, .75, na.rm = TRUE)
IQR <- IQR(eco_env_out$sst_bounded_seasonality, na.rm = TRUE)
eco_env_out$sst_bounded_seasonality[eco_env_out$sst_bounded_seasonality < (Q1 - 2*IQR)] = NA
eco_env_out$sst_bounded_seasonality[eco_env_out$sst_bounded_seasonality > (Q3 + 2*IQR)] = NA

# summarise again
summary(eco_env_out) # check number of NA's 
nrow(eco_env_out[complete.cases(eco_env_out),]) # 162

# overwrite dataframe with complete cases
eco_env_out = eco_env_out[complete.cases(eco_env_out),]


###### sample one site per coordinate pair and assign new ID

# choose only one random observation per coordinate pair
set.seed(300)
eco_env = eco_env_out %>% 
  dplyr::group_by(latitude, longitude) %>% 
  dplyr::slice_sample(n = 1) %>% 
  dplyr::ungroup()

# create unique survey id
## arrange by site and add row number as new ID
eco_env = eco_env %>% 
  dplyr::arrange(survey_date, latitude, longitude) %>%
  dplyr::mutate(new_survey_id = row_number())

## transform number into character
eco_env$new_survey_id = as.character(eco_env$new_survey_id)

## add S for survey to each element in new_survey_id
eco_env$new_survey_id = paste("S", eco_env$new_survey_id, sep="")

## move column with survey name to start of the dataframe
eco_env = eco_env %>%
  dplyr::select(new_survey_id, everything())

# save dataset
if (save_my_data == TRUE) {
  write.csv(eco_env,"~/projects/msc_thesis/data/survey_cwm_envpred_data.csv", row.names = FALSE)
  write.csv(eco_env,"/media/mari/Crucial X8/survey_cwm_envpred_data.csv", row.names = FALSE)
} else {
  print("Data not saved!")
}

# summarise data
summary(eco_env)

# check distribution of each variable
hist(na.omit(eco_env$bodysize_cwm_total))
### certain outliers: number_total, bodysize_cwv_total, total_biomass, sst_raw_mean, sst_bounded_seasonality
### slightly skewed: bodysize_cwm_total, sst_raw_var, sst_env_col, sst_colwell_p


###### get know your data

# how many sites do we have now?
## defining which subset will be the final one: subset_num, subset_deg, subset_km
final_sites = eco_env

## count
nrow(final_sites)

# plot map with sites
aussi = st_as_sf(map("worldHires", "Australia", fill=TRUE, xlim=c(110,160), ylim=c(-45,-5), mar=c(0,0,0,0)))
ggplot(data = aussi) + 
  geom_sf() + 
  geom_point(data = as_tibble(final_sites), aes(x = longitude, y = latitude, colour = sst_env_col), size = 3)
### replace data with respective binned dataframe

# check distribution
hist(final_sites$sst_colwell_p) # change variable...


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

vif_values = vif(lm(bodysize_cwv_total ~ sst_raw_mean + sst_raw_var + sst_env_col + sst_bounded_seasonality, 
                    data = final_sites_scaled))
vif_values


###### linear models

# linear model BODY SIZE CWM ***************************************************
## build potentially interesting models
cwm_models <- list(
  cwm1 = lm(bodysize_cwm_total ~ sst_raw_mean, data = final_sites),
  cwm2 = lm(bodysize_cwm_total ~ sst_raw_var, data = final_sites),
  cwm3 = lm(bodysize_cwm_total ~ sst_env_col, data = final_sites),
  cwm4 = lm(bodysize_cwm_total ~ sst_bounded_seasonality, data = final_sites),
  cwm5 = lm(bodysize_cwm_total ~ sst_raw_mean + sst_raw_var, data = final_sites),
  cwm6 = lm(bodysize_cwm_total ~ sst_raw_mean + sst_raw_var + sst_env_col + sst_bounded_seasonality, data = final_sites),
  cwm7 = lm(bodysize_cwm_total ~ sst_raw_mean * sst_raw_var, data = final_sites),
  cwm8 = lm(bodysize_cwm_total ~ sst_raw_mean * sst_raw_var * sst_env_col * sst_bounded_seasonality, data = final_sites))

## compare multiple models
compare_cwm_models = rbind(broom::glance(cwm_models$cwm1),
                         broom::glance(cwm_models$cwm2),
                         broom::glance(cwm_models$cwm3),
                         broom::glance(cwm_models$cwm4),
                         broom::glance(cwm_models$cwm5),
                         broom::glance(cwm_models$cwm6),
                         broom::glance(cwm_models$cwm7),
                         broom::glance(cwm_models$cwm8) # we will use this for the following steps
)

## qqplot
plot(cwm_models$cwm8)

## dredging to find the best model
options(na.action = "na.fail")
dredged_cwm_object = MuMIn::dredge(global.model = cwm_models$cwm6)
confset.d4 <- get.models(dredged_cwm_object, subset = delta < 3)
best_fit = confset.d4[[3]]
summary(best_fit)

options(na.action = "na.fail")
dredged_cwm_object = MuMIn::dredge(global.model = cwm_models$cwm8)
MuMIn::dredge(global.model = cwm_models$cwm8)
confset.d4 <- get.models(dredged_cwm_object, subset = delta < 3)
best_fit = confset.d4[[5]]
summary(best_fit)

## if we take a multi-model approach
MuMIn::model.avg(dredged_cwm_object)

## scientific model: what effect do env col and seasonality have on cwm?
sci_cwm = lm(bodysize_cwm_total ~ sst_env_col, data = final_sites)
summary(sci_cwm) # ß coefficent = sst_env_col: 2.264e-02 , not really interpretable 
# no general effect of env col on cwm
#--> we have to include mean, ie, high means/ low means differently affect the effect of env col

sci_cwm_2 = lm(bodysize_cwm_total ~ sst_bounded_seasonality, data = final_sites)
summary(sci_cwm_2)

sci_cwm_3 = lm(bodysize_cwm_total ~ sst_env_col * sst_raw_mean, data = final_sites)
summary(sci_cwm_3)

sci_cwm_4 = lm(bodysize_cwm_total ~ sst_bounded_seasonality * sst_raw_mean, data = final_sites)
summary(sci_cwm_4) # je höher der mean, desto steiler cwm über seasonality
### contextabhängige effekt von seasonality: scale(sst_bounded_seasonality):scale(sst_raw_mean)  0.16594


# linear model SPECIES RICHNESS ***************************************************
## build potentially interesting models
rich_models <- list(
  rich1 = lm(sp_richness ~ sst_raw_mean, data = final_sites),
  rich2 = lm(sp_richness ~ sst_raw_var, data = final_sites),
  rich3 = lm(sp_richness ~ sst_env_col, data = final_sites),
  rich4 = lm(sp_richness ~ sst_bounded_seasonality, data = final_sites),
  rich5 = lm(sp_richness ~ sst_raw_mean + sst_raw_var, data = final_sites),
  rich6 = lm(sp_richness ~ sst_raw_mean + sst_raw_var + sst_env_col + sst_bounded_seasonality, data = final_sites),
  rich7 = lm(sp_richness ~ sst_raw_mean * sst_raw_var, data = final_sites),
  rich8 = lm(sp_richness ~ sst_raw_mean * sst_raw_var * sst_env_col * sst_bounded_seasonality, data = final_sites))

## compare multiple models
compare_rich_models = rbind(broom::glance(rich_models$rich1),
                           broom::glance(rich_models$rich2),
                           broom::glance(rich_models$rich3),
                           broom::glance(rich_models$rich4),
                           broom::glance(rich_models$rich5),
                           broom::glance(rich_models$rich6),
                           broom::glance(rich_models$rich7),
                           broom::glance(rich_models$rich8) # we will use this for the following steps
)

## qqplot
plot(rich_models$rich8)

## dredging to find the best model
options(na.action = "na.fail")
dredged_rich_object = MuMIn::dredge(global.model = rich_models$rich8)

## if we take a multi-model approach
MuMIn::model.avg(dredged_rich_object)

## scientific model: what effect do env col and seasonality have on cwm?
sci_rich = lm(sp_richness ~ sst_env_col, data = final_sites)
summary(sci_rich) # ß coefficent = sst_env_col: 2.264e-02 , not really interpretable 
# no general effect of env col on cwm
#--> we have to include mean, ie, high means/ low means differently affect the effect of env col

sci_rich_2 = lm(sp_richness ~ sst_bounded_seasonality, data = final_sites)
summary(sci_rich_2)

sci_rich_3 = lm(sp_richness ~ sst_env_col * sst_raw_mean, data = final_sites)
summary(sci_rich_3)

sci_rich_4 = lm(sp_richness ~ sst_bounded_seasonality * sst_raw_mean, data = final_sites)
summary(sci_rich_4)

