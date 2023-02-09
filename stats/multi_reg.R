# BINNING, OUTLIERS, AND LINEAR MODEL

# read libs
library(tidyverse)
## calculation of distances in km between sites
library(geosphere)
library(grid)
library(gridExtra)
library(ggpubr)
library(broom)
library(corrplot)
## plot Australia map and sites
library(sf)
library(ggplot2)
library(maps)
library(mapdata)


# clean memory
rm(list=ls())

# set working directory
setwd("~/Documents/MSc_thesis")

# conditional executions
save_my_data = FALSE

# import data
eco_data = readr::read_delim("/media/mari/Crucial X8/cwm_data.csv", delim = ",")
sst_data = readr::read_delim("/media/mari/Crucial X8/env_stats_sst.csv", delim = ",")
# chla_data = readr::read_delim("/media/mari/Crucial X8/env_stats_chla.csv", delim = ",")


###### data preparation

# merge environmental and biological data 
eco_env = full_join(eco_data, sst_data, by = c("latitude", "longitude", "survey_date"))

# filter sites between 2020 and 2022 (for sites < 2020, the predictability contained missing months)
eco_env = eco_env %>% 
  dplyr::filter(survey_date > "2019-12-31")

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


###### remove fill values in SST and outliers

# summarise data
summary(eco_env)

# check distribution of each variable
hist(na.omit(eco_env$even_total))
### certain outliers: number_total, bodysize_cwv_total, total_biomass, sst_raw_mean, sst_bounded_seasonality
### slightly skewed: bodysize_cwm_total, sst_raw_var, sst_env_col, sst_colwell_p

# fill values
eco_env_out = eco_env # save copy
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
nrow(eco_env_out[complete.cases(eco_env_out),]) # 767

# overwrite dataframe with complete cases
eco_env_out = eco_env_out[complete.cases(eco_env_out),]


###### create lon-lat bins and subsample

# coordinate bins and subsampling (we want to ensure a certain distance between the sites to be included)
## check range of the coordinates
range(eco_env_out$longitude) # 113.17 167.99
range(eco_env_out$latitude) # -43.53 -9.88

## option 1 BINS: create a certain number (i.e., 110) of bins
eco_env_binned_1 = eco_env_out %>% 
  dplyr::mutate(lon_bin = cut(longitude, breaks = (168-113)/0.5),
         lat_bin = cut(latitude, breaks = (168-113)/0.5))

## option 1 SUBSAMPLE: choose one site per bin
set.seed(200)
subset_num = eco_env_binned_1 %>% 
  dplyr::group_by(lon_bin, lat_bin) %>% 
  dplyr::sample_n(1) %>% 
  dplyr::ungroup()

## option 2 BINS: create bins with a size of 0.5°
### round to the next 0.5, for neg. values we make sure that e.g. -30.75 is rounded to -30.5
eco_env_binned_2 = eco_env_out %>% 
  dplyr::mutate(lon_bin = round(2*longitude, digits = 0)/2,
         lat_bin = round(2*latitude+0.001, digits = 0)/2)

## option 2 SUBSAMPLE: choose one site per bin
set.seed(200)
subset_deg = eco_env_binned_2 %>% 
  dplyr::group_by(lon_bin, lat_bin) %>% 
  dplyr::sample_n(1) %>% 
  dplyr::ungroup()

## check spatial distribution of option 1 and 2
# ggplot(data = subset_number, aes(longitude, latitude)) + # resp., subset_deg
#   geom_point() +
#   geom_point(aes(lon_bin, lat_bin, colour = "red")) + # highlight bin coordinates
#   theme_linedraw()

## option 3 BINS and SUBSAMPLE: choose sites that have a minimum distance of ~30km from each other
out_copy = eco_env_out # use copy for the following steps
column_names = names(out_copy) # write column names into an object
subset_km = data.frame(matrix(nrow = 0, ncol = length(column_names))) # write an empty dataframe
colnames(subset_km) = column_names # assign column names of initial dataframe to the empty one
subset_km[1,] = out_copy[1,] # write the first row of the initial dataframe into the empty one

for (i in 2:nrow(out_copy)) {
  for (ii in 1:nrow(subset_km)) {
    distance = geosphere::distm(c(out_copy$longitude[[i]], out_copy$latitude[[i]]),
                                c(subset_km$longitude[[ii]], subset_km$latitude[[ii]]),
                                fun = distHaversine) # calculate distance between each row
    if (distance < 20000) { # min. dist of ~30km assuming a spherical object, thus precision isworse near equ. and poles
      break
    }
    if (ii == nrow(subset_km)) {
      subset_km = rbind(subset_km, out_copy[i,])
    }
  }
}


###### get know your data

# how many sites do we have now?
## defining which subset will be the final one: subset_num, subset_deg, subset_km
final_sites = subset_km

## count
nrow(final_sites)

# plot map with sites
aussi = st_as_sf(map("worldHires", "Australia", fill=TRUE, xlim=c(110,160), ylim=c(-45,-5), mar=c(0,0,0,0)))
ggplot(data = aussi) + 
  geom_sf() + 
  geom_point(data = as_tibble(final_sites), aes(x = longitude, y = latitude, colour = sst_env_col), size = 3)
### replace data with respective binned dataframe

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

# correlation between response variables
final_sites %>%
  na.omit() %>% 
  select(bodysize_cwm_total, bodysize_cwv_total, sp_richness, shannon, even_total, total_biomass) %>%
  cor(x = ., method = c("spearman")) %>%
  corrplot::corrplot(method = "number")


###### statistics

# if necessary, log-transform data
final_sites_log = final_sites %>% 
  mutate(
    number_total_log = log(number_total),
    bodysize_cwm_total_log = log(bodysize_cwm_total),
    bodysize_cwv_total_log = log(bodysize_cwv_total),
    total_biomass_log = log(total_biomass),
    sst_raw_mean_log = log(sst_raw_mean),
    sst_raw_var_log = log(sst_raw_var),
    sst_bounded_seasonality_log = log(sst_bounded_seasonality),
    sst_colwell_p_log = log(sst_colwell_p)
  ) %>% 
  select(-c("number_total", "bodysize_cwm_total", "bodysize_cwv_total",
            "total_biomass", "sst_raw_mean", "sst_raw_var", "sst_bounded_seasonality", "sst_colwell_p"))

# defining the respective dataset
final_sites = final_sites # or final_sies_log


# linear model BODY SIZE CWM ***************************************************
## build potentially interesting models
cwm_models <- list(
  cwm1 = lm(bodysize_cwm_total ~ sst_raw_mean, data = final_sites),
  cwm2 = lm(bodysize_cwm_total ~ sst_raw_var, data = final_sites),
  cwm3 = lm(bodysize_cwm_total ~ sst_raw_mean * sst_raw_var, data = final_sites),
  cwm4 = lm(bodysize_cwm_total ~ sst_env_col, data = final_sites),
  cwm5 = lm(bodysize_cwm_total ~ sst_bounded_seasonality, data = final_sites),
  cwm6 = lm(bodysize_cwm_total ~ sst_raw_mean * sst_env_col, data = final_sites),
  cwm7 = lm(bodysize_cwm_total ~ sst_raw_mean * sst_bounded_seasonality, data = final_sites),
  cwm8 = lm(bodysize_cwm_total ~ sst_raw_mean * sst_raw_var +
              sst_raw_mean * sst_env_col * sst_bounded_seasonality, data = final_sites),
  cwm9 = lm(bodysize_cwm_total ~ sst_raw_mean * sst_raw_var +
              sst_raw_var * sst_env_col * sst_bounded_seasonality, data = final_sites),
  cwm10 = lm(bodysize_cwm_total ~ sst_raw_mean * sst_raw_var * sst_env_col * sst_bounded_seasonality, data = final_sites),
  cwm11 = lm(bodysize_cwm_total ~ sst_raw_mean * sst_env_col * sst_bounded_seasonality, data = final_sites)
)

## compare multiple models
compare_cwm_models = rbind(broom::glance(cwm_models$cwm1),
                         broom::glance(cwm_models$cwm2),
                         broom::glance(cwm_models$cwm3),
                         broom::glance(cwm_models$cwm4),
                         broom::glance(cwm_models$cwm5),
                         broom::glance(cwm_models$cwm6),
                         broom::glance(cwm_models$cwm7),
                         broom::glance(cwm_models$cwm8),
                         broom::glance(cwm_models$cwm9),
                         broom::glance(cwm_models$cwm10),
                         broom::glance(cwm_models$cwm11)
)

## stepwise regression (dregde) of model 10
step(cwm_models$cwm10) # call the suggested model...

## cross validation 
null_cwm = lm(bodysize_cwm_total ~ 1, data = final_sites)
null_cwm_2 = lm(bodysize_cwm_total ~ sst_raw_mean, data = final_sites)
null_cwm_3 = lm(bodysize_cwm_total ~ sst_raw_mean * sst_raw_var, data = final_sites)
dredge_cwm = lm(formula = bodysize_cwm_total ~ sst_raw_mean + sst_raw_var + 
                  sst_env_col + sst_bounded_seasonality + sst_raw_mean:sst_bounded_seasonality + 
                  sst_raw_var:sst_bounded_seasonality + sst_env_col:sst_bounded_seasonality, 
                data = final_sites)

## summarise null models and dredged model
summary(null_cwm)
summary(null_cwm_2)
summary(null_cwm_3)
summary(dredge_cwm)

## double-check AIC value (step() workd on the AIC values, i.e., the lower the better)
AIC(null_cwm)
AIC(null_cwm_2)
AIC(null_cwm_3)
AIC(dredge_cwm)

## scientific model: what effect do env col and seasonality have on cwm?
sci_cwm = lm(bodysize_cwm_total ~ sst_env_col, data = final_sites)
summary(sci_cwm) # ß coefficent = sst_env_col: 6.800 , not really interpretable

### standardised version
sci_cwm_2 = lm(scale(bodysize_cwm_total) ~ scale(sst_env_col), data = final_sites)
summary(sci_cwm_2) # no general effect of env col on cwm
#--> we have to include mean, ie, high means/ low means differently affect the effect of env col
sci_cwm_3 = lm(scale(bodysize_cwm_total) ~ scale(sst_env_col) + scale(sst_raw_mean), data = final_sites)
summary(sci_cwm_3)

sci_cwm_4 = lm(scale(bodysize_cwm_total) ~ scale(sst_bounded_seasonality) * scale(sst_raw_mean), data = final_sites)
summary(sci_cwm_4) # je höher der mean, desto steiler cwm über seasonality
### contextabhängige effekt von seasonality: scale(sst_bounded_seasonality):scale(sst_raw_mean)  0.16594

