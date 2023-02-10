# FIRST EXAMINATION OF RELATION BETWEEN TRAITS AND ENVIRONMENTAL STUFF

# read libs
library(tidyverse)
library(grid)
library(gridExtra)
library(ggpubr)
library(geosphere)
library(car)
library(MuMIn)
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
eco_data = read_delim("/media/mari/Crucial X8/cwm_depth_data.csv", delim = ",")
sst_data = read_delim("/media/mari/Crucial X8/env_stats_sst.csv", delim = ",")


###### data preparation

# merge environmental and biological data 
## eco + sst
eco_env = full_join(eco_data, sst_data, by = c("latitude", "longitude", "survey_date"))

# we still have missing data in 2009, thus filter cases with envpred > 2009
eco_env = eco_env %>% 
  dplyr::filter(survey_date > "2019-12-31") #%>% 
 # dplyr::filter(latitude > -30) %>% 
  #dplyr::filter(longitude > 120) %>% 
  #dplyr::filter(depth_bin != "(20,30]")
  
# create unique survey id
## arrange by site
eco_env = eco_env %>% 
  arrange(survey_date, latitude, longitude)
## take row number as numbering
eco_env = eco_env %>%
  dplyr::mutate(new_survey_id = row_number())
## transform number into character
eco_env$new_survey_id = as.character(eco_env$new_survey_id)
## add s for survey to each element
eco_env$new_survey_id = paste("S", eco_env$new_survey_id, sep="")
## move column with survey name to start
eco_env = eco_env %>%
  select(new_survey_id, everything())

# save dataset
if (save_my_data == TRUE) {
  write.csv(eco_env,"~/projects/msc_thesis/data/survey_cwm_envpred_data.csv", row.names = FALSE)
  write.csv(eco_env,"/media/mari/Crucial X8/survey_cwm_envpred_data.csv", row.names = FALSE)
} else {
  print("Data not saved!")
}


###### create lon-lat bins and subsample

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
    if (distance < 30000) { # min. dist of ~30km assuming a spherical object, thus precision isworse near equ. and poles
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

## scale our variables
final_sites_scaled = final_sites 
final_sites_scaled[6:22] <- lapply(final_sites_scaled[6:22], function(x) c(scale(x)))
final_sites = final_sites_scaled # or final_sites

# variance inflation factor to assess multicolinearity
#create vector of VIF values
vif(lm(bodysize_cwm_total ~ longitude + latitude + sst_raw_mean + depth_bin + sst_raw_var + sst_env_col + sst_bounded_seasonality, 
                    data = final_sites_scaled))

vif(lm(bodysize_cwm_total ~ latitude + sst_raw_mean + depth_bin + sst_raw_var + sst_env_col + sst_bounded_seasonality, 
       data = final_sites_scaled))

vif(lm(bodysize_cwm_total ~ sst_raw_mean + depth_bin + sst_raw_var + sst_env_col + sst_bounded_seasonality, 
                    data = final_sites_scaled))



###### statistics

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
dredged_cwm_object = MuMIn::dredge(global.model = cwm_models$cwm8)
confset.d4 <- get.models(dredged_cwm_object, subset = delta < 3)
best_fit = confset.d4[[1]]
summary(best_fit)
