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

# import datas
eco_data = readr::read_delim("/media/mari/Crucial X8/cwm_data.csv", delim = ",")
sst_data = readr::read_delim("/media/mari/Crucial X8/env_stats_sst.csv", delim = ",")
coords_30 = readr::read_delim("/media/mari/Crucial X8/coords_30km.csv", delim = ",")
rls_area = readr::read_delim("/media/mari/Crucial X8/rls_2019_2022_upd.csv", skip = 71, delim = ",")

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

# join both datasets
eco_sst_30 = dplyr::left_join(coords_30, eco_sst, by = c("latitude", "longitude"))


###### get rid of NAs and outliers

# fill values
eco_env_out = eco_sst_30 # save copy
sst_columns = grep('sst', names(eco_env_out)) # choose columns
eco_env_out[sst_columns] = lapply(eco_env_out[sst_columns], function(x) 
  replace(x, eco_env_out$sst_raw_mean <= 0, NA)) # replace values <= 0 with NAs in sst data
summary(eco_env_out$sst_raw_mean) # check number of NA's 

# summary
summary(eco_env_out) # check number of NA's 
nrow(eco_env_out[complete.cases(eco_env_out),]) # 177

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

# add area
eco_env = dplyr::left_join(eco_env, rls_area_2, by = c("latitude", "longitude"))

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
hist(na.omit(eco_env$even_total))
### certain outliers: number_total, bodysize_cwv_total, total_biomass, sst_raw_mean, sst_bounded_seasonality
### slightly skewed: bodysize_cwm_total, sst_raw_var, sst_env_col, sst_colwell_p

# transform
eco_env_bu = eco_env
eco_env = eco_env_bu
eco_env = eco_env %>% 
  mutate(bodysize_cwm_sqrt = sqrt(bodysize_cwm_total),
         bodysize_cwv_sqrt = sqrt(bodysize_cwv_total),
         sp_richness_sqrt = sqrt(sp_richness),
         even_sqrt = sqrt(even_total),
         biomass_sqrt = sqrt(total_biomass),
         bodysize_cwm_log = log(bodysize_cwm_total),
         bodysize_cwv_log = log(bodysize_cwv_total),
         sp_richness_log = log(sp_richness),
         even_log = log(even_total),
         biomass_log = log(total_biomass))


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

# scale our variables
final_sites = final_sites %>%
  dplyr::select(area, everything())
final_sites_scaled = final_sites 
final_sites_scaled[6:33] <- lapply(final_sites_scaled[6:33], function(x) c(scale(x)))

# which transformation will it be?
final_sites = final_sites_scaled # or final_sites_log


###### exploratory stats

# quick overview how the variables are related
only_for_corplot = final_sites %>% 
  rename(SSTmean = sst_raw_mean,
         SSTvariance = sst_raw_var,
         SSTnoiseColour = sst_env_col,
         SSTseasonality = sst_bounded_seasonality,
         CWMbodysize = bodysize_cwm_total,
         CWVbodysize = bodysize_cwv_total)

pairs(~ SSTmean +
        SSTvariance +
        SSTnoiseColour +
        SSTseasonality +
        CWMbodysize +
        CWVbodysize, #+
        #even_total +
        #sp_richness +
        #total_biomass,
      data = only_for_corplot,
      cex.labels = 1.5)

# correlation between predictors
only_for_corplot %>%
  na.omit() %>% 
  dplyr::select(SSTmean, SSTvariance, SSTnoiseColour, SSTseasonality) %>%
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

# v1*v2 = v1 + v2 + v1:v2

cwm_models <- list(
  cwm1 = lm(bodysize_cwm_log ~ sst_raw_mean, data = final_sites),
  cwm2 = lm(bodysize_cwm_log  ~ sst_raw_var, data = final_sites),
  cwm3 = lm(bodysize_cwm_log  ~ sst_raw_mean + sst_raw_var + sst_raw_mean:sst_raw_var, data = final_sites),
  cwm4 = lm(bodysize_cwm_log  ~ sst_raw_mean + sst_raw_var + sst_raw_mean:sst_raw_var +
              sst_env_col + sst_raw_mean:sst_env_col, data = final_sites),
  cwm5 = lm(bodysize_cwm_log  ~ sst_raw_mean + sst_raw_var + sst_raw_mean:sst_raw_var +
              sst_bounded_seasonality + sst_raw_mean:sst_bounded_seasonality, data = final_sites),
  cwm6 = lm(bodysize_cwm_log  ~ sst_raw_mean + sst_raw_var + sst_raw_mean:sst_raw_var +
              sst_env_col + sst_bounded_seasonality +
              sst_raw_mean:sst_env_col + sst_raw_mean:sst_bounded_seasonality, data = final_sites),
  cwm7 = lm(bodysize_cwm_log  ~ sst_raw_mean + sst_env_col + sst_raw_mean:sst_env_col, data = final_sites),
  cwm8 = lm(bodysize_cwm_log  ~ sst_raw_mean + sst_bounded_seasonality + sst_raw_mean:sst_bounded_seasonality,
            data = final_sites))

## compare multiple models
compare_cwm_models = rbind(broom::glance(cwm_models$cwm1),
                         broom::glance(cwm_models$cwm2),
                         broom::glance(cwm_models$cwm3),
                         broom::glance(cwm_models$cwm4),
                         broom::glance(cwm_models$cwm5),
                         broom::glance(cwm_models$cwm6),
                         broom::glance(cwm_models$cwm7),
                         broom::glance(cwm_models$cwm8)# we will use this for the following steps
)

## qqplot
plot(cwm_models$cwm4)

plot(x=predict(cwm_models$cwm3), y= final_sites$bodysize_cwm_log,
     xlab='Predicted Values',
     ylab='Observed Values (log)',
     main='Predicted vs. Observed Values')
abline(a=0, b=1)

# cwm dredging
## dredging to find the best model
## global model
global.model = cwm_models$cwm6

## dredging
options(na.action = "na.fail")
dredged_cwm_object = MuMIn::dredge(global.model)
dredged_cwm_object
model.sub <- get.models(dredged_cwm_object, subset = delta < 2)
best_fit = model.sub[[2]]
summary(best_fit)
avg.model = model.avg(model.sub)

### mixed model
mixed6 = lmer(bodysize_cwm_log  ~ sst_raw_mean + sst_raw_var + sst_raw_mean:sst_raw_var +
            sst_env_col + sst_bounded_seasonality +
            sst_raw_mean:sst_env_col + sst_raw_mean:sst_bounded_seasonality +
            (1 | area), 
            data = final_sites)
summary(mixed6)


# linear model BODY SIZE CWV ***************************************************
## build potentially interesting models

# v1*v2 = v1 + v2 + v1:v2

cwv_models <- list(
  cwv1 = lm(bodysize_cwv_log ~ sst_raw_mean, data = final_sites),
  cwv2 = lm(bodysize_cwv_log  ~ sst_raw_var, data = final_sites),
  cwv3 = lm(bodysize_cwv_log   ~ sst_raw_mean + sst_raw_var + sst_raw_mean:sst_raw_var, data = final_sites),
  cwv4 = lm(bodysize_cwv_log   ~ sst_raw_mean + sst_raw_var + sst_raw_mean:sst_raw_var +
              sst_env_col + sst_raw_mean:sst_env_col, data = final_sites),
  cwv5 = lm(bodysize_cwv_log   ~ sst_raw_mean + sst_raw_var + sst_raw_mean:sst_raw_var +
              sst_bounded_seasonality + sst_raw_mean:sst_bounded_seasonality, data = final_sites),
  cwv6 = lm(bodysize_cwv_log   ~ sst_raw_mean + sst_raw_var + sst_raw_mean:sst_raw_var +
              sst_env_col + sst_bounded_seasonality +
              sst_raw_mean:sst_env_col + sst_raw_mean:sst_bounded_seasonality, data = final_sites))

## compare multiple models
compare_cwv_models = rbind(broom::glance(cwv_models$cwv1),
                           broom::glance(cwv_models$cwv2),
                           broom::glance(cwv_models$cwv3),
                           broom::glance(cwv_models$cwv4),
                           broom::glance(cwv_models$cwv5),
                           broom::glance(cwv_models$cwv6) # we will use this for the following steps
)

## qqplot
plot(cwv_models$cwv4)


# linear model richness ***************************************************
## build potentially interesting models

# v1*v2 = v1 + v2 + v1:v2

rich_models <- list(
  rich1 = lm(sp_richness_sqrt ~ sst_raw_mean, data = final_sites),
  rich2 = lm(sp_richness_sqrt  ~ sst_raw_var, data = final_sites),
  rich3 = lm(sp_richness_sqrt   ~ sst_raw_mean + sst_raw_var + sst_raw_mean:sst_raw_var, data = final_sites),
  rich4 = lm(sp_richness_sqrt   ~ sst_raw_mean + sst_raw_var + sst_raw_mean:sst_raw_var +
              sst_env_col + sst_raw_mean:sst_env_col, data = final_sites),
  rich5 = lm(sp_richness_sqrt   ~ sst_raw_mean + sst_raw_var + sst_raw_mean:sst_raw_var +
              sst_bounded_seasonality + sst_raw_mean:sst_bounded_seasonality, data = final_sites),
  rich6 = lm(sp_richness_sqrt   ~ sst_raw_mean + sst_raw_var + sst_raw_mean:sst_raw_var +
              sst_env_col + sst_bounded_seasonality +
              sst_raw_mean:sst_env_col + sst_raw_mean:sst_bounded_seasonality, data = final_sites))

## compare multiple models
compare_rich_models = rbind(broom::glance(rich_models$rich1),
                           broom::glance(rich_models$rich2),
                           broom::glance(rich_models$rich3),
                           broom::glance(rich_models$rich4),
                           broom::glance(rich_models$rich5),
                           broom::glance(rich_models$rich6) # we will use this for the following steps
)

## qqplot
plot(rich_models$rich5)


# linear model evennness ***************************************************
## build potentially interesting models
## take non-transformed evennness

even_models <- list(
  even1 = lm(even_total ~ sst_raw_mean, data = final_sites),
  even2 = lm(even_total  ~ sst_raw_var, data = final_sites),
  even3 = lm(even_total   ~ sst_raw_mean + sst_raw_var + sst_raw_mean:sst_raw_var, data = final_sites),
  even4 = lm(even_total   ~ sst_raw_mean + sst_raw_var + sst_raw_mean:sst_raw_var +
               sst_env_col + sst_raw_mean:sst_env_col, data = final_sites),
  even5 = lm(even_total   ~ sst_raw_mean + sst_raw_var + sst_raw_mean:sst_raw_var +
               sst_bounded_seasonality + sst_raw_mean:sst_bounded_seasonality, data = final_sites),
  even6 = lm(even_total   ~ sst_raw_mean + sst_raw_var + sst_raw_mean:sst_raw_var +
               sst_env_col + sst_bounded_seasonality +
               sst_raw_mean:sst_env_col + sst_raw_mean:sst_bounded_seasonality, data = final_sites))

## compare multiple models
compare_even_models = rbind(broom::glance(even_models$even1),
                            broom::glance(even_models$even2),
                            broom::glance(even_models$even3),
                            broom::glance(even_models$even4),
                            broom::glance(even_models$even5),
                            broom::glance(even_models$even6) # we will use this for the following steps
)

## qqplot
plot(even_models$even5)


# linear model biomass ***************************************************
## build potentially interesting models

# v1*v2 = v1 + v2 + v1:v2

biom_models <- list(
  biom1 = lm(biomass_log ~ sst_raw_mean, data = final_sites),
  biom2 = lm(biomass_log  ~ sst_raw_var, data = final_sites),
  biom3 = lm(biomass_log   ~ sst_raw_mean + sst_raw_var + sst_raw_mean:sst_raw_var, data = final_sites),
  biom4 = lm(biomass_log   ~ sst_raw_mean + sst_raw_var + sst_raw_mean:sst_raw_var +
               sst_env_col + sst_raw_mean:sst_env_col, data = final_sites),
  biom5 = lm(biomass_log   ~ sst_raw_mean + sst_raw_var + sst_raw_mean:sst_raw_var +
               sst_bounded_seasonality + sst_raw_mean:sst_bounded_seasonality, data = final_sites),
  biom6 = lm(biomass_log   ~ sst_raw_mean + sst_raw_var + sst_raw_mean:sst_raw_var +
               sst_env_col + sst_bounded_seasonality +
               sst_raw_mean:sst_env_col + sst_raw_mean:sst_bounded_seasonality, data = final_sites))

## compare multiple models
compare_biom_models = rbind(broom::glance(biom_models$biom1),
                            broom::glance(biom_models$biom2),
                            broom::glance(biom_models$biom3),
                            broom::glance(biom_models$biom4),
                            broom::glance(biom_models$biom5),
                            broom::glance(biom_models$biom6) # we will use this for the following steps
)

## qqplot
plot(biom_models$biom4)





# ## dredging to find the best model
# options(na.action = "na.fail")
# dredged_cwm_object = MuMIn::dredge(global.model = cwm_models$cwm6)
# confset.d4 <- get.models(dredged_cwm_object, subset = delta < 3)
# best_fit = confset.d4[[3]]
# summary(best_fit)
# 
# options(na.action = "na.fail")
# dredged_cwm_object = MuMIn::dredge(global.model = cwm_models$cwm8)
# MuMIn::dredge(global.model = cwm_models$cwm8)
# confset.d4 <- get.models(dredged_cwm_object, subset = delta < 3)
# best_fit = confset.d4[[5]]
# summary(best_fit)
# 
# ## if we take a multi-model approach
# MuMIn::model.avg(dredged_cwm_object)
# 
# 
# ## model averaging approach
# 
# # linear model BODY SIZE CWM ***************************************************
# ## build potentially interesting models
# cwm_models <- list(
#   cwm1 = lm(bodysize_cwm_total ~ sst_raw_mean, data = final_sites),
#   cwm2 = lm(bodysize_cwm_total ~ sst_raw_var, data = final_sites),
#   cwm3 = lm(bodysize_cwm_total ~ sst_env_col, data = final_sites),
#   cwm4 = lm(bodysize_cwm_total ~ sst_bounded_seasonality, data = final_sites),
#   cwm5 = lm(bodysize_cwm_total ~ sst_raw_mean + sst_raw_var + sst_env_col + sst_bounded_seasonality, data = final_sites),
#   cwm6 = lm(bodysize_cwm_total ~ sst_raw_mean * sst_raw_var * sst_env_col * sst_bounded_seasonality, data = final_sites))
# 
# ## compare multiple models
# compare_cwm_models = rbind(broom::glance(cwm_models$cwm1),
#                            broom::glance(cwm_models$cwm2),
#                            broom::glance(cwm_models$cwm3),
#                            broom::glance(cwm_models$cwm4),
#                            broom::glance(cwm_models$cwm5),
#                            broom::glance(cwm_models$cwm6) # we will use this for the following steps
# )
# 
# 
# ## dredging to find the best model
# ## global model
# global.model = cwm_models$cwm6
# 
# plot(global.model)
# 
# ## standardize model
# stdz.model = standardize(global.model, standardize.y = FALSE)
# 
# plot(stdz.model)
# 
# ## dredging
# options(na.action = "na.fail")
# dredged_cwm_object = MuMIn::dredge(stdz.model)
# 
# ## subset top models
# best.models <- get.models(dredged_cwm_object, subset = delta < 2)
# 
# ## average subset
# model.avg(best.models)
# summary(model.avg(best.models))
# 
# 
# 
# 
# mmm = lm(sqrt(bodysize_cwm_total) ~ sst_raw_mean + sst_raw_var + sst_env_col + sst_bounded_seasonality, data = final_sites)
# plot(mmm)
# # transform before standardise
