# BINNING, OUTLIERS, AND LINEAR MODEL

# read libs
library(tidyverse)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggpubr)
library(broom)
library(corrplot)

## vif
library(car)

## dredging
library(MuMIn)
## lmer
# library(lme4)
## standardise model
# library(arm)

# clean memory
rm(list=ls())

# set working directory
setwd("~/Documents/MSc_thesis/Figures")

# conditional code
plot_by_area = FALSE
save_to_github = FALSE
save_to_local = FALSE

# import datasets
eco_data = readr::read_delim("/media/mari/Crucial X8/cwm_data.csv", delim = ",")
sst_data = readr::read_delim("/media/mari/Crucial X8/env_stats_sst.csv", delim = ",")
coords_30 = readr::read_delim("/media/mari/Crucial X8/coords_30km.csv", delim = ",")
rls_area_raw = readr::read_delim("/media/mari/Crucial X8/rls_2019_2022_upd.csv", skip = 71, delim = ",")

###### joining data

# merge environmental and biological data 
eco_sst = dplyr::full_join(eco_data, sst_data, by = c("latitude", "longitude", "survey_date"))

# filter sites between 2020 and 2022 (for sites < 2020, the predictability contained missing months)
eco_sst = eco_sst %>% 
  dplyr::filter(survey_date > "2019-12-31")

# select area from second RLS dataset
rls_area = rls_area_raw %>% 
  dplyr::select(longitude, latitude, area) %>% 
  dplyr::distinct(longitude, latitude, .keep_all = TRUE)

# join both datasets
eco_sst_30 = dplyr::left_join(coords_30, eco_sst, by = c("latitude", "longitude"))
eco_sst_30 = dplyr::left_join(eco_sst_30, rls_area, by = c("latitude", "longitude"))

# let's have look if the data is grouped by area...
if (plot_by_area == TRUE) {
  cwm_area = ggplot(data = eco_sst_30, aes(x = sst_env_col, y = bodysize_cwm_total, colour = area)) + 
    geom_point() +
    theme(legend.position="none")
  cwv_area = ggplot(data = eco_sst_30, aes(x = sst_env_col, y = bodysize_cwv_total, colour = area)) + 
    geom_point() +
    theme(legend.position="none")
  richn_area = ggplot(data = eco_sst_30, aes(x = sst_env_col, y = sp_richness, colour = area)) + 
    geom_point()
  
  area_plot = ggarrange(cwm_area, cwv_area, richn_area, 
                       ncol = 2, nrow = 2)
  annotate_figure(area_plot,
                  top = text_grob("CWM, CWV, and species richness grouped by area",
                                  color = "black", face = "bold", size = 16))
                  } else {
  print("No plots with vars grouped by area!")
}



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

# remove systematic outlier
eco_env = eco_env %>% dplyr::filter(size_class_cwv < 800)

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
if (save_to_github == TRUE) {
  write.csv(eco_env,"~/projects/msc_thesis/data/sst_trait_data.csv", row.names = FALSE)
} else {
  print("Data not saved!")
}

if (save_to_local == TRUE) {
  write.csv(eco_env,"/media/mari/Crucial X8/sst_trait_data.csv", row.names = FALSE)
} else {
  print("Data not saved!")
}


###### log transformation

# check distribution of each variable
hist(na.omit(eco_env$sst_env_col))
### skewed: bodysize_cwv_total, total_biomass, sst_raw_mean, bodysize_cwm_total, sst_raw_var

# transform
eco_env_backup = eco_env
# eco_env = eco_env_backup
eco_env = eco_env %>%
  dplyr::mutate(bodysize_cwm_sqrt = sqrt(bodysize_cwm_total),
         bodysize_cwv_sqrt = sqrt(bodysize_cwv_total),
         sp_richness_sqrt = sqrt(sp_richness),
         even_sqrt = sqrt(even_total),
         biomass_sqrt = sqrt(total_biomass),
         sizeclass_cwm_sqrt = sqrt(size_class_cwm),
         sizeclass_cwv_sqrt = sqrt(size_class_cwv),
         bodysize_cwm_log = log(bodysize_cwm_total),
         bodysize_cwv_log = log(bodysize_cwv_total),
         sp_richness_log = log(sp_richness),
         even_log = log(even_total+0.01),
         biomass_log = log(total_biomass),
         sizeclass_cwm_log = log(size_class_cwm),
         sizeclass_cwv_log = log(size_class_cwv))

# # scale our variables
## move area to the beginning of the dataframe
eco_env = eco_env %>%
  dplyr::select(area, everything())

## copy copy copy
final_sites = eco_env

## remove outlier in cvw?
hist(final_sites$size_class_cwm)

## standardise 
# final_sites[6:39] <- lapply(final_sites[6:39], function(x) c(scale(x)))
final_sites[6:39] <- lapply(final_sites[6:39], function(x) c((x - min(x))/(max(x) - min(x))))


###### examine collinearity

# quick overview how the variables are related
## rename variables to make it more readable
only_for_corplot = final_sites %>% 
  dplyr::rename(`SST mean` = sst_raw_mean,
                `SST variance` = sst_raw_var,
                `SST noise colour` = sst_env_col,
                `SST seasonality` = sst_bounded_seasonality,
                `CWM body size` = bodysize_cwm_total,
                `CWV body size` = bodysize_cwv_total,
                `CWM size class` = size_class_cwm,
                `CWV size class` = size_class_cwv,
                `species richness` = sp_richness)

# plot all variables
pairs(~ `SST mean` +
        `SST variance` +
        `SST noise colour` +
        `SST seasonality` +
        `CWM body size` +
        `CWV body size` +
        `CWM size class` +
        `CWV size class` +
        `species richness`,
      data = only_for_corplot,
      cex.labels = 1.5)

# correlation between predictors
only_for_corplot %>%
  dplyr::select(`SST mean`, `SST variance`, `SST noise colour`, `SST seasonality`) %>%
  stats::cor(x = ., method = c("spearman")) %>%
  corrplot::corrplot(method = "number")

# # correlationplot between response variables
# only_for_corplot %>%
#   dplyr::select(`CWM body size`, `CWV body size`, `species richness`, `CWM size class`, `CWV size class`) %>%
#   stats::cor(x = ., method = c("spearman")) %>%
#   corrplot::corrplot(method = "number")

# variance inflation factor to assess multicollinearity
vif_cwm = car::vif(lm(total_biomass ~ sst_raw_mean + sst_raw_var + sst_env_col + sst_bounded_seasonality, 
                    data = final_sites))
vif_cwm

###### linear models
# note: v1*v2 = v1 + v2 + v1:v2

# linear model BODY SIZE CWM ***************************************************
# ## build potentially interesting models
# cwm_models <- list(
#   cwm1 = lm(bodysize_cwm_log ~ sst_raw_mean, data = final_sites),
#   cwm2 = lm(bodysize_cwm_log  ~ sst_raw_var, data = final_sites),
#   cwm3 = lm(bodysize_cwm_log  ~ sst_raw_mean + sst_raw_var + sst_raw_mean:sst_raw_var, data = final_sites),
#   cwm4 = lm(bodysize_cwm_log  ~ sst_raw_mean + sst_raw_var + sst_raw_mean:sst_raw_var +
#               sst_env_col + sst_raw_mean:sst_env_col, data = final_sites),
#   cwm5 = lm(bodysize_cwm_log  ~ sst_raw_mean + sst_raw_var + sst_raw_mean:sst_raw_var +
#               sst_bounded_seasonality + sst_raw_mean:sst_bounded_seasonality, data = final_sites),
#   cwm6 = lm(bodysize_cwm_log  ~ sst_raw_mean + sst_raw_var + sst_raw_mean:sst_raw_var +
#               sst_env_col + sst_bounded_seasonality +
#               sst_raw_mean:sst_env_col + sst_raw_mean:sst_bounded_seasonality, data = final_sites),
#   cwm7 = lm(bodysize_cwm_log  ~ sst_raw_mean + sst_env_col + sst_raw_mean:sst_env_col, data = final_sites),
#   cwm8 = lm(bodysize_cwm_log  ~ sst_raw_mean + sst_bounded_seasonality + sst_raw_mean:sst_bounded_seasonality,
#             data = final_sites))
# 
# ## compare multiple models
# compare_cwm_models = rbind(broom::glance(cwm_models$cwm1),
#                          broom::glance(cwm_models$cwm2),
#                          broom::glance(cwm_models$cwm3),
#                          broom::glance(cwm_models$cwm4),
#                          broom::glance(cwm_models$cwm5),
#                          broom::glance(cwm_models$cwm6),
#                          broom::glance(cwm_models$cwm7),
#                          broom::glance(cwm_models$cwm8)# we will use this for the following steps
# )
# compare_cwm_models

# check residual distribution of the best model
# plot(cwm_models$cwm3)

# ## summarise
# summary(cwm_models$cwm3)
# ### calculation for log-transformed response variables (exp(slope)-1)*100 
# ### i.e., for every one-unit increase in the mean, bodysize_cwm decreases by about 49%

## cwm dredging
### global model (the model with all predictors)
# global.model = lm(bodysize_cwm_log  ~ sst_raw_mean * sst_raw_var * sst_env_col * sst_bounded_seasonality,
#                   data = final_sites)
# plot(global.model)
# 
# ### dredging
# options(na.action = "na.fail")
# dredged_cwm_object = MuMIn::dredge(global.model)
# dredged_cwm_object
# 
# ### get the top models
# model.sub <- get.models(dredged_cwm_object, subset = delta < 2)
# best_fit = model.sub[[1]]
# model96 = model.sub[[1]]
# model7040 = model.sub[[2]]
# 
# ### summarise best fitting model
# summary(model96) # plot seas and col, 
# anova(model96, model7040)
# 
# ### if the top models are too similar
# avg.model = model.avg(model.sub)
# summary(avg.model)
# 
# ## plot predicted versus observed models
# ## either take best model from compare_cwm_models or the best dredged
# pred_obs_cwm = data.frame(actual = final_sites$bodysize_cwm_log, predicted = predict(best_fit))
# ggplot(pred_obs_cwm, aes(x = predicted, y = actual)) +
#   geom_point() +
#   geom_abline() +
#   labs(x='predicted values', y='observed values', title='CWM body size: predicted vs. observed values (log-transformed)') +
#   theme_classic() +
#   theme(plot.title=element_text(size=18, face= "bold"),
#         legend.title=element_blank(),
#         legend.text = element_text(size = 18, face= "bold"),
#         axis.title.x = element_text(size = 18, face= "bold"),
#         axis.title.y = element_text(size = 18, face= "bold"),
#         axis.text.x = element_text(size = 18, face= "bold"),
#         axis.text.y = element_text(size = 18, face= "bold"),
#         axis.line = element_line(linewidth = 1),
#         axis.ticks.length=unit(.25, "cm")) +
#   xlim(-2.8, 2.8) +
#   ylim(-2.8, 2.8)
# 
# 
# # linear model BODY SIZE CWV ***************************************************
# # ## build potentially interesting models
# # cwv_models <- list(
# #   cwv1 = lm(bodysize_cwv_log ~ sst_raw_mean, data = final_sites),
# #   cwv2 = lm(bodysize_cwv_log  ~ sst_raw_var, data = final_sites),
# #   cwv3 = lm(bodysize_cwv_log   ~ sst_raw_mean + sst_raw_var + sst_raw_mean:sst_raw_var, data = final_sites),
# #   cwv4 = lm(bodysize_cwv_log   ~ sst_raw_mean + sst_raw_var + sst_raw_mean:sst_raw_var +
# #               sst_env_col + sst_raw_mean:sst_env_col, data = final_sites),
# #   cwv5 = lm(bodysize_cwv_log   ~ sst_raw_mean + sst_raw_var + sst_raw_mean:sst_raw_var +
# #               sst_bounded_seasonality + sst_raw_mean:sst_bounded_seasonality, data = final_sites),
# #   cwv6 = lm(bodysize_cwv_log   ~ sst_raw_mean + sst_raw_var + sst_raw_mean:sst_raw_var +
# #               sst_env_col + sst_bounded_seasonality +
# #               sst_raw_mean:sst_env_col + sst_raw_mean:sst_bounded_seasonality, data = final_sites))
# # 
# # ## compare multiple models
# # compare_cwv_models = rbind(broom::glance(cwv_models$cwv1),
# #                            broom::glance(cwv_models$cwv2),
# #                            broom::glance(cwv_models$cwv3),
# #                            broom::glance(cwv_models$cwv4),
# #                            broom::glance(cwv_models$cwv5),
# #                            broom::glance(cwv_models$cwv6) # we will use this for the following steps
# # )
# # compare_cwv_models
# # 
# # ## check residual distribution of the best model
# # plot(cwv_models$cwv6)
# # 
# # ## summarise
# # summary(cwv_models$cwv6)
# 
# ## cwv dredging
# ### global model (the model with all predictors)
# global.model.cwv = lm(bodysize_cwv_log ~ sst_raw_mean * sst_raw_var * sst_env_col * sst_bounded_seasonality,
#                   data = final_sites)
# plot(global.model.cwv)
# 
# ### dredging
# options(na.action = "na.fail")
# dredged_cwv_object = MuMIn::dredge(global.model.cwv)
# dredged_cwv_object
# 
# ### get the top models
# model.sub.cwv <- get.models(dredged_cwv_object, subset = delta < 2)
# best_fit.cwv = model.sub.cwv[[1]]
# cwv7040  = model.sub.cwv[[1]]
# cwv32768  = model.sub.cwv[[2]]
# anova(cwv7040, cwv32768)
# 
# ### summarise best fitting model
# summary(best_fit.cwv)
# 
# ### if the top models are too similar
# avg.model.cwv = model.avg(model.sub.cwv)
# summary(avg.model.cwv)
# 
# ## plot predicted versus observed models
# ## either take best model from compare_cwm_models or the best dredged
# pred_obs_cwv = data.frame(actual = final_sites$bodysize_cwv_log, predicted = predict(best_fit.cwv))
# ggplot(pred_obs_cwv, aes(x = predicted, y = actual)) +
#   geom_point() +
#   geom_abline() +
#   labs(x='predicted values', y='observed values', title='CWV: predicted vs. observed values (log-transformed)') +
#   theme_classic() +
#   theme(plot.title=element_text(size=18, face= "bold"),
#         legend.title=element_blank(),
#         legend.text = element_text(size = 18, face= "bold"),
#         axis.title.x = element_text(size = 18, face= "bold"),
#         axis.title.y = element_text(size = 18, face= "bold"),
#         axis.text.x = element_text(size = 18, face= "bold"),
#         axis.text.y = element_text(size = 18, face= "bold"),
#         axis.line = element_line(linewidth = 1),
#         axis.ticks.length=unit(.25, "cm")) +
#   xlim(-2.8, 2.8) +
#   ylim(-2.8, 2.8)
# 

# linear model richness ***************************************************
# ## build potentially interesting models
# rich_models <- list(
#   rich1 = lm(sp_richness_log ~ sst_raw_mean, data = final_sites),
#   rich2 = lm(sp_richness_log ~ sst_raw_var, data = final_sites),
#   rich3 = lm(sp_richness_log ~ sst_raw_mean + sst_raw_var + sst_raw_mean:sst_raw_var, data = final_sites),
#   rich4 = lm(sp_richness_log ~ sst_raw_mean + sst_raw_var + sst_raw_mean:sst_raw_var +
#               sst_env_col + sst_raw_mean:sst_env_col, data = final_sites),
#   rich5 = lm(sp_richness_log ~ sst_raw_mean + sst_raw_var + sst_raw_mean:sst_raw_var +
#               sst_bounded_seasonality + sst_raw_mean:sst_bounded_seasonality, data = final_sites),
#   rich6 = lm(sp_richness_log ~ sst_raw_mean + sst_raw_var + sst_raw_mean:sst_raw_var +
#               sst_env_col + sst_bounded_seasonality +
#               sst_raw_mean:sst_env_col + sst_raw_mean:sst_bounded_seasonality, data = final_sites))
# 
# ## compare multiple models
# compare_rich_models = rbind(broom::glance(rich_models$rich1),
#                            broom::glance(rich_models$rich2),
#                            broom::glance(rich_models$rich3),
#                            broom::glance(rich_models$rich4),
#                            broom::glance(rich_models$rich5),
#                            broom::glance(rich_models$rich6) # we will use this for the following steps
# )
# compare_rich_models
# 
# ## check residual distribution of the best model
# plot(rich_models$rich5)
# 
# ## summarise
# summary(rich_models$rich5)

# ## richness dredging
# ### global model (the model with all predictors)
# null_mod_rich = lm(sp_richness_log  ~ sst_raw_mean, data = final_sites)
# summary(null_mod_rich)
# 
# global.model.rich = lm(sp_richness_log  ~ sst_raw_mean * sst_raw_var * sst_env_col * sst_bounded_seasonality,
#                       data = final_sites)
# plot(global.model.rich)
# 
# ### dredging
# options(na.action = "na.fail")
# dredged_rich_object = MuMIn::dredge(global.model.rich)
# dredged_rich_object
# 
# ### get the top models
# model.sub.rich <- get.models(dredged_rich_object, subset = delta < 2)
# best_fit.rich = model.sub.rich[[1]]
# best_fit.rich.2 = model.sub.rich[[2]]
# anova(best_fit.rich, best_fit.rich.2)
# 
# ### summarise best fitting model
# summary(best_fit.rich)
# 
# ### if the top models are too similar
# avg.model.rich = model.avg(model.sub.rich)
# summary(avg.model.rich)
# # path analysis, strucutural equation modelling
# ## plot predicted versus observed models
# ## either take best model from compare_cwm_models or the best dredged
# pred_null_rich = data.frame(actual = final_sites$sp_richness_log, predicted = predict(null_mod_rich))
# rich.mod0 = ggplot(pred_null_rich, aes(x = predicted, y = actual)) +
#   geom_point() +
#   geom_abline() +
#   labs(x='predicted values', y='observed values', title='Null model') +
#   theme_classic() +
#   theme(plot.title=element_text(size=18, face= "bold"),
#         legend.title=element_blank(),
#         legend.text = element_text(size = 18, face= "bold"),
#         axis.title.x = element_text(size = 18, face= "bold"),
#         axis.title.y = element_text(size = 18, face= "bold"),
#         axis.text.x = element_text(size = 18, face= "bold"),
#         axis.text.y = element_text(size = 18, face= "bold"),
#         axis.line = element_line(linewidth = 1),
#         axis.ticks.length=unit(.25, "cm")) +
#   xlim(-3.8, 2) +
#   ylim(-3.8, 2)
# 
# pred_obs_rich = data.frame(actual = final_sites$sp_richness_log, predicted = predict(best_fit.rich))
# rich.mod1 = ggplot(pred_obs_rich, aes(x = predicted, y = actual)) +
#   geom_point() +
#   geom_abline() +
#   labs(x='predicted values', y='observed values', title='Highest-ranked model based on AICc') +
#   theme_classic() +
#   theme(plot.title=element_text(size=18, face= "bold"),
#         legend.title=element_blank(),
#         legend.text = element_text(size = 18, face= "bold"),
#         axis.title.x = element_text(size = 18, face= "bold"),
#         axis.title.y = element_text(size = 18, face= "bold"),
#         axis.text.x = element_text(size = 18, face= "bold"),
#         axis.text.y = element_text(size = 18, face= "bold"),
#         axis.line = element_line(linewidth = 1),
#         axis.ticks.length=unit(.25, "cm")) +
#   xlim(-3.8, 2) +
#   ylim(-3.8, 2)
# 
# rich_pred_plot = ggarrange(rich.mod0, rich.mod1, 
#                           ncol = 2, nrow = 1)
# annotate_figure(rich_pred_plot,
#                 top = text_grob("Species richness (log-transformed): predicted vs. observed values",
#                                 color = "black", face = "bold", size = 20))

## size class cwm dredging
### global model (the model with all predictors)
null_mod_cwm = lm(size_class_cwm  ~ sst_raw_mean, data = final_sites)
plot(null_mod_cwm)
summary(null_mod_cwm)

global.model.class.cwm = lm(size_class_cwm  ~ sst_raw_mean * sst_raw_var * sst_env_col * sst_bounded_seasonality,
                       data = final_sites)
plot(global.model.class.cwm)

### dredging
options(na.action = "na.fail")
dredged_sc_cwm_object = MuMIn::dredge(global.model.class.cwm)
dredged_sc_cwm_object

### get the top models
model.sub.sc.cwm <- get.models(dredged_sc_cwm_object, subset = delta < 2) # delta 0.00 and 1.25
best_fit.sc.cwm = model.sub.sc.cwm[[1]]
best_fit.sc.cwm.2 = model.sub.sc.cwm[[2]]

### summarise best fitting model
summary(best_fit.sc.cwm)
summary(best_fit.sc.cwm.2)

####
anova(model.sub.sc.cwm[[1]], model.sub.sc.cwm[[2]])

### model averaging on models with aicc <2
averaged_class_cwm = model.avg(model.sub.sc.cwm)
summary(averaged_class_cwm)

# path analysis, strucutural equation modelling
## plot predicted versus observed models
## either take best model from compare_cwm_models or the best dredged
pred_null_cwm = data.frame(actual = final_sites$sizeclass_cwm_log, predicted = predict(null_mod_cwm))
cwm.mod0 = ggplot(pred_null_cwm, aes(x = predicted, y = actual)) +
  geom_point() +
  geom_abline() +
  labs(x='predicted values', y='observed values', title='Null model') +
  theme_classic() +
  theme(plot.title=element_text(size=18, face= "bold"),
        legend.title=element_blank(),
        legend.text = element_text(size = 18, face= "bold"),
        axis.title.x = element_text(size = 18, face= "bold"),
        axis.title.y = element_text(size = 18, face= "bold"),
        axis.text.x = element_text(size = 18, face= "bold"),
        axis.text.y = element_text(size = 18, face= "bold"),
        axis.line = element_line(linewidth = 1),
        axis.ticks.length=unit(.25, "cm")) +
  xlim(-2.8, 2.8) +
  ylim(-2.8, 2.8)

pred_obs_sc_cwm = data.frame(actual = final_sites$sizeclass_cwm_log, predicted = predict(best_fit.sc.cwm))
cwm.mod1 = ggplot(pred_obs_sc_cwm, aes(x = predicted, y = actual)) +
  geom_point() +
  geom_abline() +
  labs(x='predicted values', y='observed values', title='Optimal model based on AICc') +
  theme_classic() +
  theme(plot.title=element_text(size=18, face= "bold"),
        legend.title=element_blank(),
        legend.text = element_text(size = 18, face= "bold"),
        axis.title.x = element_text(size = 18, face= "bold"),
        axis.title.y = element_text(size = 18, face= "bold"),
        axis.text.x = element_text(size = 18, face= "bold"),
        axis.text.y = element_text(size = 18, face= "bold"),
        axis.line = element_line(linewidth = 1),
        axis.ticks.length=unit(.25, "cm")) +
  xlim(-2.8, 2.8) +
  ylim(-2.8, 2.8)

cwm_pred_plot = ggarrange(cwm.mod0, cwm.mod1, 
                      ncol = 2, nrow = 1)
annotate_figure(cwm_pred_plot,
                top = text_grob("Size class CWM (log-transformed): predicted vs. observed values",
                                color = "black", face = "bold", size = 22))

## size class cwv dredging
### global model (the model with all predictors)
null_mod_cwv = lm(size_class_cwv ~ sst_raw_mean, data = final_sites)
plot(null_mod_cwv)
summary(null_mod_cwv)

global.model.class.cwv = lm(size_class_cwv  ~ sst_raw_mean * sst_raw_var * sst_env_col * sst_bounded_seasonality,
                            data = final_sites)
plot(global.model.class.cwv)

### dredging
options(na.action = "na.fail")
dredged_sc_cwv_object = MuMIn::dredge(global.model.class.cwv)
dredged_sc_cwv_object

### get the top models
model.sub.sc.cwv <- get.models(dredged_sc_cwv_object, subset = delta < 2) # delta 0.00 and 0.44
best_fit.sc.cwv = model.sub.sc.cwv[[1]]
best_fit.sc.cwv.2 = model.sub.sc.cwv[[2]]
best_fit.sc.cwv.3 = model.sub.sc.cwv[[3]]

### summarise best fitting model
anova(best_fit.sc.cwv, best_fit.sc.cwv.2) # 0.1882
summary(best_fit.sc.cwv)
summary(best_fit.sc.cwv.3)

### if the top models are too similar
avg.model.sc.cwv = model.avg(model.sub.sc.cwv)
summary(avg.model.sc.cwv)

# path analysis, strucutural equation modelling
## plot predicted versus observed models
## either take best model from compare_cwm_models or the best dredged
pred_null_cwv = data.frame(actual = final_sites$sizeclass_cwv_sqrt, predicted = predict(null_mod_cwv))
cwv.mod0 = ggplot(pred_null_cwv, aes(x = predicted, y = actual)) +
  geom_point() +
  geom_abline() +
  labs(x='predicted values', y='observed values', title='Null model') +
  theme_classic() +
  theme(plot.title=element_text(size=18, face= "bold"),
        legend.title=element_blank(),
        legend.text = element_text(size = 18, face= "bold"),
        axis.title.x = element_text(size = 18, face= "bold"),
        axis.title.y = element_text(size = 18, face= "bold"),
        axis.text.x = element_text(size = 18, face= "bold"),
        axis.text.y = element_text(size = 18, face= "bold"),
        axis.line = element_line(linewidth = 1),
        axis.ticks.length=unit(.25, "cm")) +
  xlim(-2.5, 7.5) +
  ylim(-2.5, 7.5)

pred_obs_sc_cwv = data.frame(actual = final_sites$sizeclass_cwv_sqrt, predicted = predict(best_fit.sc.cwv))
cwv.mod1 = ggplot(pred_obs_sc_cwv, aes(x = predicted, y = actual)) +
  geom_point() +
  geom_abline() +
  labs(x='predicted values', y='observed values', title='Optimal model based on AICc') +
  theme_classic() +
  theme(plot.title=element_text(size=18, face= "bold"),
        legend.title=element_blank(),
        legend.text = element_text(size = 18, face= "bold"),
        axis.title.x = element_text(size = 18, face= "bold"),
        axis.title.y = element_text(size = 18, face= "bold"),
        axis.text.x = element_text(size = 18, face= "bold"),
        axis.text.y = element_text(size = 18, face= "bold"),
        axis.line = element_line(linewidth = 1),
        axis.ticks.length=unit(.25, "cm")) +
  xlim(-2.5, 7.5) +
  ylim(-2.5, 7.5)

cwv_pred_plot = ggarrange(cwv.mod0, cwv.mod1, 
                          ncol = 2, nrow = 1)
annotate_figure(cwv_pred_plot,
                top = text_grob("Size class CWV (square root-transformed): predicted vs. observed values",
                                color = "black", face = "bold", size = 22))

## evenness dredging
### global model (the model with all predictors)
null_mod_eve = lm(even_total  ~ sst_raw_mean, data = final_sites)
summary(null_mod_eve)
plot(null_mod_eve)

global.model.eve = lm(even_total  ~ sst_raw_mean * sst_raw_var * sst_env_col * sst_bounded_seasonality,
                       data = final_sites)
plot(global.model.eve)

### dredging
options(na.action = "na.fail")
dredged_eve_object = MuMIn::dredge(global.model.eve)
dredged_eve_object

### get the top models
model.sub.eve <- get.models(dredged_eve_object, subset = delta < 2)
best_fit.eve = model.sub.eve[[1]]

### summarise best fitting model
summary(best_fit.eve)

# plot resis
pred_null_eve = data.frame(actual = final_sites$even_total, predicted = predict(null_mod_eve))
eve.mod0 = ggplot(pred_null_eve, aes(x = predicted, y = actual)) +
  geom_point() +
  geom_abline() +
  labs(x='predicted values', y='observed values', title='Null model') +
  theme_classic() +
  theme(plot.title=element_text(size=18, face= "bold"),
        legend.title=element_blank(),
        legend.text = element_text(size = 18, face= "bold"),
        axis.title.x = element_text(size = 18, face= "bold"),
        axis.title.y = element_text(size = 18, face= "bold"),
        axis.text.x = element_text(size = 18, face= "bold"),
        axis.text.y = element_text(size = 18, face= "bold"),
        axis.line = element_line(linewidth = 1),
        axis.ticks.length=unit(.25, "cm")) +
  xlim(-3, 3) +
  ylim(-3, 3)

pred_obs_eve = data.frame(actual = final_sites$even_total, predicted = predict(best_fit.eve))
eve.mod1 = ggplot(pred_obs_eve, aes(x = predicted, y = actual)) +
  geom_point() +
  geom_abline() +
  labs(x='predicted values', y='observed values', title='Highest-ranked model based on AICc') +
  theme_classic() +
  theme(plot.title=element_text(size=18, face= "bold"),
        legend.title=element_blank(),
        legend.text = element_text(size = 18, face= "bold"),
        axis.title.x = element_text(size = 18, face= "bold"),
        axis.title.y = element_text(size = 18, face= "bold"),
        axis.text.x = element_text(size = 18, face= "bold"),
        axis.text.y = element_text(size = 18, face= "bold"),
        axis.line = element_line(linewidth = 1),
        axis.ticks.length=unit(.25, "cm")) +
  xlim(-3, 3) +
  ylim(-3, 3)

eve_pred_plot = ggarrange(eve.mod0, eve.mod1, 
                           ncol = 2, nrow = 1)
annotate_figure(eve_pred_plot,
                top = text_grob("Evenness: predicted vs. observed values",
                                color = "black", face = "bold", size = 20))


write.csv(final_sites,"/media/mari/Crucial X8/sst_trait_data_norm.csv", row.names = FALSE)
