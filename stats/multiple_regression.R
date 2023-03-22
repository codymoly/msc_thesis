###### MULTIPLE REGRESSION ANALYSIS

# required libraries
## schnick schnack
library(tidyverse)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggpubr)
library(broom)
library(corrplot)
## variance inflation factors
library(car)
## dredging
library(MuMIn)

# clean memory
rm(list=ls())

# set working directory
setwd("~/Documents/MSc_thesis/Figures")

# conditional code
plot_by_area = FALSE
save_to_github = FALSE
save_to_local = FALSE
save_top_models = FALSE

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


###### sample one site per coordinate pair and assign new ID,
##### requires the coordinates for sites with pairwise distance of 30km

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


###### examine multicolinearity

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

# variance inflation factor to assess multicollinearity
vif_cwm = car::vif(lm(size_class_cwm ~ sst_raw_mean + sst_raw_var + sst_env_col + sst_bounded_seasonality, 
                    data = final_sites))
vif_cwm

###### linear models
# note: v1*v2 = v1 + v2 + v1:v2

# CWM
## build null model
null_mod_cwm = lm(size_class_cwm  ~ sst_raw_mean, data = final_sites)
# plot(null_mod_cwm)
summary(null_mod_cwm)

## build saturated model
global.model.class.cwm = lm(size_class_cwm  ~ sst_raw_mean * sst_raw_var * sst_env_col * sst_bounded_seasonality,
                            data = final_sites)
# plot(global.model.class.cwm)

## dredging
options(na.action = "na.fail")
dredged_sc_cwm_object = MuMIn::dredge(global.model.class.cwm, extra = c("R^2", "adjR^2"))
dredged_sc_cwm_object

## get the top models based on cut-off: deltaAICc </= 2
model.sub.sc.cwm <- get.models(dredged_sc_cwm_object, subset = delta < 2) # delta 0.00 and 1.25
best_fit.sc.cwm = model.sub.sc.cwm[[1]]

## AICc of null model and saturated model
AICc(null_mod_cwm) # -113.7783
AICc(global.model.class.cwm) # -122.8208

## model averaging on models with deltaAICc </= 2
averaged_class_cwm = model.avg(model.sub.sc.cwm)
sum.avg.mod = summary(averaged_class_cwm)
sum.avg.mod # summary of averaged models
sw(averaged_class_cwm) # relative importance
confint(sum.avg.mod) # confidence intervals

# ###### depricated; plot predicted versus observed values of null and best model
# ## either take best model from compare_cwm_models or the best dredged
# pred_null_cwm = data.frame(actual = final_sites$sizeclass_cwm_log, predicted = predict(null_mod_cwm))
# cwm.mod0 = ggplot(pred_null_cwm, aes(x = predicted, y = actual)) +
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
#   xlim(-2.8, 2.8) +
#   ylim(-2.8, 2.8)
# 
# pred_obs_sc_cwm = data.frame(actual = final_sites$sizeclass_cwm_log, predicted = predict(best_fit.sc.cwm))
# cwm.mod1 = ggplot(pred_obs_sc_cwm, aes(x = predicted, y = actual)) +
#   geom_point() +
#   geom_abline() +
#   labs(x='predicted values', y='observed values', title='Optimal model based on AICc') +
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
# cwm_pred_plot = ggarrange(cwm.mod0, cwm.mod1, 
#                       ncol = 2, nrow = 1)
# annotate_figure(cwm_pred_plot,
#                 top = text_grob("Size class CWM (log-transformed): predicted vs. observed values",
#                                 color = "black", face = "bold", size = 22))


# CWV
## build null model
null_mod_cwv = lm(size_class_cwv ~ sst_raw_mean, data = final_sites)
# plot(null_mod_cwv)
summary(null_mod_cwv)

## build saturated model
global.model.class.cwv = lm(size_class_cwv  ~ sst_raw_mean * sst_raw_var * sst_env_col * sst_bounded_seasonality,
                            data = final_sites)
# plot(global.model.class.cwv)

## dredging
options(na.action = "na.fail")
dredged_sc_cwv_object = MuMIn::dredge(global.model.class.cwv, extra = c("R^2", "adjR^2"))
dredged_sc_cwv_object

## get the top models based on cut-off: deltaAICc </= 2
model.sub.sc.cwv <- get.models(dredged_sc_cwv_object, subset = delta < 2) # delta 0.00 and 0.44
# best_fit.sc.cwv = model.sub.sc.cwv[[1]]

## AICc of null model and saturated model
AICc(null_mod_cwv) # -31.91236
AICc(global.model.class.cwv) # -24.63129

## model averaging on models with deltaAICc </= 2
avg.model.sc.cwv = model.avg(model.sub.sc.cwv)
sum.avg.mod.cwv = summary(avg.model.sc.cwv)
sum.avg.mod.cwv # summary of averaged models
sw(avg.model.sc.cwv) # relative importance
confint(sum.avg.mod.cwv) # confidence intervals

# ###### depricated; plot predicted versus observed values of null and best model
# ## either take best model from compare_cwm_models or the best dredged
# pred_null_cwv = data.frame(actual = final_sites$sizeclass_cwv_sqrt, predicted = predict(null_mod_cwv))
# cwv.mod0 = ggplot(pred_null_cwv, aes(x = predicted, y = actual)) +
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
#   xlim(-2.5, 7.5) +
#   ylim(-2.5, 7.5)
# 
# pred_obs_sc_cwv = data.frame(actual = final_sites$sizeclass_cwv_sqrt, predicted = predict(best_fit.sc.cwv))
# cwv.mod1 = ggplot(pred_obs_sc_cwv, aes(x = predicted, y = actual)) +
#   geom_point() +
#   geom_abline() +
#   labs(x='predicted values', y='observed values', title='Optimal model based on AICc') +
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
#   xlim(-2.5, 7.5) +
#   ylim(-2.5, 7.5)
# 
# cwv_pred_plot = ggarrange(cwv.mod0, cwv.mod1, 
#                           ncol = 2, nrow = 1)
# annotate_figure(cwv_pred_plot,
#                 top = text_grob("Size class CWV (square root-transformed): predicted vs. observed values",
#                                 color = "black", face = "bold", size = 22))
# 
# ## evenness dredging
# ### global model (the model with all predictors)
# null_mod_eve = lm(even_total  ~ sst_raw_mean, data = final_sites)
# summary(null_mod_eve)
# plot(null_mod_eve)
# 
# global.model.eve = lm(even_total  ~ sst_raw_mean * sst_raw_var * sst_env_col * sst_bounded_seasonality,
#                        data = final_sites)
# plot(global.model.eve)
# 
# ### dredging
# options(na.action = "na.fail")
# dredged_eve_object = MuMIn::dredge(global.model.eve)
# dredged_eve_object
# 
# ### get the top models
# model.sub.eve <- get.models(dredged_eve_object, subset = delta < 2)
# best_fit.eve = model.sub.eve[[1]]
# 
# ### summarise best fitting model
# summary(best_fit.eve)
# 
# # plot resis
# pred_null_eve = data.frame(actual = final_sites$even_total, predicted = predict(null_mod_eve))
# eve.mod0 = ggplot(pred_null_eve, aes(x = predicted, y = actual)) +
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
#   xlim(-3, 3) +
#   ylim(-3, 3)
# 
# pred_obs_eve = data.frame(actual = final_sites$even_total, predicted = predict(best_fit.eve))
# eve.mod1 = ggplot(pred_obs_eve, aes(x = predicted, y = actual)) +
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
#   xlim(-3, 3) +
#   ylim(-3, 3)
# 
# eve_pred_plot = ggarrange(eve.mod0, eve.mod1, 
#                            ncol = 2, nrow = 1)
# annotate_figure(eve_pred_plot,
#                 top = text_grob("Evenness: predicted vs. observed values",
#                                 color = "black", face = "bold", size = 20))


###### save top models for CWM and CWV
# convert weird dredge object into dataframe
dredged_sc_cwm_df = as_tibble(dredged_sc_cwm_object)
dredged_sc_cwv_df = as_tibble(dredged_sc_cwv_object)

# select top models from dredge object
cwm_top_models = dredged_sc_cwm_df[1:9,]
cwv_top_models = dredged_sc_cwv_df[1:7,]

# merge models to predict CWM and CWV
top_models = rbind(cwm_top_models, cwv_top_models)
top_models = round(top_models, digits = 3) # round numbers

# save results
if (save_top_models == TRUE) {
  write.csv(top_models, "/media/mari/Crucial X8/cwm_dredge_models.csv", row.names = TRUE)
} else {
  print("Top models not saved!")
}
