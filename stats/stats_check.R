# FIRST EXAMINATION OF RELATION BETWEEN TRAITS AND ENVIRONMENTAL STUFF

# read libs
library(tidyverse)
library(grid)
library(gridExtra)
library(ggpubr)

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
grid_coords = read_delim("/media/mari/Crucial X8/grid_coords.csv", delim = ",")

###### data preparation

# merge data for analysis
## eco + sst
eco_env = full_join(eco_data, sst_data, by = c("latitude", "longitude", "survey_date"))
## add chla
eco_env = full_join(eco_env, chla_data, by = c("latitude", "longitude", "survey_date"))
## add grid data
eco_env = left_join(eco_env, grid_coords, by = c("latitude", "longitude"))
## baaaaaam perfect match
# double-check if we have incomplete cases...
nrow(eco_env[complete.cases(eco_env),]) # 201
nrow(eco_env[complete.cases(eco_env$chla_raw_mean),]) # 201
## chla is missing for some sites 

# create unique survey id
## arrange by site
eco_env_copy = eco_env %>% 
  arrange(survey_date, latitude, longitude)
## take row number as numbering
eco_env_copy = eco_env_copy %>%
  dplyr::mutate(new_survey_id = row_number())
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
  print("Data not saved!")
}

# create dataframe with only one site per grid
ecoenv_red = eco_env_copy %>% distinct(nw, .keep_all = TRUE)
eco_env_copy = ecoenv_red
##### data exploration

# get variable names
names(eco_env_copy)
summary(eco_env_copy)
## some sites have sst values <= 0, this might be due to the fill values in the cds dataset
nas_replaced = eco_env_copy # save copy
nm1 = grep('sst', names(nas_replaced)) # choose columns
nas_replaced[nm1] = lapply(nas_replaced[nm1], function(x) 
  replace(x, nas_replaced$sst_raw_mean <= 0, NA)) # replace values <= 0 with NAs in sst data
## sanity check
summary(nas_replaced)

# remove outliers from "sst_bounded_seasonality"
## calculate quantiles
Q1 <- quantile(nas_replaced$chla_env_col, .25, na.rm = TRUE)
Q3 <- quantile(nas_replaced$chla_env_col, .75, na.rm = TRUE)
IQR <- IQR(nas_replaced$chla_env_col, na.rm = TRUE)
## replace outliers in chla_env_col
nas_replaced$chla_env_col[nas_replaced$chla_env_col > (Q3 + 2*IQR)] = NA


##### visual inspection

# sst
pairs(~ sst_raw_mean + 
        sst_raw_var +
        sst_bounded_seasonality +
        sst_env_col, 
      data = nas_replaced)

# chla
pairs(~ chla_raw_mean + 
        chla_raw_var + 
        chla_raw_cv + 
        chla_predicted_var + 
        chla_unpredicted_var + 
        chla_unbounded_seasonality +
        chla_bounded_seasonality +
        chla_env_col, 
      data = nas_replaced)

# all
pairs(~ sst_raw_mean + 
        sst_bounded_seasonality +
        sst_env_col +
        bodysize_cwm_total +
        sp_richness +
        latitude,
      data = nas_replaced)

summary(lm(eco_env_copy$bodysize_cwm_total ~ eco_env_copy$sst_env_col*eco_env_copy$sst_bounded_seasonality))
summary(lm(eco_env_copy$sp_richness ~ eco_env_copy$bodysize_cwm_total))

# 

a = ggplot(nas_replaced, aes(x = "", y = sst_raw_mean)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.y=mean, geom="point", shape=13, size=12) +
  geom_jitter(shape=16, position=position_jitter(0.1))

b = ggplot(nas_replaced, aes(x = "", y = sst_unbounded_seasonality)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.y=mean, geom="point", shape=13, size=12) +
  geom_jitter(shape=16, position=position_jitter(0.1))

c = ggplot(nas_replaced, aes(x = "", y = sst_env_col)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.y=mean, geom="point", shape=13, size=12) +
  geom_jitter(shape=16, position=position_jitter(0.1))

d = ggplot(nas_replaced, aes(x = "", y = sst_colwell_p)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.y=mean, geom="point", shape=13, size=12) +
  geom_jitter(shape=16, position=position_jitter(0.1))

sst_plot = ggarrange(a, b, c, d, 
                      ncol = 2, nrow = 2)
annotate_figure(sst_plot,
                top = text_grob("SST",
                color = "black", face = "bold", size = 16)
                )


e = ggplot(nas_replaced, aes(x = "", y = chla_raw_mean)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.y=mean, geom="point", shape=13, size=12) +
  geom_jitter(shape=16, position=position_jitter(0.1))

f = ggplot(nas_replaced, aes(x = "", y = chla_unbounded_seasonality)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.y=mean, geom="point", shape=13, size=12) +
  geom_jitter(shape=16, position=position_jitter(0.1))

g = ggplot(nas_replaced, aes(x = "", y = chla_env_col)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.y=mean, geom="point", shape=13, size=12) +
  geom_jitter(shape=16, position=position_jitter(0.1))

h = ggplot(nas_replaced, aes(x = "", y = chla_colwell_p)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.y=mean, geom="point", shape=13, size=12) +
  geom_jitter(shape=16, position=position_jitter(0.1))

chl_plot = ggarrange(e, f, g, h, 
                     ncol = 2, nrow = 2)
annotate_figure(chl_plot,
                top = text_grob("ChlA",
                                color = "black", face = "bold", size = 16)
)


i = ggplot(nas_replaced, aes(x = "", y = bodysize_cwm_total)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.y=mean, geom="point", shape=13, size=12) +
  geom_jitter(shape=16, position=position_jitter(0.1))

j = ggplot(nas_replaced, aes(x = "", y = total_biomass)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.y=mean, geom="point", shape=13, size=12) +
  geom_jitter(shape=16, position=position_jitter(0.1))

k = ggplot(nas_replaced, aes(x = "", y = sp_richness)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.y=mean, geom="point", shape=13, size=12) +
  geom_jitter(shape=16, position=position_jitter(0.1))

bio_plot = ggarrange(i, j, k, 
                     ncol = 2, nrow = 2)
annotate_figure(bio_plot,
                top = text_grob("CWM bodysize, biomass, species richness",
                                color = "black", face = "bold", size = 16)
)
