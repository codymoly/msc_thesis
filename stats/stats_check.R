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
save_my_data = FALSE

# import data
eco_data = read_delim("/media/mari/Crucial X8/cwm_data.csv", delim = ",")
sst_data = read_delim("/media/mari/Crucial X8/env_stats_sst.csv", delim = ",")
chla_data = read_delim("/media/mari/Crucial X8/env_stats_chla.csv", delim = ",")


###### data preparation

# merge environmental and biological data 
## eco + sst
eco_env = full_join(eco_data, sst_data, by = c("latitude", "longitude", "survey_date"))
## add chla
eco_env = full_join(eco_env, chla_data, by = c("latitude", "longitude", "survey_date"))

# we still have missing data in 2009, thus filter cases with envpred > 2009
eco_env = eco_env %>% 
  dplyr::filter(survey_date > "2019-12-31")

# check complete cases
nrow(eco_env[complete.cases(eco_env),]) # 201
nrow(eco_env[complete.cases(eco_env$sst_raw_mean),]) # 1249
nrow(eco_env[complete.cases(eco_env$chla_raw_mean),]) # 201

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

# create coordinate bins
eco_env_binned = eco_env %>% 
  mutate(lon_bin = cut(longitude, breaks = (168-113)/0.5),
         lat_bin = cut(latitude, breaks = (168-113)/0.5)
         )

# subsample binned sites
set.seed(200)
binned_subset = eco_env_binned %>% 
  group_by(lon_bin, lat_bin) %>% 
  sample_n(1) %>% 
  ungroup()

# spatial distribution of the subsample
plot(binned_subset$longitude, binned_subset$latitude)

###### know your data

# distribution
summary(binned_subset)
plot(density(binned_subset$number_total))
### skewed variables: number_total, bodysize_cwm_total, bodysize_cwv_total, total_biomass,
### sst_raw_mean, sst_raw_var, sst_bounded_seasonality, sst_colwell_p

# outliers and other ugly things
## some sites have sst values <= 0, this might be due to the fill values in the cds dataset
binned_subset_2 = binned_subset # save copy
nm1 = grep('sst', names(binned_subset_2)) # choose columns
binned_subset_2[nm1] = lapply(binned_subset_2[nm1], function(x) 
  replace(x, binned_subset_2$sst_raw_mean <= 0, NA)) # replace values <= 0 with NAs in sst data

## sanity check
summary(binned_subset_2) # good
plot(density(na.omit(binned_subset_2$sst_raw_mean)))
nrow(binned_subset_2[complete.cases(binned_subset_2$sst_raw_mean),]) # 133 sites are left

## plot to visually identify outliers
pairs(~ sst_raw_mean +
        sst_raw_var +
        sst_bounded_seasonality +
        sst_env_col +
        bodysize_cwm_total +
        bodysize_cwv_total +
        sp_richness +
        shannon,
      data = binned_subset_2)

## bodysize_cwv_total needs a little treatment
Q1 <- quantile(binned_subset_2$bodysize_cwv_total, .25, na.rm = TRUE)
Q3 <- quantile(binned_subset_2$bodysize_cwv_total, .75, na.rm = TRUE)
IQR <- IQR(binned_subset_2$bodysize_cwv_total, na.rm = TRUE)
## replace outliers in chla_env_col
binned_subset_2$bodysize_cwv_total[binned_subset_2$bodysize_cwv_total > (Q3 + 2*IQR)] = NA

## biomass needs a little treatment
Q1 <- quantile(binned_subset_2$total_biomass, .25, na.rm = TRUE)
Q3 <- quantile(binned_subset_2$total_biomass, .75, na.rm = TRUE)
IQR <- IQR(binned_subset_2$total_biomass, na.rm = TRUE)
## replace outliers in chla_env_col
binned_subset_2$total_biomass[binned_subset_2$total_biomass > (Q3 + 2*IQR)] = NA

## plot again
pairs(~ sst_raw_mean +
        sst_raw_var +
        sst_bounded_seasonality +
        sst_env_col +
        bodysize_cwm_total +
        bodysize_cwv_total +
        sp_richness +
        total_biomass,
      data = binned_subset_2) # yepp

# spatial distribution of the subsample
plot(binned_subset_2$longitude, binned_subset$latitude)

## let's have a closer look
### sst stuff
sst1 = ggplot(binned_subset_2, aes(x = "", y = sst_raw_mean)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.y=mean, geom="point", shape=13, size=12) +
  geom_jitter(shape=16, position=position_jitter(0.1))

sst2 = ggplot(binned_subset_2, aes(x = "", y = sst_bounded_seasonality)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.y=mean, geom="point", shape=13, size=12) +
  geom_jitter(shape=16, position=position_jitter(0.1))

sst3 = ggplot(binned_subset_2, aes(x = "", y = sst_env_col)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.y=mean, geom="point", shape=13, size=12) +
  geom_jitter(shape=16, position=position_jitter(0.1))

sst4 = ggplot(binned_subset_2, aes(x = "", y = sst_colwell_p)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.y=mean, geom="point", shape=13, size=12) +
  geom_jitter(shape=16, position=position_jitter(0.1))

sst_plot = ggarrange(sst1, sst2, sst3, sst4, 
                     ncol = 2, nrow = 2)
annotate_figure(sst_plot,
                top = text_grob("SST",
                                color = "black", face = "bold", size = 16)
)

### community stuff
com1 = ggplot(binned_subset_2, aes(x = "", y = bodysize_cwm_total)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.y=mean, geom="point", shape=13, size=12) +
  geom_jitter(shape=16, position=position_jitter(0.1))

com2 = ggplot(binned_subset_2, aes(x = "", y = total_biomass)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.y=mean, geom="point", shape=13, size=12) +
  geom_jitter(shape=16, position=position_jitter(0.1))

com3 = ggplot(binned_subset_2, aes(x = "", y = sp_richness)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.y=mean, geom="point", shape=13, size=12) +
  geom_jitter(shape=16, position=position_jitter(0.1))

com4 = ggplot(binned_subset_2, aes(x = "", y = shannon)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.y=mean, geom="point", shape=13, size=12) +
  geom_jitter(shape=16, position=position_jitter(0.1))

bio_plot = ggarrange(com1, com2, com3, com4,
                     ncol = 2, nrow = 2)
annotate_figure(bio_plot,
                top = text_grob("CWM bodysize, biomass, species richness, shannon",
                                color = "black", face = "bold", size = 16)
)


##### models

# bodysize
summary(lm(binned_subset_2$bodysize_cwm_total ~ binned_subset_2$sst_raw_mean))
summary(lm(binned_subset_2$bodysize_cwm_total ~ binned_subset_2$sst_raw_var))
summary(lm(binned_subset_2$bodysize_cwm_total ~ binned_subset_2$sst_env_col))
summary(lm(binned_subset_2$bodysize_cwm_total ~ binned_subset_2$sst_bounded_seasonality))
summary(lm(binned_subset_2$bodysize_cwm_total ~ binned_subset_2$sst_colwell_p))
summary(lm(binned_subset_2$bodysize_cwm_total ~ binned_subset_2$sst_raw_mean*binned_subset_2$sst_env_col))
summary(lm(binned_subset_2$bodysize_cwm_total ~ binned_subset_2$sst_raw_mean*binned_subset_2$sst_bounded_seasonality))
summary(lm(binned_subset_2$bodysize_cwm_total ~ 
             binned_subset_2$sst_raw_mean*binned_subset_2$sst_env_col*binned_subset_2$sst_bounded_seasonality))

# richness
summary(lm(binned_subset_2$sp_richness ~ binned_subset_2$bodysize_cwm_total))
summary(lm(binned_subset_2$sp_richness ~ binned_subset_2$sst_raw_mean))
summary(lm(binned_subset_2$sp_richness ~ binned_subset_2$sst_env_col))
summary(lm(binned_subset_2$sp_richness ~ binned_subset_2$sst_bounded_seasonality))
summary(lm(binned_subset_2$sp_richness ~ binned_subset_2$sst_colwell_p))
summary(lm(binned_subset_2$sp_richness ~ binned_subset_2$sst_raw_mean*binned_subset_2$sst_env_col))
summary(lm(binned_subset_2$sp_richness ~ binned_subset_2$sst_raw_mean*binned_subset_2$sst_bounded_seasonality))
summary(lm(binned_subset_2$sp_richness ~ 
             binned_subset_2$sst_raw_mean*binned_subset_2$sst_env_col*binned_subset_2$sst_bounded_seasonality))

# shannon
summary(lm(binned_subset_2$total_biomass ~ binned_subset_2$sst_raw_mean))
summary(lm(binned_subset_2$total_biomass ~ binned_subset_2$sst_raw_var))
summary(lm(binned_subset_2$total_biomass ~ binned_subset_2$sst_env_col))
summary(lm(binned_subset_2$total_biomass ~ binned_subset_2$sst_bounded_seasonality))
summary(lm(binned_subset_2$total_biomass ~ binned_subset_2$sst_colwell_p))
summary(lm(binned_subset_2$total_biomass ~ binned_subset_2$sst_raw_mean*binned_subset_2$sst_env_col))
summary(lm(binned_subset_2$total_biomass ~ binned_subset_2$sst_raw_mean*binned_subset_2$sst_bounded_seasonality))
summary(lm(binned_subset_2$total_biomass ~ 
             binned_subset_2$sst_raw_mean*binned_subset_2$sst_env_col*binned_subset_2$sst_bounded_seasonality))
