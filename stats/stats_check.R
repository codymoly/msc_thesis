# FIRST EXAMINATION OF RELATION BETWEEN TRAITS AND ENVIRONMENTAL STUFF

# read libs
library(tidyverse)
library(grid)
library(gridExtra)
library(ggpubr)
library(broom)
library(corrplot)

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
summary(binned_subset_2)
qqp(binned_subset_2$sst_raw_mean)
qqp(binned_subset_2$sst_raw_mean)
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
pairs(~ log(sst_raw_mean) +
        sst_raw_var +
        sst_bounded_seasonality +
        sst_env_col +
        log(bodysize_cwm_total),
      data = binned_subset_2) # yepp

pairs(~ sst_raw_mean +
        sst_raw_var +
        sst_bounded_seasonality +
        sst_env_col +
        bodysize_cwm_total +
        bodysize_cwv_total +
        sp_richness +
        total_biomass,
      data = binned_subset_2)

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

##### predictors
binned_subset_2 %>%
  na.omit() %>% 
  select(latitude, longitude, sst_raw_mean, sst_raw_var, sst_env_col, sst_bounded_seasonality) %>%
  cor(x = ., method = c("spearman")) %>%
  corrplot(method = "number")

##### response
binned_subset_2 %>%
  na.omit() %>% 
  select(bodysize_cwm_total, bodysize_cwv_total, sp_richness, shannon, total_biomass) %>%
  cor(x = ., method = c("spearman")) %>%
  corrplot(method = "number")

##### models
binned_subset_log = binned_subset_2 %>% 
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

# bodysize
cwm_models <- list(
  cwm1 = lm(bodysize_cwm_total ~ sst_raw_mean, data = binned_subset_2),
  cwm2 = lm(bodysize_cwm_total ~ sst_raw_var, data = binned_subset_2),
  cwm3 = lm(bodysize_cwm_total ~ sst_raw_mean * sst_raw_var, data = binned_subset_2),
  cwm4 = lm(bodysize_cwm_total ~ sst_env_col, data = binned_subset_2),
  cwm5 = lm(bodysize_cwm_total ~ sst_bounded_seasonality, data = binned_subset_2),
  cwm6 = lm(bodysize_cwm_total ~ sst_raw_mean * sst_env_col, data = binned_subset_2),
  cwm7 = lm(bodysize_cwm_total ~ sst_raw_mean * sst_bounded_seasonality, data = binned_subset_2),
  cwm8 = lm(bodysize_cwm_total ~ sst_raw_mean * sst_raw_var +
              sst_raw_mean * sst_env_col * sst_bounded_seasonality, data = binned_subset_2),
  cwm9 = lm(bodysize_cwm_total ~ sst_raw_mean * sst_raw_var +
              sst_raw_var * sst_env_col * sst_bounded_seasonality, data = binned_subset_2),
  cwm10 = lm(bodysize_cwm_total ~ sst_raw_mean * sst_raw_var * sst_env_col * sst_bounded_seasonality, data = binned_subset_2),
  cwm11 = lm(bodysize_cwm_total ~ sst_raw_mean * sst_env_col * sst_bounded_seasonality, data = binned_subset_2)
)

qqp(cwm_models$cwm11)

summary(lm(bodysize_cwm_total ~ 
             sst_raw_mean * sst_env_col +
             sst_raw_mean * sst_bounded_seasonality +
             sst_env_col * sst_bounded_seasonality, 
           data = binned_subset_2))

step(cwm_models$cwm10)

##### dredge 
null_lm = lm(bodysize_cwm_total ~ 1, data = binned_subset_2)
null_lm_2 = lm(bodysize_cwm_total ~ sst_raw_mean, data = binned_subset_2)
null_lm_3 = lm(bodysize_cwm_total ~ sst_raw_mean * sst_raw_var, data = binned_subset_2)
dredge_lm = lm(formula = bodysize_cwm_total ~ sst_raw_mean + sst_raw_var + 
                 sst_env_col + sst_bounded_seasonality + sst_raw_var:sst_env_col + 
                 sst_raw_var:sst_bounded_seasonality + sst_env_col:sst_bounded_seasonality + 
                 sst_raw_var:sst_env_col:sst_bounded_seasonality, data = binned_subset_2)

summary(null_lm)
summary(null_lm_2)
summary(null_lm_3)
summary(dredge_lm)

AIC(null_lm)
AIC(null_lm_2)
AIC(null_lm_3)
AIC(dredge_lm)
# aic wird niedriger obwohl mehr Variablen, niedriger AIc ist gut
library(stats)
anova(null_lm_3, dredge_lm)

### scientific model: what effect do env col and seasonality have on body size?

envcol_lm = lm(bodysize_cwm_total ~ sst_env_col, data = binned_subset_2)
summary(envcol_lm) # ß coefficent = sst_env_col: 6.800 , not really interpretable

# standardised version
envcol_lm_2 = lm(scale(bodysize_cwm_total) ~ scale(sst_env_col), data = binned_subset_2)
summary(envcol_lm_2) # no general effect of env col on body size
#--> we have to include mean, ie, high means/ low means differently affect the effect of env col
envcol_lm_3 = lm(scale(bodysize_cwm_total) ~ scale(sst_env_col) + scale(sst_raw_mean), data = binned_subset_2)
summary(envcol_lm_3)

envcol_lm_4 = lm(scale(bodysize_cwm_total) ~ scale(sst_bounded_seasonality) * scale(sst_raw_mean), data = binned_subset_2)
summary(envcol_lm_4) # je höher der mean, desto steiler bodysize über seasonality
# contextabhängige effekt von seasonality: scale(sst_bounded_seasonality):scale(sst_raw_mean)  0.16594

# compare multiple models
model.comparison = rbind(broom::glance(cwm_models$cwm1),
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

# bodysize variance
cwv_models <- list(
  cwv1 = lm(bodysize_cwv_total ~ sst_raw_mean, data = binned_subset_2),
  cwv2 = lm(bodysize_cwv_total ~ sst_raw_var, data = binned_subset_2),
  cwv3 = lm(bodysize_cwv_total ~ sst_raw_mean * sst_raw_var, data = binned_subset_2),
  cwv4 = lm(bodysize_cwv_total ~ sst_env_col, data = binned_subset_2),
  cwv5 = lm(bodysize_cwv_total ~ sst_bounded_seasonality, data = binned_subset_2),
  cwv6 = lm(bodysize_cwv_total ~ sst_raw_var + sst_env_col, data = binned_subset_2),
  cwv7 = lm(bodysize_cwv_total ~ sst_raw_var + sst_bounded_seasonality, data = binned_subset_2),
  cwv8 = lm(bodysize_cwv_total ~ sst_raw_mean * sst_raw_var +
              sst_raw_mean * sst_env_col * sst_bounded_seasonality, data = binned_subset_2),
  cwv9 = lm(bodysize_cwv_total ~ sst_raw_mean * sst_raw_var +
              sst_raw_var * sst_env_col * sst_bounded_seasonality, data = binned_subset_2),
  cwv10 = lm(bodysize_cwv_total ~ sst_raw_mean * sst_raw_var * sst_env_col * sst_bounded_seasonality, data = binned_subset_2)
)

# compare multiple models
model.comparison.2 = rbind(broom::glance(cwv_models$cwv1),
                         broom::glance(cwv_models$cwv2),
                         broom::glance(cwv_models$cwv3),
                         broom::glance(cwv_models$cwv4),
                         broom::glance(cwv_models$cwv5),
                         broom::glance(cwv_models$cwv6),
                         broom::glance(cwv_models$cwv7),
                         broom::glance(cwv_models$cwv8),
                         broom::glance(cwv_models$cwv9),
                         broom::glance(cwv_models$cwv10)
)

# richness
rich_models <- list(
  rich1 = lm(sp_richness ~ sst_raw_mean, data = binned_subset_2),
  rich2 = lm(sp_richness ~ sst_raw_var, data = binned_subset_2),
  rich3 = lm(sp_richness ~ sst_raw_mean * sst_raw_var, data = binned_subset_2),
  rich4 = lm(sp_richness ~ sst_env_col, data = binned_subset_2),
  rich5 = lm(sp_richness ~ sst_bounded_seasonality, data = binned_subset_2),
  rich6 = lm(sp_richness ~ sst_raw_var + sst_env_col, data = binned_subset_2),
  rich7 = lm(sp_richness ~ sst_raw_var + sst_bounded_seasonality, data = binned_subset_2),
  rich8 = lm(sp_richness ~ sst_raw_mean * sst_raw_var +
              sst_raw_mean * sst_env_col * sst_bounded_seasonality, data = binned_subset_2),
  rich9 = lm(sp_richness ~ sst_raw_mean * sst_raw_var +
              sst_raw_var * sst_env_col * sst_bounded_seasonality, data = binned_subset_2),
  rich10 = lm(sp_richness ~ sst_raw_mean * sst_raw_var * sst_env_col * sst_bounded_seasonality, data = binned_subset_2)
)

# compare multiple models
model.comparison.3 = rbind(broom::glance(rich_models$rich1),
                         broom::glance(rich_models$rich2),
                         broom::glance(rich_models$rich3),
                         broom::glance(rich_models$rich4),
                         broom::glance(rich_models$rich5),
                         broom::glance(rich_models$rich6),
                         broom::glance(rich_models$rich7),
                         broom::glance(rich_models$rich8),
                         broom::glance(rich_models$rich9),
                         broom::glance(rich_models$rich10)
)
