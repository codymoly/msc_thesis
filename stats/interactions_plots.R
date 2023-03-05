# PLOT INTERACTIONS

# read libs
library(tidyverse)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggpubr)
library(corrplot)

# clean memory
rm(list=ls())

# set working directory
setwd("~/Documents/MSc_thesis/Figures")

# import datasets
norm_data = readr::read_delim("/media/mari/Crucial X8/sst_trait_data_norm.csv", delim = ",")

# logic:

# CWM ~ variance, if seasonality is high and colour is high
# CWM ~ variance, if seasonality is low and colour is low
# CWM ~ variance, if seasonality is high and colour is low
# CWM ~ variance, if seasonality is low and colour is high
# 
# CWM ~ seasonality, if variance is high 
# CWM ~ seasonality, if variance is low

# subset data
norm_subset = norm_data %>% 
  select(sst_raw_var, sst_bounded_seasonality, sst_env_col, size_class_cwm) %>% 
  mutate(
    var_sea1_col1 = 0.162 * 1 - 0.003 * 1 + 0.068 * sst_raw_var - 0.948 * 1 * sst_raw_var  +  0.611 * 1 * sst_raw_var,
    var_sea0_col0 = 0.162 * 0 - 0.003 * 0 + 0.068 * sst_raw_var - 0.948 * 0 * sst_raw_var  +  0.611 * 0 * sst_raw_var,
    var_sea1_col0 = 0.162 * 1 - 0.003 * 0 + 0.068 * sst_raw_var - 0.948 * 1 * sst_raw_var  +  0.611 * 0 * sst_raw_var,
    var_sea0_col1 = 0.162 * 0 - 0.003 * 1 + 0.068 * sst_raw_var - 0.948 * 0 * sst_raw_var  +  0.611 * 1 * sst_raw_var,
    sea_var1_col1 = 0.162 * sst_bounded_seasonality - 0.003 * 1 + 0.068 * 1 - 0.948 * sst_bounded_seasonality * 1  +  0.611 * 1 * 1,
    sea_var0_col0 = 0.162 * sst_bounded_seasonality - 0.003 * 0 + 0.068 * 0 - 0.948 * sst_bounded_seasonality * 0  +  0.611 * 0 * 0,
    sea_var1_col0 = 0.162 * sst_bounded_seasonality - 0.003 * 0 + 0.068 * 1 - 0.948 * sst_bounded_seasonality * 1  +  0.611 * 0 * 1,
    sea_var0_col1 = 0.162 * sst_bounded_seasonality - 0.003 * 1 + 0.068 * 0 - 0.948 * sst_bounded_seasonality * 0  +  0.611 * 1 * 0,
    col_var1_sea1 = 0.162 * 1 - 0.003 * sst_env_col + 0.068 * 1 - 0.948 * 1 * 1  +  0.611 * sst_env_col * 1,
    col_var0_sea0 = 0.162 * 0 - 0.003 * sst_env_col + 0.068 * 0 - 0.948 * 0 * 0 +  0.611 * sst_env_col * 0, 
    col_var1_sea0 = 0.162 * 0 - 0.003 * sst_env_col + 0.068 * 1 - 0.948 * 0 * 1  +  0.611 * sst_env_col * 1,
    col_var0_sea1 = 0.162 * 1 - 0.003 * sst_env_col + 0.068 * 0 - 0.948 * 1 * 0  +  0.611 * sst_env_col * 0
  )

norm_long = norm_subset %>% 
  pivot_longer(
  cols = var_sea1_col1:col_var0_sea1,
  names_to = "variable",
  values_to = "values"
)

ggplot(data = norm_long %>% filter(str_detect(variable, 'var_sea')), aes(x = sst_raw_var, y = values, colour = variable)) +
  geom_smooth(method = "lm")

ggplot(data = norm_long %>% filter(str_detect(variable, 'sea_var')), aes(x = sst_bounded_seasonality, y = values, colour = variable)) +
  geom_smooth(method = "lm")

ggplot(data = norm_long %>% filter(str_detect(variable, 'col_var')), aes(x = sst_env_col, y = values, colour = variable)) +
  geom_smooth(method = "lm")


cwv1 = ggplot(data = norm_data, aes(x = sst_bounded_seasonality, y = size_class_cwv)) +
  theme_gray() +
  geom_point() +
  geom_smooth(aes(colour = size_class_cwv), method = "lm", color="#FFA800", linewidth = 2) +
  xlab("SST seasonality (min-max normalised)") +
  ylab("CWV size class (min-max normalised)") +
  theme(axis.title.x = element_text(size = 14, face= "bold"),
        axis.title.y = element_text(size = 14, face= "bold"),
        axis.text.x = element_text(size = 12, face= "bold"),
        axis.text.y = element_text(size = 12, face= "bold"),
        axis.ticks.length=unit(.2, "cm"),
        axis.line = element_line(linewidth = 0.8)) +
  annotate(geom="text", x=0.85, y=0.9, label="ß = 0.149",
           color="#FFA800", size = 7)

cwv2 = ggplot(data = norm_data, aes(x = sst_raw_var, y = size_class_cwv)) +
  theme_gray() +
  geom_point() +
  geom_smooth(aes(colour = size_class_cwv), method = "lm", color="#37A8E2", linewidth = 2) +
  xlab("SST variance (min-max normalised)") +
  ylab("CWV size class (min-max normalised)") +
  theme(axis.title.x = element_text(size = 14, face= "bold"),
        axis.title.y = element_text(size = 14, face= "bold"),
        axis.text.x = element_text(size = 12, face= "bold"),
        axis.text.y = element_text(size = 12, face= "bold"),
        axis.ticks.length=unit(.2, "cm"),
        axis.line = element_line(linewidth = 0.8)) +
  annotate(geom="text", x=0.85, y=0.9, label="ß = -0.319",
           color="#37A8E2", size = 7)

ggarrange(cwv1, cwv2, ncol = 2, nrow = 1, labels = c("A", "B"), vjust = 1.5)
  


