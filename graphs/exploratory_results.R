###### EXPLORATORY RESULTS
###### CWM AND CWV MAPS AND VIOLIN PLOTS (AND 3D INTERACTIONS, DEPRICATED)

# libraries
library(sf) # '1.0.9'
library(ozmaps) # ‘0.4.5’
library(ggsn) # ‘0.5.0’
library(tidyverse)
library(grid)
library(gridExtra)
library(ggpubr)

# clean memory
rm(list=ls())

# set working directory
setwd("~/Documents/MSc_thesis/Figures")

# import datasets
final_dataset = readr::read_delim("/media/mari/Crucial X8/sst_trait_data.csv", delim = ",")
rls_area_raw = readr::read_delim("/media/mari/Crucial X8/rls_2019_2022_avg.csv", delim = ",")
coords_30 = readr::read_delim("/media/mari/Crucial X8/coords_30km.csv", delim = ",")

# kelvin into deg celsius
final_dataset["sst_raw_mean"] = final_dataset["sst_raw_mean"] - 273.15


##### taxonomy

# filter sites between 2020 and 2022 (for sites < 2020, the predictability contained missing months)
sites30_coords = final_dataset %>% 
  dplyr::select(new_survey_id, latitude, longitude, survey_date, area)

# join both datasets
rls_30 = dplyr::left_join(sites30_coords, rls_area_raw, by = c("latitude", "longitude", "survey_date"))

# explore
length(unique(rls_30[["family"]])) # 74
length(unique(rls_30[["valid_name"]])) # 834
rls_30 %>% select(valid_name) %>% filter(!is.na(valid_name)) %>% n_distinct() # 833 on species level
on_fam_lev = rls_30 %>% select(family, valid_name) %>% filter(is.na(valid_name)) # 52 individuals on fam level...
length(unique(on_fam_lev[["family"]])) # ... in 18 families


###### maps

# get states of Australia
sf_oz <- ozmap_data("states", xlim=c(110,170), ylim=c(-45,-8), mar=c(0,0,0,0))


###### maps for sst

mean_map = 
  ggplot(data = sf_oz) + 
  geom_sf(colour = "gray25", linewidth = 0.4, fill = "gray80") + 
  geom_point(data = final_dataset, 
             aes(x = longitude, y = latitude, fill = sst_raw_mean), 
             pch=21,
             size = 2) +
  xlim(c(112,168.5)) +
  ylim(c(-44.5,-8.4)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_classic() +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "gray30"),
        legend.box.margin = margin(1, 1, 6, 1),
        legend.spacing.y = unit(0.5, 'cm'),
        legend.text = element_text(size = 10, face= "bold"),
        legend.title = element_text(size = 10, face= "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10, face= "bold"),
        axis.text.y = element_text(size = 10, face= "bold"),
        axis.ticks.length=unit(.25, "cm"),
        axis.line = element_line(colour = "gray30", linewidth = 0.6)) +
  scale_fill_viridis_c(name = "SST \nmean \n(°C)",
                       option = "magma",
                       breaks = c(14.0, 
                                  round(mean(final_dataset$sst_raw_mean), digits = 1), 
                                  round(max(final_dataset$sst_raw_mean), digits = 1)),
                       labels = c("14.0", 
                                  round(mean(final_dataset$sst_raw_mean), digits = 1), 
                                  round(max(final_dataset$sst_raw_mean), digits = 1)))

var_map = 
  ggplot(data = sf_oz) + 
  geom_sf(colour = "gray25", linewidth = 0.4, fill = "gray80") + 
  geom_point(data = final_dataset, 
             aes(x = longitude, y = latitude, fill = sst_raw_var), 
             pch = 21,
             size = 2) +
  xlim(c(112,168.5)) +
  ylim(c(-44.5,-8.4)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_classic() +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_blank(),
        legend.box.margin = margin(1, 1, 6, 1),
        legend.spacing.y = unit(0.5, 'cm'),
        legend.box.background = element_rect(colour = "gray30"),
        legend.text = element_text(size = 10, face= "bold"),
        legend.title = element_text(size = 10, face= "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10, face= "bold"),
        axis.text.y = element_text(size = 10, face= "bold"),
        axis.ticks.length=unit(.25, "cm"),
        axis.line = element_line(colour = "gray30", linewidth = 0.6)) +
  scale_fill_viridis_c(name = "SST \nvariance \n(°C²)", option = "magma",
                       breaks = c(round(min(final_dataset$sst_raw_var), digits = 3), 
                                  round(mean(final_dataset$sst_raw_var), digits = 3), 
                                  round(max(final_dataset$sst_raw_var), digits = 5)),
                       labels = c(round(min(final_dataset$sst_raw_var), digits = 1), 
                                  round(mean(final_dataset$sst_raw_var), digits = 1), 
                                  round(max(final_dataset$sst_raw_var), digits = 1)))

col_map = 
  ggplot(data = sf_oz) + 
  geom_sf(colour = "gray25", linewidth = 0.4, fill = "gray80") +
  geom_point(data = final_dataset, 
             aes(x = longitude, y = latitude, fill = sst_env_col),
             pch = 21,
             size = 2) +
  xlim(c(112,168.5)) +
  ylim(c(-44.5,-8.4)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_classic() +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "gray30"),
        legend.box.margin = margin(1, 1, 6, 1),
        legend.spacing.y = unit(0.5, 'cm'),
        legend.text = element_text(size = 10, face= "bold"),
        legend.title = element_text(size = 10, face= "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10, face= "bold"),
        axis.text.y = element_text(size = 10, face= "bold"),
        axis.ticks.length=unit(.25, "cm"),
        axis.line = element_line(colour = "gray30", linewidth = 0.6)) +
  scale_fill_viridis_c(name = "SST \ncolour", option = "magma",
                       breaks = c(round(min(final_dataset$sst_env_col), digits = 2), 
                                  round(mean(final_dataset$sst_env_col), digits = 2), 
                                  round(max(final_dataset$sst_env_col), digits = 2)),
                       labels = c(round(min(final_dataset$sst_env_col), digits = 2), 
                                  round(mean(final_dataset$sst_env_col), digits = 2), 
                                  round(max(final_dataset$sst_env_col), digits = 2)))

sea_map = 
  ggplot(data = sf_oz) + 
  geom_sf(colour = "gray25", linewidth = 0.4, fill = "gray80") + 
  geom_point(data = final_dataset, 
             aes(x = longitude, y = latitude, fill = sst_bounded_seasonality), 
             pch = 21,
             size = 2.5) +
  xlim(c(112,168.5)) +
  ylim(c(-44.5,-8.4)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_classic() +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "gray30"),
        legend.box.margin = margin(1, 1, 6, 1),
        legend.spacing.y = unit(0.5, 'cm'),
        legend.text = element_text(size = 10, face= "bold"),
        legend.title = element_text(size = 10, face= "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10, face= "bold"),
        axis.text.y = element_text(size = 10, face= "bold"),
        axis.ticks.length=unit(.25, "cm"),
        axis.line = element_line(colour = "gray30", linewidth = 0.6)) +
  scale_fill_viridis_c(name = "SST \nseaso-\nnality",
                       option = "magma",
                       breaks = c(round(min(final_dataset$sst_bounded_seasonality), digits = 5), 
                                  round(mean(final_dataset$sst_bounded_seasonality), digits = 3), 
                                  round(max(final_dataset$sst_bounded_seasonality), digits = 3)),
                       labels = c(round(min(final_dataset$sst_bounded_seasonality), digits = 2), 
                                  round(mean(final_dataset$sst_bounded_seasonality), digits = 2), 
                                  "0.97")
  )

sst_maps = ggarrange(mean_map, var_map, col_map, sea_map, ncol = 2, nrow = 2, labels = c("A", "B", "C", "D"))
ggpubr::annotate_figure(sst_maps,
                        fig.lab.size = 12,
                        fig.lab.face = "bold",
                        left = textGrob("Latitude", rot = 90, gp = gpar(cex = 1.2, fontface="bold")),
                        bottom = textGrob("Longitude", vjust = 0.1, gp = gpar(cex = 1.2, fontface="bold")))


###### maps for community traits

# cwm 
cwm_map = 
  ggplot(data = sf_oz) + 
  geom_sf(colour = "gray20", linewidth = 0.5, fill = "gray80") +
  geom_point(data = final_dataset, 
             aes(x = longitude, y = latitude, fill = size_class_cwm), 
             pch=21,
             size = 2.5) +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  xlim(c(112,168.5)) +
  ylim(c(-44.5,-8.4)) +
  theme(panel.grid = element_line(colour = "white", linetype = "dashed"),
        panel.border = element_rect(colour = "gray30", linewidth = 1),
        legend.position = c(.84,.88),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "gray30"),
        legend.text = element_text(size = 14, face= "bold"),
        legend.title = element_text(size = 14, face= "bold"),
        legend.box.margin = margin(4, 18, 4, 4),
        legend.spacing.x = unit(0.5, 'cm'),
        axis.title.x = element_text(size = 16, face= "bold"),
        axis.title.y = element_text(size = 16, face= "bold"),
        axis.text.x = element_text(size = 14, face= "bold"),
        axis.text.y = element_text(size = 14, face= "bold"),
        axis.ticks.length=unit(.25, "cm")) +
  scale_fill_viridis_c(direction = -1,
                       name = "CWM \nsize \n(cm)",
                       breaks = c(round(min(final_dataset$size_class_cwm), digits = 5), 
                                  round(mean(final_dataset$size_class_cwm), digits = 5), 
                                  round(max(final_dataset$size_class_cwm), digits = 5)),
                       labels = c(round(min(final_dataset$size_class_cwm), digits = 1), 
                                  round(mean(final_dataset$size_class_cwm), digits = 1), 
                                  round(max(final_dataset$size_class_cwm), digits = 1)))

# cwv
cwv_map =
ggplot(data = sf_oz) + 
  geom_sf(colour = "gray20", linewidth = 0.5, fill = "gray80") +
  geom_point(data = final_dataset, 
             aes(x = longitude, y = latitude, fill = size_class_cwv),
             pch=21,
             size = 2.5) +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  xlim(c(112,168.5)) +
  ylim(c(-44.5,-8.4)) +
  theme(panel.grid = element_line(colour = "white", linetype = "dashed"),
        panel.border = element_rect(colour = "gray30", linewidth = 1),
        legend.position = c(.84,.88),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "gray30"),
        legend.text = element_text(size = 14, face= "bold"),
        legend.title = element_text(size = 14, face= "bold"),
        legend.box.margin = margin(4, 18, 4, 4),
        legend.spacing.x = unit(0.5, 'cm'),
        axis.title.x = element_text(size = 16, face= "bold"),
        axis.title.y = element_text(size = 16, face= "bold"),
        axis.text.x = element_text(size = 14, face= "bold"),
        axis.text.y = element_text(size = 14, face= "bold"),
        axis.ticks.length=unit(.25, "cm")) +
  scale_fill_viridis_c(direction = -1,
                       name = "CWV \nsize \n(cm²)",
                       breaks = c(round(min(final_dataset$size_class_cwv), digits = 5), 
                                  round(mean(final_dataset$size_class_cwv), digits = 5), 
                                  round(max(final_dataset$size_class_cwv), digits = 5)),
                       labels = c(round(min(final_dataset$size_class_cwv), digits = 1), 
                                  round(mean(final_dataset$size_class_cwv), digits = 1), 
                                  round(max(final_dataset$size_class_cwv), digits = 1)))


###### data distribution

###### violin plots of SST variables

final_long = final_dataset %>%
  select(latitude, longitude, survey_date, sst_raw_mean, sst_raw_var, sst_bounded_seasonality, sst_env_col) %>%
  pivot_longer(cols = c("sst_raw_mean", "sst_raw_var", "sst_bounded_seasonality", "sst_env_col"),
               names_to = "sst_variable",
               values_to = "value")

mean_data = final_long %>% filter(sst_variable == "sst_raw_mean")
mean_plot = ggplot(data = mean_data, mapping = aes(x = sst_variable["sst_raw_mean"], y = value)) + 
  theme_bw() +
  geom_violin(trim = TRUE,  fill='#A4A4A4', color="black", lwd = 0.6) + 
  geom_boxplot(width=0.2, colour = "black", lwd = 0.6) +
  ggtitle("SST mean (°C)") +
  theme(plot.title = element_text(size = 16, face= "bold"),
        panel.grid = element_line(colour = "white", linetype = "dashed"),
        panel.border = element_rect(colour = "gray30", linewidth = 0.6),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 12, face= "bold"),
        axis.ticks.length=unit(.2, "cm")) +
  scale_y_continuous(limits = c(min(mean_data$value), max(mean_data$value)))

vari_data = final_long %>% filter(sst_variable == "sst_raw_var")
var_plot = ggplot(data = vari_data, mapping = aes(x = sst_variable["sst_raw_var"], y = value)) +
  theme_bw() +
  geom_violin(trim = TRUE,  fill='#A4A4A4', color="black", lwd = 0.6) + 
  geom_boxplot(width=0.2, colour = "black", lwd = 0.6) +
  ggtitle("SST variance (°C²)") +
  theme(plot.title = element_text(size = 16, face= "bold"),
        panel.grid = element_line(colour = "white", linetype = "dashed"),
        panel.border = element_rect(colour = "gray30", linewidth = 0.6),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 12, face= "bold"),
        axis.ticks.length=unit(.2, "cm")) +
  scale_y_continuous(limits = c(min(vari_data$value), max(vari_data$value)))

col_data = final_long %>% filter(sst_variable == "sst_env_col")
col_plot = ggplot(data = col_data, mapping = aes(x = sst_variable["sst_env_col"], y = value)) + 
  theme_bw() +
  geom_violin(trim = TRUE,  fill='#A4A4A0', color="black", lwd = 0.6) + 
  geom_boxplot(width=0.2, colour = "black", lwd = 0.6) +
  ggtitle("SST colour") +
  theme(plot.title = element_text(size = 16, face= "bold"),
        panel.grid = element_line(colour = "white", linetype = "dashed"),
        panel.border = element_rect(colour = "gray30", linewidth = 0.6),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 12, face= "bold"),
        axis.ticks.length=unit(.2, "cm")) +
  scale_y_continuous(limits = c(min(col_data$value), max(col_data$value)))

sea_data = final_long %>% filter(sst_variable == "sst_bounded_seasonality")
sea_plot = ggplot(data = sea_data, mapping = aes(x = sst_variable["sst_bounded_seasonality"], y = value)) + 
  theme_bw() +
  geom_violin(trim = TRUE,  fill='#A4A4A4', color="black", lwd = 0.6) + 
  geom_boxplot(width=0.2, colour = "black", lwd = 0.6) +
  ggtitle("SST seasonality") +
  theme(plot.title = element_text(size = 16, face= "bold"),
        panel.grid = element_line(colour = "white", linetype = "dashed"),
        panel.border = element_rect(colour = "gray30", linewidth = 0.6),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 12, face= "bold"),
        axis.ticks.length=unit(.2, "cm")) +
  scale_y_continuous(limits = c(min(sea_data$value), max(sea_data$value)))

ggarrange(mean_plot, var_plot, col_plot, sea_plot, 
          ncol = 4, nrow = 1, 
          labels = c("A", "B", "C", "D"))


###### violin plots of CWM, CWV, and species richness
bio_long = final_dataset %>%
  select(latitude, longitude, survey_date, size_class_cwm, size_class_cwv, sp_richness) %>%
  pivot_longer(cols = c("size_class_cwm", "size_class_cwv", "sp_richness"),
               names_to = "bio_variable",
               values_to = "value")

cwm_data = bio_long %>% filter(bio_variable == "size_class_cwm")
cwm_plot = 
  ggplot(data = cwm_data, mapping = aes(x = bio_variable, y = value)) +
  theme_bw() +
  geom_violin(trim = TRUE,  fill='#A4A4A4', color="black", lwd = 0.6) + 
  geom_boxplot(width = 0.2, colour = "black", lwd = 0.6) +
  ggtitle("") +
  labs(x = "CWM size (cm)") +
  theme(plot.title = element_text(size = 16, face= "bold"),
        panel.grid = element_line(colour = "white", linetype = "dashed"),
        panel.border = element_rect(colour = "gray30", linewidth = 1),
        axis.title.x = element_text(size = 16, face= "bold"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 12, face= "bold"),
        axis.ticks.length=unit(.2, "cm")) +
  scale_y_continuous(limits = c(0, 30))

cwv_data = bio_long %>% filter(bio_variable == "size_class_cwv", value < 800)
cwv_plot = 
  ggplot(data = cwv_data, mapping = aes(x = bio_variable, y = value)) + 
  theme_bw() +
  geom_violin(trim = TRUE,  fill='#A4A4A4', color="black", lwd = 0.6) + 
  geom_boxplot(width = 0.2, colour = "black", lwd = 0.6) +
  ggtitle("") +
  labs(x = "CWV size (cm²)") +
  theme(plot.title = element_text(size = 16, face= "bold"),
        panel.grid = element_line(colour = "white", linetype = "dashed"),
        panel.border = element_rect(colour = "gray30", linewidth = 1),
        axis.title.x = element_text(size = 16, face= "bold"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 12, face= "bold"),
        axis.ticks.length=unit(.2, "cm")) +
  scale_y_continuous(limits = c(0, 180))

ggarrange(cwm_plot, cwv_plot, ncol = 2, nrow = 1, labels = c("A", "B"))


###### violin plots of CWM, CWV, and species richness
bio_long = final_dataset %>%
  select(latitude, longitude, survey_date, size_class_cwm, size_class_cwv, sp_richness) %>%
  pivot_longer(cols = c("size_class_cwm", "size_class_cwv", "sp_richness"),
               names_to = "bio_variable",
               values_to = "value")

cwm_data = bio_long %>% filter(bio_variable == "size_class_cwm")
cwm_plot = ggplot(data = cwm_data, mapping = aes(x = bio_variable, y = value)) +
  theme_bw() +
  geom_violin(trim = TRUE,  fill='#A4A4A4', color="black", lwd = 0.6) + 
  geom_boxplot(width=0.2, colour = "black", lwd = 0.6) +
  labs(x = "CWM of size (cm)") +
  theme(panel.grid = element_line(colour = "white", linetype = "dashed"),
        panel.border = element_rect(colour = "gray30", linewidth = 0.6),
        axis.title.x = element_text(size = 14, face= "bold"),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12, face= "bold"),
        axis.ticks.length=unit(.2, "cm")) +
  scale_y_continuous(limits = c(0, 30))

cwv_data = bio_long %>% filter(bio_variable == "size_class_cwv", value < 800)
cwv_plot = ggplot(data = cwv_data, mapping = aes(x = bio_variable, y = value)) + 
  theme_bw() +
  geom_violin(trim = TRUE,  fill='#A4A4A4', color="black", lwd = 0.6) + 
  geom_boxplot(width=0.2, colour = "black", lwd = 0.6) +
  labs(x = "CWV of size (cm²)") +
  theme(panel.grid = element_line(colour = "white", linetype = "dashed"),
        panel.border = element_rect(colour = "gray30", linewidth = 0.6),
        axis.title.x = element_text(size = 14, face= "bold"),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12, face= "bold"),
        axis.ticks.length=unit(.2, "cm")) +
  scale_y_continuous(limits = c(0, 180))

ggarrange(cwm_plot, cwv_plot, ncol = 2, nrow = 1, labels = c("A", "B"))


###### combine map and violin in one for size class CWM and CWV
ggarrange(cwm_map, cwm_plot, ncol = 2, nrow = 1, labels = c("A", "B"),
          widths = c(1.125, 0.5), heights = c(1, -4))

ggarrange(cwv_map, cwv_plot, ncol = 2, nrow = 1, labels = c("A", "B"),
          widths = c(1.125, 0.5), heights = c(1, -4))

###### same, change order
ggarrange(cwm_plot, cwm_map, ncol = 2, nrow = 1, labels = c("A", "B"),
          widths = c(0.5, 1.125), heights = c(-4, 1))

ggarrange(cwv_plot, cwv_map, ncol = 2, nrow = 1, labels = c("A", "B"),
          widths = c(0.5, 1.125), heights = c(-4, 1))


###### density plots + boxplot
# ggplot(data = final_dataset, mapping = aes(x = sst_raw_mean)) + 
#   geom_density(fill="gray20", color="#e9ecef", alpha=0.8) + 
#   theme_classic() +
#   labs(x = "SST mean (°C)",
#        y = "Density") +
#   theme(plot.title = element_text(size = 16, face= "bold"),
#         axis.title.x = element_text(size = 14, face= "bold"),
#         axis.title.y = element_text(size = 14, face= "bold"),
#         axis.text.y = element_text(size = 12, face= "bold"),
#         axis.ticks.length=unit(.2, "cm")) 
# 
# ggplot(data = final_dataset) +
#   geom_flat_violin(mapping = aes(x = sst_raw_mean),
#                    position = position_nudge(x = .2),
#                    alpha = 0.7) +
#   geom_point(aes(color = verified_income),
#              position = position_jitter(w = .15)) +
#   geom_boxplot(width = .25) + # New code added here
#   coord_flip()
# 
# ggplot(final_long, aes(x = value, y = -0.5)) +
#   geom_boxplot(aes(fill = sst_variable)) +
#   geom_density(aes(x = value, colour = sst_variable), linewidth = 1, inherit.aes = FALSE) +
#   stat_boxplot(varwidth = FALSE, aes(fill = sst_variable)) +
#   stat_boxplot(varwidth = FALSE, aes(fill = sst_variable)) +
#   stat_boxplot(varwidth = FALSE, aes(fill = sst_variable)) +
#   facet_wrap(sst_variable ~ ., scales = "free") +
#   scale_fill_discrete()


# ####### 3D plots for interactions
# 
# # create bins
# hist(final_dataset$sst_env_col)
# binned_data = final_dataset %>% 
#   select(sst_raw_mean, sst_raw_var, sst_bounded_seasonality, sst_env_col, size_class_cwm, size_class_cwv) %>% 
#   dplyr::mutate(var_bin = cut(sst_raw_var, breaks = c(1.5,2.6,4.6,15.3)),
#                 sea_bin = cut(sst_bounded_seasonality, breaks = c(0.7,0.85,0.92,0.97)),
#                 col_bin = cut(sst_env_col, breaks = c(1.5,1.75,1.9,2.2)))
# 
# descrete_data = final_dataset %>% 
#   select(sst_raw_mean, sst_raw_var, sst_bounded_seasonality, sst_env_col, size_class_cwm, size_class_cwv) %>% 
#   dplyr::mutate(var_min_sea = 0.162*min(sst_bounded_seasonality) + 0.068*sst_raw_var - 0.948*min(sst_bounded_seasonality)*sst_raw_var,
#                 var_med_sea = 0.162*median(sst_bounded_seasonality) + 0.068*sst_raw_var - 0.948*median(sst_bounded_seasonality)*sst_raw_var,
#                 var_max_sea = 0.162*max(sst_bounded_seasonality) + 0.068*sst_raw_var - 0.948*max(sst_bounded_seasonality)*sst_raw_var,
#                 sea_min_var = 0.162*sst_bounded_seasonality + 0.068*min(sst_raw_var) - 0.948*sst_bounded_seasonality*min(sst_raw_var),
#                 sea_med_var = 0.162*sst_bounded_seasonality + 0.068*median(sst_raw_var) - 0.948*sst_bounded_seasonality*median(sst_raw_var),
#                 sea_max_var = 0.162*sst_bounded_seasonality + 0.068*max(sst_raw_var) - 0.948*sst_bounded_seasonality*max(sst_raw_var),
#                 var_min_col = 0.003*min(sst_env_col) + 0.068*sst_raw_var + 0.611*min(sst_env_col)*sst_raw_var,
#                 var_med_col = 0.003*median(sst_env_col) + 0.068*sst_raw_var + 0.611*median(sst_env_col)*sst_raw_var,
#                 var_max_col = 0.003*max(sst_env_col) + 0.068*sst_raw_var + 0.611*max(sst_env_col)*sst_raw_var,
#                 col_min_var = 0.003*sst_env_col + 0.068*min(sst_raw_var) + 0.611*sst_env_col*min(sst_raw_var),
#                 col_med_var = 0.003*sst_env_col + 0.068*median(sst_raw_var) + 0.611*sst_env_col*median(sst_raw_var),
#                 col_max_var = 0.003*sst_env_col + 0.068*max(sst_raw_var) + 0.611*sst_env_col*max(sst_raw_var)
#   )
# 
# # variance vs seasonality
# vari_1 = ggplot(data = binned_data, 
#                 aes(x = sst_raw_var, y = size_class_cwm)) +
#   geom_point() +
#   geom_smooth(aes(colour = sea_bin),
#               method="lm",
#               se = TRUE,
#               alpha = 0.2,
#               linewidth = 2,
#               lineend = "round") +
#   xlab("SST variance (°C²)") +
#   ylab("CWM size class (cm)") +
#   theme_classic() +
#   theme(legend.position = "top",
#         legend.background = element_blank(),
#         legend.direction = "horizontal",
#         legend.box.background = element_rect(colour = "black"),
#         legend.text = element_text(size = 12, face= "bold"),
#         legend.title = element_text(size = 12, face= "bold"),
#         axis.title.x = element_text(size = 14, face= "bold"),
#         axis.title.y = element_text(size = 14, face= "bold"),
#         axis.text.x = element_text(size = 12, face= "bold"),
#         axis.text.y = element_text(size = 12, face= "bold"),
#         axis.ticks.length=unit(.2, "cm"),
#         axis.line = element_line(linewidth = 0.8)) +
#   scale_colour_manual(name = "SST seasonality",
#                       labels = c("low", "medium", "high"),
#                       values = c("#A7C737", "#7DB5D6", "#D85F78")) 
# 
# vari_2 = ggplot(data = binned_data, 
#                 aes(x = sst_bounded_seasonality, y = size_class_cwm)) +
#   geom_point() +
#   geom_smooth(aes(colour = var_bin),
#               method="lm",
#               se = TRUE,
#               alpha = 0.2,
#               linewidth = 2,
#               lineend = "round") +
#   xlab("SST seasonality") +
#   ylab("CWM size class (cm)") +
#   theme_classic() +
#   theme(legend.position = "top",
#         legend.background = element_blank(),
#         legend.direction = "horizontal",
#         legend.box.background = element_rect(colour = "black"),
#         legend.text = element_text(size = 12, face= "bold"),
#         legend.title = element_text(size = 12, face= "bold"),
#         axis.title.x = element_text(size = 14, face= "bold"),
#         axis.title.y = element_text(size = 14, face= "bold"),
#         axis.text.x = element_text(size = 12, face= "bold"),
#         axis.text.y = element_text(size = 12, face= "bold"),
#         axis.ticks.length=unit(.2, "cm"),
#         axis.line = element_line(linewidth = 0.8)) +
#   scale_colour_manual(name = "SST variance",
#                       labels = c("low", "medium", "high"),
#                       values = c("#A7C737", "#7DB5D6", "#D85F78"))
# 
# ggarrange(vari_1, vari_2, ncol = 2, nrow = 1, labels = c("A", "B"), vjust = 2.5)
# 
# # 
# vari_3 = ggplot(data = binned_data, 
#                 aes(x = sst_raw_var, y = size_class_cwm)) +
#   geom_point() +
#   geom_smooth(aes(colour = col_bin),
#               method="lm",
#               se = TRUE,
#               alpha = 0.2,
#               linewidth = 2,
#               lineend = "round") +
#   xlab("SST variance (°C²)") +
#   ylab("CWM size class (cm)") +
#   theme_classic() +
#   theme(legend.position = "top",
#         legend.background = element_blank(),
#         legend.direction = "horizontal",
#         legend.box.background = element_rect(colour = "black"),
#         legend.text = element_text(size = 12, face= "bold"),
#         legend.title = element_text(size = 12, face= "bold"),
#         axis.title.x = element_text(size = 14, face= "bold"),
#         axis.title.y = element_text(size = 14, face= "bold"),ggplot(data = descrete_data, 
#                                                                     aes(x = sst_raw_var, y = size_class_cwm)) +
#           axis.text.x = element_text(size = 12, face= "bold"),
#         axis.text.y = element_text(size = 12, face= "bold"),
#         axis.ticks.length=unit(.2, "cm"),
#         axis.line = element_line(linewidth = 0.8)) +
#   scale_colour_manual(name = "SST colour",
#                       labels = c("low", "medium", "high"),
#                       values = c("#A7C737", "#7DB5D6", "#D85F78")) 
# 
# vari_4 = ggplot(data = binned_data, 
#                 aes(x = sst_env_col, y = size_class_cwm)) +
#   geom_point() +
#   geom_smooth(aes(colour = var_bin),
#               method="lm",
#               se = TRUE,
#               alpha = 0.2,
#               linewidth = 2,
#               lineend = "round") +
#   xlab("SST colour") +
#   ylab("CWM size class (cm)") +
#   theme_classic() +
#   theme(legend.position = "top",
#         legend.background = element_blank(),
#         legend.direction = "horizontal",
#         legend.box.background = element_rect(colour = "black"),
#         legend.text = element_text(size = 12, face= "bold"),
#         legend.title = element_text(size = 12, face= "bold"),
#         axis.title.x = element_text(size = 14, face= "bold"),
#         axis.title.y = element_text(size = 14, face= "bold"),
#         axis.text.x = element_text(size = 12, face= "bold"),
#         axis.text.y = element_text(size = 12, face= "bold"),
#         axis.ticks.length=unit(.2, "cm"),
#         axis.line = element_line(linewidth = 0.8)) +
#   scale_colour_manual(name = "SST variance",
#                       labels = c("low", "medium", "high"),
#                       values = c("#A7C737", "#7DB5D6", "#D85F78"))
# 
# ggarrange(vari_3, vari_4, ncol = 2, nrow = 1, labels = c("A", "B"), vjust = 2.5)
# 
# ggarrange(vari_1, vari_2, vari_3, vari_4, ncol = 2, nrow = 2, labels = c("A", "B", "C", "D"), vjust = 2.5)
# 
# 
# 
# ###### same with descrete values
# 
# vari_1 = ggplot(data = descrete_data, 
#                 aes(x = sst_raw_var, y = size_class_cwm)) +
#   geom_point() +
#   #geom_smooth(aes(x = size_class_cwm, y = var_min_sea))
#   geom_line(aes(x = sst_raw_var, y = var_min_sea), linewidth = 2, lineend = "round", colour = "#A7C737") +
#   geom_line(aes(x = sst_raw_var, y = var_med_sea), linewidth = 2, lineend = "round", colour = "#7DB5D6") +
#   geom_line(aes(x = sst_raw_var, y = var_max_sea), linewidth = 2, lineend = "round", colour = "#D85F78") +
#   xlab("SST variance (°C²)") +
#   ylab("CWM size class (cm)") +
#   theme_classic() +
#   theme(legend.position = "top",
#         legend.background = element_blank(),
#         legend.direction = "horizontal",
#         legend.box.background = element_rect(colour = "black"),
#         legend.text = element_text(size = 12, face= "bold"),
#         legend.title = element_text(size = 12, face= "bold"),
#         axis.title.x = element_text(size = 14, face= "bold"),
#         axis.title.y = element_text(size = 14, face= "bold"),
#         axis.text.x = element_text(size = 12, face= "bold"),
#         axis.text.y = element_text(size = 12, face= "bold"),
#         axis.ticks.length=unit(.2, "cm"),
#         axis.line = element_line(linewidth = 0.8))# +
# scale_colour_manual(name = "SST seasonality",
#                     labels = c("low", "medium", "high"),
#                     values = c("#A7C737", "#7DB5D6", "#D85F78")) 
# 
# vari_2 = ggplot(data = binned_data, 
#                 aes(x = sst_bounded_seasonality, y = size_class_cwm)) +
#   geom_point() +
#   geom_smooth(aes(colour = var_bin),
#               method="lm",
#               se = TRUE,
#               alpha = 0.2,
#               linewidth = 2,
#               lineend = "round") +
#   xlab("SST seasonality") +
#   ylab("CWM size class (cm)") +
#   theme_classic() +
#   theme(legend.position = "top",
#         legend.background = element_blank(),
#         legend.direction = "horizontal",
#         legend.box.background = element_rect(colour = "black"),
#         legend.text = element_text(size = 12, face= "bold"),
#         legend.title = element_text(size = 12, face= "bold"),
#         axis.title.x = element_text(size = 14, face= "bold"),
#         axis.title.y = element_text(size = 14, face= "bold"),
#         axis.text.x = element_text(size = 12, face= "bold"),
#         axis.text.y = element_text(size = 12, face= "bold"),
#         axis.ticks.length=unit(.2, "cm"),
#         axis.line = element_line(linewidth = 0.8)) +
#   scale_colour_manual(name = "SST variance",
#                       labels = c("low", "medium", "high"),
#                       values = c("#A7C737", "#7DB5D6", "#D85F78"))
# 
# ggarrange(vari_1, vari_2, ncol = 2, nrow = 1, labels = c("A", "B"), vjust = 2.5)
# 
# # 
# vari_3 = ggplot(data = binned_data, 
#                 aes(x = sst_raw_var, y = size_class_cwm)) +
#   geom_point() +
#   geom_smooth(aes(colour = col_bin),
#               method="lm",
#               se = TRUE,
#               alpha = 0.2,
#               linewidth = 2,
#               lineend = "round") +
#   xlab("SST variance (°C²)") +
#   ylab("CWM size class (cm)") +
#   theme_classic() +
#   theme(legend.position = "top",
#         legend.background = element_blank(),
#         legend.direction = "horizontal",
#         legend.box.background = element_rect(colour = "black"),
#         legend.text = element_text(size = 12, face= "bold"),
#         legend.title = element_text(size = 12, face= "bold"),
#         axis.title.x = element_text(size = 14, face= "bold"),
#         axis.title.y = element_text(size = 14, face= "bold"),
#         axis.text.x = element_text(size = 12, face= "bold"),
#         axis.text.y = element_text(size = 12, face= "bold"),
#         axis.ticks.length=unit(.2, "cm"),
#         axis.line = element_line(linewidth = 0.8)) +
#   scale_colour_manual(name = "SST colour",
#                       labels = c("low", "medium", "high"),
#                       values = c("#A7C737", "#7DB5D6", "#D85F78")) 
# 
# vari_4 = ggplot(data = binned_data, 
#                 aes(x = sst_env_col, y = size_class_cwm)) +
#   geom_point() +
#   geom_smooth(aes(colour = var_bin),
#               method="lm",
#               se = TRUE,
#               alpha = 0.2,
#               linewidth = 2,
#               lineend = "round") +
#   xlab("SST colour") +
#   ylab("CWM size class (cm)") +
#   theme_classic() +
#   theme(legend.position = "top",
#         legend.background = element_blank(),
#         legend.direction = "horizontal",
#         legend.box.background = element_rect(colour = "black"),
#         legend.text = element_text(size = 12, face= "bold"),
#         legend.title = element_text(size = 12, face= "bold"),
#         axis.title.x = element_text(size = 14, face= "bold"),
#         axis.title.y = element_text(size = 14, face= "bold"),
#         axis.text.x = element_text(size = 12, face= "bold"),
#         axis.text.y = element_text(size = 12, face= "bold"),
#         axis.ticks.length=unit(.2, "cm"),
#         axis.line = element_line(linewidth = 0.8)) +
#   scale_colour_manual(name = "SST variance",
#                       labels = c("low", "medium", "high"),
#                       values = c("#A7C737", "#7DB5D6", "#D85F78"))
# 
# ggarrange(vari_3, vari_4, ncol = 2, nrow = 1, labels = c("A", "B"), vjust = 2.5)
# 
# ggarrange(vari_1, vari_2, vari_3, vari_4, ncol = 2, nrow = 2, labels = c("A", "B", "C", "D"), vjust = 2.5)
# 
# # #####  with continous axis
# # # variance vs seasonality
# vari_1 = ggplot(data = binned_data,
#                 aes(x = sst_raw_var, y = size_class_cwm)) +
#   geom_point(aes(colour = sst_bounded_seasonality), size = 2) +
#   geom_smooth(aes(linetype = sea_bin),
#               colour = "black",
#               method="lm",
#               se = FALSE,
#               alpha = 0.2,
#               linewidth = 1,
#               lineend = "round") +
#   xlab("SST variance (°C²)") +
#   ylab("CWM size class (cm)") +
#   theme_classic() +
#   theme(legend.position = "right",
#         legend.background = element_blank(),
#         legend.direction = "vertical",
#         legend.spacing.x = unit(0.5, 'cm'),
#         legend.text = element_text(size = 12, face= "bold"),
#         legend.title = element_text(size = 12, face= "bold"),
#         axis.title.x = element_text(size = 14, face= "bold"),
#         axis.title.y = element_text(size = 14, face= "bold"),
#         axis.text.x = element_text(size = 12, face= "bold"),
#         axis.text.y = element_text(size = 12, face= "bold"),
#         axis.ticks.length=unit(.2, "cm"),
#         axis.line = element_line(linewidth = 0.8)) +
#   scale_colour_viridis(name = "SST \nseasonality",
#                        option = "magma",
#                        breaks = c(min(final_dataset$sst_bounded_seasonality),
#                                   mean(final_dataset$sst_bounded_seasonality),
#                                   max(final_dataset$sst_bounded_seasonality)),
#                        labels = c(round(min(final_dataset$sst_bounded_seasonality), digits = 2),
#                                   round(mean(final_dataset$sst_bounded_seasonality), digits = 2),
#                                   round(max(final_dataset$sst_bounded_seasonality), digits = 2))) +
#   scale_linetype_manual(name = "SST \nseasonality \nlevels",
#                         values = c("solid", "dotdash", "dotted"),
#                         labels = c("low", "medium", "high"))
