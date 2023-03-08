# DECOMPOSITION PLOT ENVPRED PACKAGE

# libs
library(tidyverse)
library(ggplot2)
library(ggbreak)
library(grid)
library(gridExtra)
library(ggpubr)

# clean memory
rm(list=ls())

# set working directory
setwd("~/Documents/MSc_thesis/Figures")

# get data
orig_ts = readr::read_delim("/media/mari/Crucial X8/concept_stuff/signal.csv", delim = ",")
detrend_ts = readr::read_delim("/media/mari/Crucial X8/concept_stuff/detrended.csv", delim = ",")
seasonal_trend = readr::read_delim("/media/mari/Crucial X8/concept_stuff/interpolated_season.csv", delim = ",")
noise = readr::read_delim("/media/mari/Crucial X8/concept_stuff/noise.csv", delim = ",")
loglog = readr::read_delim("/media/mari/Crucial X8/concept_stuff/loglog_reg.csv", delim = ",")
whitelog = readr::read_delim("/media/mari/Crucial X8/concept_stuff/white_noise_loglog_reg.csv", delim = ",")

# original ts
long_ts = orig_ts %>% pivot_longer(cols = c("y", "trend"),
                                   names_to = "variable",
                                   values_to = "values")

original = 
ggplot() +
  theme_classic() +
  geom_line(data = long_ts, aes(x = x, y = values, group = variable, colour = variable), 
            linewidth = 1.5, 
            lineend = "round") +
  xlab("Time") +
  ylab("SST") +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.text = element_text(size = 16, face= "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(linewidth = 1)) +
    scale_color_manual(name = "",
                         labels = c("LINEAR TREND", "RAW TIME SERIES"),
                         values = c("#DC267F", "#613406"))

# detrended ts
long_detrend = detrend_ts %>% pivot_longer(cols = c("y", "trend"),
                                   names_to = "variable",
                                   values_to = "values")

detrended = 
ggplot() +
  theme_classic() +
  geom_line(data = long_detrend %>% filter(variable == "y"), aes(x = x, y = values, colour = variable), 
            linewidth = 1.5, 
            lineend = "round") +
  xlab("Time") +
  ylab("SST") +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.text = element_text(size = 16, face= "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(linewidth = 1)) +
  scale_color_manual(name = "",
                     labels = "DETRENDED TIME SERIES",
                     values = "#AAE040")


season = 
ggplot() +
  theme_classic() +
  geom_line(data = seasonal_trend, aes(x = x, y = y, colour = "#648FFF"), 
            linewidth = 1.5, 
            lineend = "round") +
  xlab("Time") +
  ylab("SST") +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.text = element_text(size = 16, face= "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(linewidth = 1)) +
  scale_color_manual(name = "",
                     labels = "SEASONAL TREND",
                     values = "#648FFF")

noise_p = 
ggplot() +
  theme_classic() +
  geom_line(data = noise, aes(x = x, y = y, colour = "#FFD900"), 
            linewidth = 1.5, 
            lineend = "round") +
  xlab("Time") +
  ylab("SST") +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.text = element_text(size = 16, face= "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(linewidth = 1)) +
  scale_color_manual(name = "",
                     labels = "NOISE",
                     values = "#FFD900")


decomps = ggarrange(original, detrended, season, noise_p,
                    ncol = 2, nrow = 2,
                    labels = c("A", "B", "C", "D"),
                    font.label = list(size = 16),
                    vjust = 2.3,
                    hjust = -1.5)
ggpubr::annotate_figure(decomps,
                        fig.lab.size = 18,
                        fig.lab.face = "bold",
                        left = textGrob("SST", rot = 90, gp = gpar(cex = 1.5, fontface="bold")),
                        bottom = textGrob("TIME = 10 YEARS", vjust = 0.1, gp = gpar(cex = 1.5, fontface="bold")))

## log log
# original ts
long_log = loglog %>% pivot_longer(cols = c("y", "regression"),
                                   names_to = "variable",
                                   values_to = "values")


mean_y = mean(whitelog$y)

whitelog_3 = whitelog
whitelog_3$y = (whitelog_3$y-mean_y)*5 + mean_y
long_white = whitelog_3 %>% pivot_longer(cols = c("y", "regression"),
                                   names_to = "variable",
                                   values_to = "values")

ggplot() +
  theme_classic() +
  geom_line(data = long_white, aes(x = x, y = values, group = variable, linetype = variable, colour ="gray"), 
            linewidth = 1.5, 
            lineend = "round") +
  geom_line(data = long_log, aes(x = x, y = values, group = variable, linetype = variable, colour ="red3"), 
            linewidth = 1.5, 
            lineend = "round") +
  xlab("Log frequency") +
  ylab("Log power spectral density") +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(size = 18, face= "bold"),
        legend.text = element_text(size = 18, face= "bold"),
        axis.title.x = element_text(size = 20, face= "bold"),
        axis.title.y = element_text(size = 20, face= "bold"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(linewidth = 1)) +
  scale_linetype_manual(name = "",
                        labels = c("Linear regression", "Power spectrum"),
                        values = c("dotted", "solid")) +
  scale_color_manual(name = "",
                     labels = c("White noise", "Reddend noise"),
                     values = c("gray", "red3")) +
  annotate(geom="text", x = 4.85, y = -1.5, label="𝛽 = 0",
           color="gray", size = 8, fontface = 2) +
  annotate(geom="text", x = 4.85, y = -14, label="0.5 < |𝛽| ≤ 2",
           color="red3", size = 8, fontface = 2)
