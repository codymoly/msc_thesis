# nMDS FOR ASSESSING THE EFFECT OF DEPTH ON THE COMMUNITY COMPOSITION
library(tidyverse)
library(vegan)
library(corrplot)

# clean memory
rm(list=ls())

# set working directory
setwd("~/projects/msc_thesis")

# read raw rls data
rls_raw = read_delim("/media/mari/Crucial X8/rls_2019_2022_clean.csv", delim = ",")

# subset data 
rls_sub = rls_raw %>%
  select(site_code, latitude, longitude, survey_date, depth,
         block, species_name, total
        )

# tranform species list into columns
rls_wide = rls_sub %>%
  pivot_wider(names_from = species_name,
              values_from = total,
              names_sep = "_",
              values_fill = list(total = 0),
              values_fn = list(total = mean)
              )

# list species names
rls_names =
  select(rls_wide, -(site_code:block) ) %>%
  names()
rls_names

# choose random cases since the df is biiiiggg
set.seed(200)
rls_rsub = sample_n(rls_wide, 200)

# select abundance data
rls_species =
  rls_rsub %>%
  select(all_of(rls_names))

# calculate dissimilarities between all rows
vegdist(x = rls_species, method = "bray", diag = TRUE, upper = TRUE)

# perform nMDS on species-only data
nmds_rls <- metaMDS(comm = rls_species,
                    distance = "bray",
                    k = 2,
                    autotransform = FALSE)

# check stress values
nmds_rls$stress

# stress plot
stressplot(nmds_rls)

# extract nMDS scores and create a dataframe
nmds_dat = tibble::as_tibble(nmds_rls$points)

# write site information into a separate dataframe
site_dat = select(rls_rsub, site_code, latitude, longitude, survey_date, depth, block)

# bind both dfs
nmds_dat = dplyr::bind_cols(nmds_dat, site_dat)

# check correlation between nMDS axes, latitude, longitude, and depth
nmds_dat %>%
  select(MDS1, MDS2, latitude, longitude, depth) %>%
  cor(x = ., method = c("pearson")) %>%
  corrplot(method = "number")

# mediation analysis
summary(lm(scale(nmds_dat$MDS1) ~ scale(nmds_dat$latitude)))
## scale(nmds_dat$latitude) 8.849e-01  3.311e-02   26.73   <2e-16 ***
## Multiple R-squared:  0.783,	Adjusted R-squared:  0.7819 
summary(lm(scale(nmds_dat$MDS1) ~ scale(nmds_dat$depth)))
## scale(nmds_dat$depth) 2.166e-01  6.938e-02   3.121  0.00207 **
## Multiple R-squared:  0.04689,	Adjusted R-squared:  0.04208
summary(lm(scale(nmds_dat$MDS1) ~ scale(nmds_dat$latitude) + scale(nmds_dat$depth)))
## scale(nmds_dat$latitude)  8.857e-01  3.426e-02  25.849   <2e-16 ***
## scale(nmds_dat$depth)    -3.374e-03  3.426e-02  -0.098    0.922
## Multiple R-squared:  0.783,	Adjusted R-squared:  0.7808

# plot nMDS
## turn depth and latitude into discrete variables
nmds_dat$depth = as.character(nmds_dat$depth)
nmds_dat$latitude = as.character(nmds_dat$latitude)
## depth
ggplot(data = nmds_dat,
       mapping = aes(x = MDS1, y = MDS2, colour = depth)) +
  geom_point(size = 2.5) +
  scale_colour_viridis_d() +
  theme_classic()
## latitude
ggplot(data = nmds_dat,
       mapping = aes(x = MDS1, y = MDS2, colour = latitude)) +
  geom_point(size = 2.5) +
  scale_colour_viridis_d() +
  theme_classic()

