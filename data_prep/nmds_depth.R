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

# rls_raw = rls_raw %>% 
#   dplyr::filter(survey_date > "2019-12-31") %>% 
#   dplyr::filter(latitude > -30) %>% 
#   dplyr::filter(longitude > 120)

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
set.seed(600)
rls_rsub = sample_n(rls_wide, 600)

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
rls_sub_binned = rls_rsub %>% 
  mutate(depth_bin = cut(depth, breaks = c(0,10,20,30)),
         lat_bin = cut(latitude, breaks = c(-5,-15,-25,-35,-45))
  )

site_dat = select(rls_sub_binned, site_code, latitude, longitude, survey_date, depth, depth_bin, lat_bin, block)

# bind both dfs
nmds_dat = dplyr::bind_cols(nmds_dat, site_dat)

# check correlation between nMDS axes, latitude, longitude, and depth
nmds_dat %>%
  select(MDS1, MDS2, latitude, longitude, depth) %>%
  cor(x = ., method = c("pearson")) %>%
  corrplot(method = "number")

# mediation analysis
summary(lm(scale(nmds_dat$MDS1) ~ scale(nmds_dat$latitude)))
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.53200 -0.30097  0.04741  0.29363  1.14365 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              1.175e-16  1.811e-02    0.00        1    
# scale(nmds_dat$latitude) 8.965e-01  1.812e-02   49.47   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4435 on 598 degrees of freedom
# Multiple R-squared:  0.8036,	Adjusted R-squared:  0.8033 
# F-statistic:  2447 on 1 and 598 DF,  p-value: < 2.2e-16 
summary(lm(scale(nmds_dat$MDS1) ~ scale(nmds_dat$depth)))
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.3918 -0.9376 -0.1260  1.0483  1.9904 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)           5.091e-17  4.073e-02   0.000   1.0000  
# scale(nmds_dat$depth) 8.080e-02  4.076e-02   1.982   0.0479 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9976 on 598 degrees of freedom
# Multiple R-squared:  0.006528,	Adjusted R-squared:  0.004867 
# F-statistic:  3.93 on 1 and 598 DF,  p-value: 0.0479
summary(lm(scale(nmds_dat$MDS1) ~ scale(nmds_dat$latitude) + scale(nmds_dat$depth)))
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.59569 -0.29749  0.05933  0.28820  1.17645 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              1.258e-16  1.802e-02   0.000     1.00    
# scale(nmds_dat$latitude) 8.947e-01  1.805e-02  49.570   <2e-16 ***
#   scale(nmds_dat$depth)    4.664e-02  1.805e-02   2.584     0.01 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4414 on 597 degrees of freedom
# Multiple R-squared:  0.8058,	Adjusted R-squared:  0.8052 
# F-statistic:  1239 on 2 and 597 DF,  p-value: < 2.2e-16

# # plot nMDS
## depth
ggplot(data = nmds_dat,
       mapping = aes(x = MDS1, y = MDS2, colour = depth_bin)) +
  geom_point(size = 2.5) +
  scale_colour_viridis_d() +
  theme_classic()
## latitude
ggplot(data = nmds_dat,
       mapping = aes(x = MDS1, y = MDS2, colour = lat_bin)) +
  geom_point(size = 2.5) +
  scale_colour_viridis_d() +
  theme_classic()

