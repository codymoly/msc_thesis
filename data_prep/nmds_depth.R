# nMDS FOR ASSESSING THE EFFECT OF DEPTH ON THE COMMUNITY COMPOSITION

# load libraries
library(tidyverse)
library(vegan)
library(corrplot)

# clean memory
rm(list=ls())

# set working directory
setwd("~/projects/msc_thesis")

# read raw rls data
rls_raw = read_delim("/media/mari/Crucial X8/rls_2019_2022_clean.csv", delim = ",")
coords_30 = read_delim("/media/mari/Crucial X8/coords_30km.csv", delim = ",")
traities = read_delim("/media/mari/Crucial X8/species_traits_imputed.csv", delim = ",") 


###### subset sites with minimum distance of 30km

# join both datasets
rls_30 = dplyr::left_join(coords_30, rls_raw, by = c("latitude", "longitude"))

# subset traities
bodysize = traities %>% 
  dplyr::mutate(species_name = paste(genus, species, sep = " ")) %>% 
  dplyr:: select(species_name, bodySize)

# select relevant columns
rls_sub = rls_30 %>%
  dplyr::filter(survey_date > "2019-12-31",
                block == 1) %>% 
  dplyr::select(site_code, latitude, longitude, survey_date, hour, depth,
         species_name, total
        )


###### join body size data
rls_sub_bs = left_join(rls_sub, bodysize, by = "species_name")

###### preparing the data for the nMDS

# tranform bodysize list into columns
rls_wide_bs = rls_sub_bs %>%
  pivot_wider(names_from = species_name,
              values_from = bodySize,
              names_sep = "_",
              values_fill = list(total = 0),
              values_fn = list(total = mean)
  )

# tranform total list into columns
rls_wide_species = rls_sub %>%
  pivot_wider(names_from = species_name,
              values_from = total,
              names_sep = "_",
              values_fill = list(total = 0),
              values_fn = list(total = mean)
              )

# define final set
rls_wide = 

# choose only one random observation per coordinate pair
set.seed(300)
rls_wide = rls_wide %>% 
  dplyr::group_by(latitude, longitude) %>% 
  dplyr::slice_sample(n = 1) %>% 
  dplyr::ungroup()
### copy this code for the other scripts to make sure that we always sample the same sites

# list species names
rls_names =
  select(rls_wide, -(site_code:block) ) %>%
  names()
rls_names

# choose random cases since the df is biiiiggg
set.seed(600)
rls_rsub = sample_n(rls_wide, 600)
rls_rsub = rls_wide

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
range(rls_rsub$depth)
rls_rsub_pos = rls_rsub
rls_rsub_pos$hour = as.POSIXct(rls_rsub_pos$hour, format="%H:%M:%S", tz="UTC")
rls_sub_binned = rls_rsub_pos %>% 
  mutate(depth_bin = cut(depth, breaks = c(0,10,20,30)),
         hour_bin = cut(hour, breaks = "2.5 hours"),
         lat_bin = cut(latitude, breaks = c(-5,-15,-25,-35,-45))
  )

site_dat = select(rls_sub_binned, site_code,
                  latitude, longitude,
                  survey_date, depth,
                  depth_bin, lat_bin, hour_bin)

# bind both dfs
nmds_dat = dplyr::bind_cols(nmds_dat, site_dat)

# check correlation between nMDS axes, latitude, longitude, and depth
nmds_dat %>%
  select(MDS1, MDS2, latitude, longitude, depth) %>%
  cor(x = ., method = c("pearson")) %>%
  corrplot(method = "number")

# # plot nMDS
## depth
ggplot(data = nmds_dat,
       mapping = aes(x = MDS1, y = MDS2, colour = depth_bin)) +
  geom_point(size = 2.5) +
  scale_colour_viridis_d() +
  theme_classic() +
  ggtitle("Depth bins") +
  theme(legend.position="right") +
  theme(plot.title = element_text(size = 16, face = "bold")) +
  theme(text = element_text(size=16)) +
  grid()
## latitude
ggplot(data = nmds_dat,
       mapping = aes(x = MDS1, y = MDS2, colour = lat_bin)) +
  geom_point(size = 2.5) +
  scale_colour_viridis_d() +
  theme_classic() +
  ggtitle("Latitude bins") +
  theme(legend.position="right") +
  theme(plot.title = element_text(size = 16, face = "bold")) +
  theme(text = element_text(size=16)) +
  grid()

ggplot(data = nmds_dat,
       mapping = aes(x = MDS1, y = MDS2, colour = hour_bin)) +
  geom_point(size = 2.5) +
  scale_colour_viridis_d() +
  theme_classic() +
  ggtitle("Daytime bins") +
  theme(legend.position="right") +
  theme(plot.title = element_text(size = 16, face = "bold")) +
  theme(text = element_text(size=16)) +
  grid()

