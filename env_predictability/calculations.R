# CALCULATE ENVIRONMENTAL MEASURES 

# loop over files
# site_code, latitude, longitude, survey_date

rls_raw = read_delim("/media/mari/Crucial X8/RLS_20190101_20221120.csv", skip = 71, delim = ",")
rls_sub = rls_raw %>%
          select(area, 
                 ecoregion, location, site_code, site_name,
                 latitude, longitude, survey_date, depth, block,
                 class, order, family, species_name,
                 size_class, total, biomass
                 )

sstFiles = list.files(pattern="data*.csv")
chlaFiles = list.files(pattern="data*.csv")