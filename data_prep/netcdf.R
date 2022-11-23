# EXPEDITION INTO THE WORLD OF netCDF
## here we have 3-dimensional data: latitude, longitude, time
## the dataset used in the little test run contains gridded daily mean sst measurements from:
## the steps are derived from: https://towardsdatascience.com/how-to-crack-open-netcdf-files-in-r-and-extract-data-as-time-series-24107b70dcd

# load required libraries
#library(forecast)
library(ncdf4)
#library(raster)
#library(rerddap)
library(tidyverse)
#library(envPred)

# we like a clean environment: clear memory
rm(list=ls())

# set working directory
setwd("~/Downloads")

# open the netCDF file, here we only look at sst measurements from 2018
sst_2018 = nc_open("subset.nc")
print(sst_2018)

##### EXTRACT RELEVANT DATA #####

# print names of variable (SST) and dimensions (time, longitude, latitude)
attributes(sst_2018$var) # sst
attributes(sst_2018$dim) # t, lon, lat

# write lats and lons into objects
## latitude
lat = ncvar_get(sst_2018, "lat")
nlat = dim(lat)
## longitude
lon  = ncvar_get(sst_2018, "lon")
nlon = dim(lon)
## compare spatial dimensions with metadata, i.e., print(sst_2018)
print(c(nlon, nlat)) # output: [1] 720 350, alrighty!

## get the time dimension
## write time into an object
time = ncvar_get(sst_2018, "time")
head(time) # days are stored as in numerical order
ntime = dim(time) # It's 365, good.
## get units for time (days)
tunits <- ncatt_get(sst_2018, "time", "units")

# sea surface temperature data
## write sst into an object, i.e., array
sst_array = ncvar_get(sst_2018, "sst")
## get fill values
fillvalue = ncatt_get(sst_2018, "sst", "_FillValue")
## check dimensions
dim(sst_array) # output: [1] 720 350 365, i.e., lat long time
## replace fill values by NAs
sst_array[sst_array == fillvalue$value] <- NA


##### TRANSFORM DATA INTO HUMAN-FRIENDLY FORMAT #####

# convert time units into YYYY-MM-DD format
time_obs = as.POSIXct.Date(time,
                           tz ="GMT",
                           format = "%Y-%m-%d %H:%M:%S") # wrong year... why?
dim(time_obs) # output: [1] 365
range(time_obs) # check start and end date

# create a dataframe with all netCDF variables
## write sst values of the whole area at day 50 
sst_slice <- sst_array[ , , 50]
## plot sst at day 50
image(lon, lat, sst_slice)
## 2D-matrix with lat, lon, time
dim_mx = as.matrix(expand.grid(lon, lat, time_obs))
## write sst array into vector
sst_vec_long <- as.vector(sst_array)
length(sst_vec_long) 
## merge all into dataframe
sst_df <- data.frame(cbind(dim_mx, sst_vec_long))
## rename column names
colnames(sst_df) = c("Long", "Lat", "Date", "SST_degc")




