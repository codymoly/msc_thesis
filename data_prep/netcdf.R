# QUANTIFICATION OF ENVIRONMENTAL PREDICTABILITY ACCORDING TO BURGESS & MARSHALL (2015)
## here we have 3-dimensional data: latitude, longitude, time

# load required libraries
library(ncdf4)
library(raster)
library(rerddap)
library(tidyverse)

# clear memory
rm(list=ls())

# set working directory
setwd("~/Downloads")

# read netCDF file
sst_2022 = nc_open("subset.nc")

# data exploration
dim(sst_2022)
print(sst_2022)

# get variables
time = ncvar_get(sst_2022, "time")
lat = ncvar_get(sst_2022, "lat")
lon  = ncvar_get(sst_2022, "lon")

# check dimensions of lat and long
nlat = dim(lat)
nlon = dim(lon)
print(c(nlat, nlon))

# get temperature
dname = "sst"
tmp_array <- ncvar_get(sst_2022,dname)
dlname <- ncatt_get(sst_2022,dname,"long_name")
dunits <- ncatt_get(sst_2022,dname,"units")
fillvalue <- ncatt_get(sst_2022,dname,"_FillValue")
dim(tmp_array)