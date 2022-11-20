# Pipeline for quantifying environmental predictability

1. Download RLS dataset for given area and time
2. Extract coordinates for RLS sites
3. Download netCDF datasets for sst and chlA
4. Crop dataset to area of interest: choose coordinates according to RLS dataset, (spatially) average data around survey sites
5. Calculate envPred stats with [envPred package by Barneche et al. (2018)](https://github.com/dbarneche/envPred)

Helpful sources:

[netCDF in R](https://pjbartlein.github.io/REarthSysSci/netCDF.html#get-coordinate-including-time-variables)

[Cheat sheet for netCDF handling](https://www.r-bloggers.com/2016/08/a-netcdf-4-in-r-cheatsheet/)
