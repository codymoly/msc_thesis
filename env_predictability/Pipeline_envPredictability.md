# Pipeline for quantifying environmental predictability according to Marshall & Burgess (2015)

## 1. Preparation
- time series over 10 years relative to survey time at each location
- get rid of linear trends: extract the residuals from linear regression of raw time-series data

## 2. Seasonality
- assess seasonal trends: average monthly binned SST and chlA time series data
- seasonal time series over period of 10y: linear interpolation between monthly averages
- calculate variance of the seasonal trend per location and variance of residual time series

## 3. Environmental colour/ noise or autocorrelation
- residual time series without seasonal trend: substract seasonal value from each time-series value
- spectral analysis on residual time series; envPred stats with [envPred package by Barneche et al. (2018)](https://github.com/dbarneche/envPred)

Helpful sources:

[netCDF in R](https://pjbartlein.github.io/REarthSysSci/netCDF.html#get-coordinate-including-time-variables)

[Cheat sheet for netCDF handling](https://www.r-bloggers.com/2016/08/a-netcdf-4-in-r-cheatsheet/)

## References

Marshall, D. J. & Burgess, S. C. (2015). Deconstructing environmental predictability: seasonality, environmental colour and the biogeography of marine life. Ecology Letters, 18(2), 174-181.

