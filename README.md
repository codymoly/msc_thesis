![logo](./msc_logo.png)

# Beyond the mean: assessing the role of temperature variability and predictability in shaping the size structure of reef fish communities
## Master's thesis, Department of Marine Sciences, University of Gothenburg

This repository contains scripts, data links, and compiled data I use for my master's thesis. Dependencies are listed in the [requirements.txt](https://github.com/codymoly/msc_thesis/blob/main/requirements.txt).

## Summary
In this study, I investigated the effect of the variability and predictability of sea surface temperature (SST) on the size structure of reef fish communities in Australia using multiple linear regression analysis. Predicting the community-weighted mean (CWM) and variance (CWV) of body size (i.e., length), I compared the predictive performance of models containing different metrics of SST as predictors: mean, variance, seasonality, and the colour of noise (defined from the literature). 

## Current state
The next step is to build new hypotheses to test the effect of the different SST metrics on body size of fishes. [Here](https://codymoly.github.io/msc_thesis/), you can find interactive 4D cubes showing the model predictions from the preliminary analysis. Have fun!  

## Data sources

* [Reef Life Survey data on reef-fish abundance and biomass](https://portal.aodn.org.au/search)
* [FishBase data on life-hitory traits](https://www.fishbase.se/search.php)
* [Fish egg size data from Barneche et al. (2018)](https://github.com/dbarneche/fishEggSize/blob/master/data/fishEggsMSData.csv)
* [Pelagic larval duration data from Luiz et al. (2013)](https://doi.org/10.1073/pnas.1304074110)
* [Sea surface temperature data](https://cds.climate.copernicus.eu/cdsapp#!/dataset/satellite-sea-surface-temperature?tab=form)
* [Chlorophyll-a data](https://cds.climate.copernicus.eu/cdsapp#!/dataset/satellite-ocean-colour?tab=form)

