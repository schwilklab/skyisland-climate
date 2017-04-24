skyisland-climate
=================

Microclimate measurement and modeling for Sky Island project


This repository holds data for the decagon soil moisture and precipitation sensors (`decagon` folder) and for the iButton temperature/humidity sensors and associated soil moisture measurements (`microclimate`)


## Requirements ##

### R and R packages ###

These analyses require R and the following packages: lubridate, xts, dplyr, ggplot2, raster, sp, pcaMethods, tidyr

Note that running on windows may require an up-to-date version of [lubridate](https://github.com/hadley/lubridate).  To obtain this, use the devtools package and install lubridate from github:

```R
library(devtools)

install_github("hadley/lubridate")
```

Not that this will require installing [Rtools for windows](http://cran.r-project.org/bin/windows/Rtools/) and installing the devtools package.

### Python ###

Requires a working python installation

## Specific Methods ##

### Details on iButton sensors ###

See [the microclimate README](./microclimate/README.md) for more information.


### Topographic variable calculations ###

See [the methods document](./methods/topo_grid_methods.md) for more information.


### Decomposing iButton daily records into spatial and temporal components

1. Run PCA on the daily temperature tmins and tmax separately for each mountain range
2. The "loadings" provide the spatial component and we predict these (PC1, PC2, PC3) from the topographic variables.  `See random-forest.R`
3. The "scores" are the temporal variation. We predict these (PC1, PC2, PC3) from the wwather station records. See predict-temporal.R
4. We combine thes predictions to reconstruct predicted tmins and tmax in `reconstruct-climate.R`


### Microclimate modeling steps ###

See github issue #8: https://github.com/schwilklab/skyisland-climate/issues/8

Steps for modeling microclimate across three west Texas mountain ranges:

The goal: A 60-year daily predicted time series for Tmin and one for Tmax for each point on the landscapes (and the same thing for the future under ESM projections). 

Modified goal: summarize each landscape point to a set of annual climate summary variables rather than having 365 tmin and tmax values for each landscape point.

To achieve this goal we are will produce functions (will take multiple steps) that predict daily tmin and tmax as a function of topographic variables AND single daily weather station time series. This is a completely separate process for each mtn range. To do this we decompose our iButton data into temporal and spatial components.

PCAs. For each mtn range, use PCA to reduce the iButton time series to a set of loadings and scores.  See "Decomposing iButton ...." above.  Save these PCA models because as we will use the "loadings" (PC axes) to create the topographical models, and then expand these across a full raster map (not just the actual iButton locations) then transform back to scores in order to fit the time model.

NOTE: I originally considered splitting the time series seasonally because the topographic effects on tmin and tmax seem to vary seasonally. But that is currently not impllemented and would add considerable complexity. It does not seem necessary in my current tests

Some details to record our decisions re PCA:

- Ignore iButtons for which fewer than 1000 days of data exist.  
- Impute missing values for PCA using ppca. See http://www.bioconductor.org/packages/release/bioc/html/pcaMethods.html
- Fit models that predict PCA loadings (axes) as a function of the topographic variables. We ahve two daily variables (tmin and tmax). So s models per mtn range. We used random forest models for prediction and run models on the top 3 PCA axes based on data exploration. See predict-topo.R for code.
- Fit a model: PCA scores (by date) ~ wxstation data (last 60 years of daily values). We use a basic linear model that uses wx station temperature and PRECIP data to predict PCA scores (3 PCA axes)
- Use models created in step 2 to predict scores for every past date (3x14360 matrix) and loadings for every xy on the landscape (1290564 x 3 matrix). Again, this is done for each mtn range and variable (tmin and tmax)
- Use data and models in step 4 and 5 to produce a predicted daily time series for each location on the landscape for past 60 years. We can also use the same models and data to predict next 100 years using our projected wx data. That is a big matrix multiplication -- naive result is 14360 x 1290564). See #35
