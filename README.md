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
3. The "scores" are the temporal variation. We repdict these (PC1, PC2, PC3) from the wwather station records. See predict-temporal.R
4. We combine thes predictions to reconstruct predicted tmins and tmax in `reconstruct-climate.R`
