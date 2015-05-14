skyisland-climate
=================

Microclimate measurement and modeling for Sky Island project


This repository holds data for the decagon soil moisture and precipitation sensors (`decagon` folder) and for the iButton temperature/humidity sensors and associated soil moisture measurements (`microclimate`)


## Requirements ##

### R and R packages ###

These analyses require R and the following packages: lubridate, xts, plyr, ggplot2

Note that running on windows may require an up-to-date version of [lubridate](https://github.com/hadley/lubridate).  To obtain this, use the devtools package and install lubridate from github:

```R
library(devtools)

install_github("hadley/lubridate")
```

Not that this will require installing [Rtools for windows](http://cran.r-project.org/bin/windows/Rtools/) and installing the devtools package.

### Python ###

Requires a working python installation


See [the microclimate README](./microclimate/README.md) for more information.
