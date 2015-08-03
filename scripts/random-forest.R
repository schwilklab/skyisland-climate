# random-forest.R
#
# Code to fit random forest models
library(randomForest)
library(raster)
library(sp)
library(maptools) # for readASCIIGrid

# get the PCA scores and loadings. Function called below returns list of PCA
# objects, one for each mtn range. Use `force=TRUE` to rerun the PCAs,
# otherwise the loadings and scores are read from a data object saved in the
# results/tempdata directory.
source("./microclimate-topo-PCA.R")
PCAs <- loadPCAData()
dat <- PCAs[["DM"]]$tmin$loadings # grab DM dta only for testing. TODO: fix

#names(dat)

## build rf model
rf1 <- randomForest(PC1 ~  elev +  wetness	+ slope_degrees	+ relelev_l	+
                           ldist_tovalley	+ ruggedness + ldist_ridge +
                           zdist_valley + radiation + relelev_watershed_minmax,
                    data=dat, ntree=2000, mtry=5, importance=T)

# removed for now bc diff grid size relelev_z  +

print(rf1)
varImpPlot(rf1, type=1)
partialPlot(rf1, dat, "elev")

# load ascii grids
source("load_grids.R")

# this makes the predicted loading surface
predPC1 <- predict(topostack, rf1, type="response")
plot(predPC1)
writeRaster(predPC1, file=file.path(data_output, "predPC1.tif"), overwrite=TRUE)
