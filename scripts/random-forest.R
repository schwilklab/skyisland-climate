# random-forest.R
#
# Code to fit random forest models
library(randomForest)
library(raster)
library(sp)
library(maptools) # for readASCIIGrid

GIS_DATA_DIR <- "../rf/ascii"

# get the PCA scores and loadings. Function called below returns list of PCA
# objects, one for each mtn range. Use `force=TRUE` to rerun the PCAs,
# otherwise the loadings and scores are read from a data object saved in the
# results/tempdata directory.
source("./microclimate-topo-PCA.R")
PCAs <- loadPCAData()
dat <- PCAs[["DM"]]$tmin$loadings

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


# now read in rasters, stack them
elev <- readAsciiGrid(file.path(GIS_DATA_DIR, "elev.asc"))
slope_degrees <- readAsciiGrid(file.path(GIS_DATA_DIR, "slope.asc"))
wetness <- readAsciiGrid(file.path(GIS_DATA_DIR, "wetness.asc"))
radiation<- readAsciiGrid(file.path(GIS_DATA_DIR, "radiation.asc"))
zdist_tovalley<- readAsciiGrid(file.path(GIS_DATA_DIR, "distvalley.asc"))
ldist_tovalley<- readAsciiGrid(file.path(GIS_DATA_DIR, "ldist_valley.asc"))
relelev_watershed_minmax <- readAsciiGrid(file.path(GIS_DATA_DIR, "relelev_huc.asc"))
relelev_l <- readAsciiGrid(file.path(GIS_DATA_DIR, "relelev_l.asc"))
relelev_z <- readAsciiGrid(file.path(GIS_DATA_DIR, "relelev_z.asc"))
ldist_ridge<- readAsciiGrid(file.path(GIS_DATA_DIR, "upslope.asc"))
ruggedness <- readAsciiGrid(file.path(GIS_DATA_DIR, "ruggedness.asc"))

r1 <-raster(elev)
r2<-raster(slope_degrees)
r3<-raster(wetness)
r4<-raster(radiation)
r5<-raster(zdist_tovalley)
r6<-raster(ldist_tovalley)
r7<-raster(relelev_watershed_minmax)
r8<-raster(relelev_l)
#r9<-raster(relelev_z)# note this one has a different extent fix?
r10<-raster(ldist_ridge)
r11<-raster(ruggedness)
s <- stack(r1, r2, r3, r4, r5, r6, r7, r8, r10, r11)
topostack <- s
names(topostack) <- c("elev", "wetness",	"slope_degrees",	"relelev_l",	"ldist_tovalley",	"ruggedness",	"ldist_ridge",	"zdist_valley",	"radiation",	"relelev_watershed_minmax")
#removed for now "relelev_z"
# this makes the predicted loading surface
predPC1 <- predict(topostack, rf1, type="response")
writeRaster(predPC1, file=file.path(data_output, "predPC1.tif"), overwrite=TRUE)
