
# example for Helen of how to model the pca loadings
library(randomForest)
pc.file <- "DM-tmin-loadings.csv"
dat <- read.csv(pc.file)
names(dat)
##build rf model
rf1 <- randomForest(PC1 ~  elev +  wetness	+ slope_degrees	+ relelev_l	+ ldist_tovalley	+ ruggedness	+ ldist_ridge	+ zdist_valley	+ radiation	+ relelev_watershed_minmax, data=dat, ntree=2000, mtry=5, importance=T)
#removed for now bc diff grid size relelev_z  +
print(rf1)
varImpPlot(rf1, type=1)
partialPlot(rf1, dat, "elev")
# now read in rasters, stack them
library(raster)
library(sp)


elev <- readAsciiGrid("elev.asc")
slope_degrees <- readAsciiGrid("slope.asc")
wetness <- readAsciiGrid("wetness.asc")
radiation<- readAsciiGrid("radiation.asc")
zdist_tovalley<- readAsciiGrid("distvalley.asc")
ldist_tovalley<- readAsciiGrid("ldist_valley.asc")
relelev_watershed_minmax <- readAsciiGrid("relelev_huc.asc")
relelev_l <- readAsciiGrid("relelev_l.asc")
relelev_z <- readAsciiGrid("relelev_z.asc")
ldist_ridge<- readAsciiGrid("upslope.asc")
ruggedness <- readAsciiGrid("ruggedness.asc")
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
writeRaster(predPC1, file="predPC1.tif", overwrite=TRUE)
