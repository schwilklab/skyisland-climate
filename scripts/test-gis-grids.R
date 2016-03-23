library(rgdal)
library(randomForest)
library(raster)
library(sp)
library(maptools) # for readASCIIGrid
library(ggplot2)

source("./load_grids.R")

# assign the vars below for CM, DM or GM
topostack <- DM.topostack
topodf <- data.frame(rasterToPoints(topostack))

## Checking each topo var one by one

makeMap("elev")
ggplot(topodf, aes(x,y, fill=elev)) + geom_raster()
# elev ok

makeMap("flow_accum")
ggplot(topodf, aes(x,y, fill=log10(flow_accum))) + geom_raster()
ggplot(topodf, aes(x=flow_accum)) + geom_histogram()
# Something is wrong with those super high outliers
quantile(topodf$flow_accum, c(0.1, 0.25, 0.5, 0.95, 1), na.rm=TRUE)
any(is.na(topodf$flow_accum))
# missing values in DM at least!

ggplot(topodf, aes(x,y, fill=ldist_ridge)) + geom_raster()
ggplot(topodf, aes(x=ldist_ridge)) + geom_histogram(binwidth=50)
# looks ok

ggplot(topodf, aes(x,y, fill=ldist_valley)) + geom_raster()
ggplot(topodf, aes(x=ldist_valley)) + geom_histogram(binwidth=50)
# good

ggplot(topodf, aes(x,y, fill=log10(msd))) + geom_raster()
ggplot(topodf, aes(x=msd)) + geom_histogram(binwidth=1)
# Not sure -- some really high outliers again.

ggplot(topodf, aes(x,y, fill=radiation)) + geom_raster()
ggplot(topodf, aes(x=radiation)) + geom_histogram()
# Nope, bad data -- see outliers to right?
ggplot(topodf, aes(x=slope, y=radiation, color=elev)) + geom_point()
ggsave("../results/plots/radiation.png")
# so problem is 

ggplot(topodf, aes(x,y, fill=relelev_l)) + geom_raster()
ggplot(topodf, aes(x=relelev_l)) + geom_histogram()
ggplot(topodf, aes(x=slope, y=relelev_l, color=elev)) + geom_point()
# looks good

ggplot(topodf, aes(x,y, fill=relelev_shed)) + geom_raster()
ggplot(cm.topodf, aes(x=relelev_shed)) + geom_histogram()
ggplot(cm.topodf, aes(x=slope, y=relelev_shed, color=elev)) + geom_point()
# weird polygons.  Might be ok. Is this using the usgs "watersheds"

ggplot(topodf, aes(x,y, fill=log10(relelev_z))) + geom_raster()
ggplot(topodf, aes(x=relelev_z)) + geom_histogram()
ggplot(topodf, aes(x=slope, y=relelev_z, color=elev)) + geom_point()  
# Maybe ok.  

ggplot(topodf, aes(x,y, fill=slope)) + geom_raster()
ggplot(topodf, aes(x=slope)) + geom_histogram()
# looks fine

ggplot(topodf, aes(x,y, fill=wetness)) + geom_raster()
ggplot(topodf, aes(x=wetness)) + geom_histogram()
ggplot(topodf, aes(x=slope, y=wetness, color=elev)) + geom_point()
ggsave("../results/plots/wetness.png")
# Hm, weird discontinuous data.  Maybe ok

ggplot(topodf, aes(x,y, fill=log10(zdist_ridge))) + geom_raster()
ggplot(topodf, aes(x=zdist_ridge)) + geom_histogram()
# Probably fine

ggplot(topodf, aes(x,y, fill=zdist_valley)) + geom_raster()
# why polygons?  Valley should be continuous
ggplot(topodf, aes(x=zdist_valley)) + geom_histogram()
