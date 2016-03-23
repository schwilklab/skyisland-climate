library(rgdal)
library(randomForest)
library(raster)
library(sp)
library(maptools) # for readASCIIGrid
library(ggplot2)
library(ggmap)
library(scales)

source("./load_grids.R")

# assign the vars below for CM, DM or GM
topostack <- CM.topostack
topodf <- data.frame(rasterToPoints(topostack))

# googlemap
get_gmap <- function(df) {
    return(get_map(location = c(left=min(df$x-0.01),
                                bottom=min(df$y-0.01),
                                right=max(df$x+0.01),
                                top=max(df$y+0.01))))
}

googlemap <- get_gmap(topodf)
zoommap <- get_map(c(lon=mean(topodf$x, na.rm=TRUE), lat=mean(topodf$y, na.rm=TRUE)),
                   maptype="satellite", zoom=12)

## Checking each topo var one by one

#elev
ggplot(topodf, aes(x,y, fill=elev)) + geom_raster()
any(is.na(topodf$elev))
# elev ok but DM is missing values
ggmap(googlemap) + geom_point(aes(x,y), data=subset(topodf, is.na(elev)))
#ggsave("../results/plots/dm-missing-elevations.png")

# flow_accum
ggplot(topodf, aes(x,y, fill=log10(flow_accum))) + geom_raster()
ggplot(topodf, aes(x=flow_accum)) + geom_histogram()
# Something is wrong with those super high outliers
quantile(topodf$flow_accum, c(0.1, 0.25, 0.5, 0.95, 1), na.rm=TRUE)
any(is.na(topodf$flow_accum))
# missing values in DM at least!

#ldist_ridge2
ggplot(topodf, aes(x,y, fill=log10(ldist_ridge2))) + geom_raster()
ggplot(topodf, aes(x=ldist_ridge)) + geom_histogram()
# not useful. wrong?


# ldist_ridge
ggplot(topodf, aes(x,y, fill=ldist_ridge)) + geom_raster()
ggplot(topodf, aes(x=ldist_ridge2)) + geom_histogram()
# looks ok

#ldist_valley2
ggplot(topodf, aes(x,y, fill=ldist_valley2)) + geom_raster()
ggplot(topodf, aes(x=ldist_valley2)) + geom_histogram()
# no good. sharp transitions

# ldist_valley
ggplot(topodf, aes(x,y, fill=ldist_valley)) + geom_raster()
ggplot(topodf, aes(x=ldist_valley)) + geom_histogram(binwidth=50)
ggmap(zoommap) + coord_cartesian() +
    geom_raster(aes(x,y,fill=ldist_valley), alpha=0.7, data=topodf) +
    scale_fill_gradient2(low=muted("white"), high=muted("red"))
ggsave("../results/plots/ldist_valley-cm_map.png")
# good


# msd
ggplot(topodf, aes(x,y, fill=log10(msd))) + geom_raster()
ggplot(topodf, aes(x=msd)) + geom_histogram(binwidth=1)
# Not sure -- some really high outliers again.

# radiation
ggplot(topodf, aes(x,y, fill=radiation)) + geom_raster()
ggplot(topodf, aes(x=radiation)) + geom_histogram()
any(is.na(topodf$radiation))
# Nope, bad data -- see outliers to right?
ggplot(topodf, aes(x=slope, y=radiation, color=elev)) + geom_point()
ggsave("../results/plots/radiation.png")

# googlemap of radiation
ggmap(googlemap) + coord_cartesian() +
    geom_raster(aes(x,y,fill=radiation), alpha=0.7, data=topodf) +
    scale_fill_gradient2(low="white", high=muted("red"),
                         midpoint=quantile(topodf$radiation, 0.1, na.rm=TRUE))
#ggsave("../results/plots/radiation_dm_map.png")

# relelev_l
ggplot(topodf, aes(x,y, fill=relelev_l)) + geom_raster()
ggplot(topodf, aes(x=relelev_l)) + geom_histogram()
ggplot(topodf, aes(x=slope, y=relelev_l, color=elev)) + geom_point()
# looks good

# relev_shed
ggplot(topodf, aes(x,y, fill=relelev_shed)) + geom_raster()
ggplot(cm.topodf, aes(x=relelev_shed)) + geom_histogram()
ggplot(cm.topodf, aes(x=slope, y=relelev_shed, color=elev)) + geom_point()
# No good, we can't use these arbitrary huc "watersheds" they are not
# conintuous


# relelev_z
ggplot(topodf, aes(x,y, fill=log10(relelev_z))) + geom_raster()
ggplot(topodf, aes(x=relelev_z)) + geom_histogram()
ggplot(topodf, aes(x=slope, y=relelev_z, color=elev)) + geom_point()  
# Maybe ok.  Why does this look so different than relelev_l?
 

# slope
ggplot(topodf, aes(x,y, fill=slope)) + geom_raster()
ggplot(topodf, aes(x=slope)) + geom_histogram()
# looks fine

# zdist_ridge
ggplot(topodf, aes(x,y, fill=log10(zdist_ridge))) + geom_raster()
ggplot(topodf, aes(x=zdist_ridge)) + geom_histogram()
# Looks funny to me

# zdist_valley
ggplot(topodf, aes(x,y, fill=zdist_valley)) + geom_raster()
ggplot(topodf, aes(x=zdist_valley)) + geom_histogram()
# googlemap of relelev_z zdist_valley
zoommap <- get_map(c(lon=mean(topodf$x, na.rm=TRUE), lat=mean(topodf$y, na.rm=TRUE)),
                   maptype="satellite", zoom=12)
ggmap(zoommap) + coord_cartesian() +
    geom_raster(aes(x,y,fill=zdist_valley), alpha=0.7, data=topodf) +
    scale_fill_gradient2(low=muted("white"), high=muted("red"))
# sharp transitions?
#ggsave("../results/plots/z_dist_valley-cm_map.png")
