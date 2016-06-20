library(rgdal)
library(raster)
library(sp)
library(ggplot2)
library(ggmap)
library(scales)

source("./load_grids.R")

# googlemap
get_gmap <- function(df) {
    return(get_map(location = c(left=min(df$x-0.01),
                                bottom=min(df$y-0.01),
                                right=max(df$x+0.01),
                                top=max(df$y+0.01))))
}

# get some basemaps
CM.googlemap <- get_gmap(CM.topodf)
DM.googlemap <- get_gmap(DM.topodf)
GM.googlemap <- get_gmap(GM.topodf)

GM.zoommap <- get_map(c(lon=mean(GM.topodf$x, na.rm=TRUE), lat=mean(GM.topodf$y, na.rm=TRUE)),
                   maptype="satellite", zoom=13)

CM.zoommap <- get_map(c(lon=mean(CM.topodf$x, na.rm=TRUE), lat=mean(CM.topodf$y, na.rm=TRUE)),
                   maptype="satellite", zoom=12)

# plot for one variable
map_var <- function(topodf, var, basemap) {
    # overall raster map
    p <- ggmap(basemap) + coord_cartesian() +
        geom_raster(aes_string("x", "y" ,fill=var), alpha=0.7, data=topodf)
    p
}

## Checking each topo var one by one

#elev
map_var(CM.topodf, "elev", CM.googlemap)
map_var(DM.topodf, "elev", DM.googlemap)
map_var(GM.topodf, "elev", GM.googlemap)

any(is.na(CM.topodf$elev))
any(is.na(DM.topodf$elev))
any(is.na(GM.topodf$elev))
# All good

# flow_accum
map_var(CM.topodf, "flow_accum", CM.googlemap)
map_var(DM.topodf, "flow_accum", DM.googlemap)
map_var(GM.topodf, "flow_accum", GM.googlemap)
# ok, but high outliers. Probably won't use as direct predictor.
ggplot(CM.topodf, aes(x=flow_accum)) + geom_histogram()
quantile(CM.topodf$flow_accum, c(0.1, 0.25, 0.5, 0.95, 1), na.rm=TRUE)

#ldist_ridge2
map_var(CM.topodf, "ldist_ridge2", CM.googlemap)
map_var(DM.topodf, "ldist_ridge2", DM.googlemap)
map_var(GM.topodf, "ldist_ridge2", GM.googlemap)
# Basically just shows flow accum so same issues as above

# ldist_ridge
map_var(CM.topodf, "ldist_ridge", CM.googlemap)
map_var(DM.topodf, "ldist_ridge", DM.googlemap)
map_var(GM.topodf, "ldist_ridge", GM.googlemap)

ggplot(GM.topodf, aes(x=ldist_ridge)) + geom_histogram()
#
ggmap(GM.googlemap) + geom_point(aes(x,y), size = 0.001, data=subset(GM.topodf, ldist_ridge==0))
ggmap(GM.zoommap) + geom_point(aes(x,y), size = 0.001, data=subset(GM.topodf, ldist_ridge==0))
#ggsave("../results/plots/ridge-def-problem-example-GM.png")

# same issue for CM?
ggmap(CM.zoommap) + geom_point(aes(x,y), size = 0.001, data=subset(CM.topodf, ldist_ridge==0))
#ggsave("../results/plots/ridge-def-problem-example-CM.png")
# Ok, looks good now

#ldist_valley2
map_var(CM.topodf, "ldist_valley2", CM.googlemap)
ggsave("../results/plots/ldist_valley2_issue_example_CM.png")
map_var(DM.topodf, "ldist_valley2", DM.googlemap)
map_var(GM.topodf, "ldist_valley2", GM.googlemap)
# no good. sharp transitions.
ggplot(CM.topodf, aes(x=ldist_valley2)) + geom_histogram()


# ldist_valley
map_var(CM.topodf, "ldist_valley", CM.googlemap)
map_var(DM.topodf, "ldist_valley", DM.googlemap)
map_var(GM.topodf, "ldist_valley", GM.googlemap)
ggmap(GM.zoommap) + coord_cartesian() +
    geom_raster(aes(x,y,fill=ldist_valley), alpha=0.7, data=GM.topodf) +
    scale_fill_gradient2(low=muted("white"), high=muted("red"))
#ggsave("../results/plots/ldist_valley-cm_map.png")
# Looks good


# msd
map_var(CM.topodf, "msd", CM.zoommap)
map_var(DM.topodf, "msd", DM.googlemap)
map_var(GM.topodf, "msd", GM.zoommap)

ggplot(CM.topodf, aes(x=msd)) + geom_histogram(binwidth=1)
# Ok, but we'll need to think about how to sue this variable.

# radiation
map_var(CM.topodf, "radiation", CM.zoommap)
map_var(DM.topodf, "radiation", DM.googlemap)
map_var(GM.topodf, "radiation", GM.zoommap)

# Artefacts fixed!
# zoom in:
ggplot(aes(x=x, y=y, fill=radiation), data=GM.topodf) +
    geom_raster() +
    xlim(c(-104.9, -104.8 )) +
    ylim(c(31.9,32))
#ggsave("../results/plots/radiation-gm-zoom-artifacts.png")

# is the problem in the elev layer?
ggplot(aes(x=x, y=y, fill=elev), data=GM.topodf) +
    geom_raster() +
    xlim(c(-104.9, -104.8 )) +
    ylim(c(31.9,32))

ggplot(CM.topodf, aes(x=radiation)) + geom_histogram()
# ok, but distribution has been fixed


ggplot(CM.topodf, aes(x=slope, y=radiation, color=elev)) + geom_point()
#ggsave("../results/plots/radiation-issue-CM-example.png")
# Looks good now!

# googlemap of radiation
ggmap(CM.zoommap) + coord_cartesian() +
    geom_raster(aes(x,y,fill=radiation), alpha=0.7, data=CM.topodf) +
    scale_fill_gradient2(low="white", high=muted("red"),
                         midpoint=quantile(CM.topodf$radiation, 0.1, na.rm=TRUE))
#ggsave("../results/plots/radiation_dm_map.png")

# relelev_l # deleted
#map_var(CM.topodf, "relelev_l", CM.zoommap)
#map_var(DM.topodf, "relelev_l", DM.googlemap)
#map_var(GM.topodf, "relelev_l", GM.zoommap)
# Can't trust this as it depends upon the ridge definition (issue #29)


# relev_shed deleted
#map_var(CM.topodf, "relelev_shed", CM.googlemap)
#map_var(DM.topodf, "relelev_shed", DM.googlemap)
#map_var(GM.topodf, "relelev_shed", GM.googlemap)
#ggplot(CM.topodf, aes(x=relelev_shed)) + geom_histogram()
# No good, we can't use these arbitrary huc "watersheds" they are not
# continuous. Opened issue #32


# relelev_z  # It hought we wanted this one?
#map_var(CM.topodf, "relelev_z", CM.zoommap)
#map_var(DM.topodf, "relelev_z", DM.googlemap)
#map_var(GM.topodf, "relelev_z", GM.googlemap)
# So affected by issue #29
 

# slope
map_var(CM.topodf, "slope", CM.zoommap)
map_var(DM.topodf, "slope", DM.googlemap)
map_var(GM.topodf, "slope", GM.zoommap)

ggplot(CM.topodf, aes(x=slope)) + geom_histogram()
# looks fine

# zdist_ridge
map_var(CM.topodf, "zdist_ridge", CM.zoommap)
map_var(DM.topodf, "zdist_ridge", DM.googlemap)
map_var(GM.topodf, "zdist_ridge", GM.zoommap)

ggplot(CM.topodf, aes(x=zdist_ridge)) + geom_histogram()
# Looks good

# zdist_valley
map_var(CM.topodf, "zdist_valley", CM.zoommap)
map_var(DM.topodf, "zdist_valley", DM.googlemap)
map_var(GM.topodf, "zdist_valley", GM.zoommap)

ggplot(CM.topodf, aes(x=zdist_valley)) + geom_histogram()
# Ok

#In fact, I don't understand z_valley upon which this is based:
# z_valley

# z_valley gone
#map_var(CM.topodf, "z_valley", CM.zoommap)
#map_var(DM.topodf, "z_valley", DM.googlemap)
#map_var(GM.topodf, "z_valley", GM.zoommap)
