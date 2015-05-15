#! /usr/bin/env Rscript
## script to take sensor master list and output a csv file in correct format
## for uploading to the garmin gps units

sensor.master <- read.csv("../microclimate/sensors.csv")
sensor.master$gone <- ifelse(is.na(sensor.master$gone), FALSE,TRUE)
clean.df <- subset(sensor.master, !gone)
clean.df <- data.frame(name=clean.df$sensor,
                       lat=clean.df$lat,
                       lon=clean.df$lon,
                       elev=clean.df$elev)
write.csv(clean.df, "../results/field-sheets/sensors-to-upload.csv", row.names=FALSE)
# then upload with
# gpsbabel -i unicsv -o garmin -f sensors-to-upload.csv -F usb:


## # And make a shapefile version
## library(sp)
## library(rgdal)
## shp.df <- SpatialPointsDataFrame(coords = clean.df[, c(3,2)], data = clean.df[, c(1,4)])
## writeOGR(shp.df, "../field-sheets/sensors-shp", layer="sensors-DM", driver="ESRI Shapefile")

## # pretty map of DM
## clean.df <- clean.df[clean.df$name %in% subset(sensor.master, mtn=="DM")$sensor, ]
## dmmap <- get_map(location = c(lon=median(clean.df$lon + 0.02), lat=median(clean.df$lat+0.02)), zoom=12)
## ggmap(dmmap) + geom_point(aes(lon,lat), data=clean.df)
