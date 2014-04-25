####geonames  -- use if we need to add new elevations
## library(geonames)
## options(geonamesUsername="dschwilk")
## getelev <- function(lat, lon){
##   return(GNsrtm3(lat=lat,lng=lon)[1])
## }

##  sn <- ddply(tail(sensors.raw), .(sensor) ,transform, elev.gn = getelev(lat,lon))
##  sn$elev.gn <- sn$srtm3
##  sensors.raw <- sn
##  write.csv(sensors.raw,file="dm-sensors.csv")
