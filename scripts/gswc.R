library(reshape2)
library(plyr)
library(lubridate)
library(ggplot2)

sensor.soil <- read.csv("../microclimate/soil/sensor-soil.csv")
sensors <- read.csv("../microclimate/sensors.csv")
sensor.soil <- merge(sensor.soil,sensors, all.x=TRUE)
sensor.soil$date <- mdy(sensor.soil$date)

GSWC <- function(df) {
    return ( ((df$wb.soil.wet - df$wb) - (df$wb.soil.dry-df$wb)) / (df$wb.soil.dry -df$wb))
}

## save gswc in data file (in some cases we need this as original weigh data is gone)
gswc.backup <- sensor.soil$gswc
## calculate gswc
sensor.soil$gswc <- GSWC(sensor.soil)
## replace with backup where needed:
sensor.soil$gswc[is.na(sensor.soil$gswc)] <- gswc.backup[is.na(sensor.soil$gswc)]


# simple version
season <- function(d) {
    pseason <- function(x) {
        m <- month(x)
        if (m %in% c(3,4,5) ) {return ("Spring")}
        else if (m %in% c(6,7,8) ) {return ("Summer")}
        else if (m %in% c(9,10,11) ) {return ("Fall")}
        else {return ("Winter")}
    }
    
    sapply(d, pseason)
}

sensor.soil$season <- season(sensor.soil$date)
sensor.soil$year <- year(sensor.soil$date)


ggplot(sensor.soil, aes(gswc , elev)) +
    geom_point() +
    facet_grid(year ~ season)

ggplot(subset(sensor.soil, season=="Spring"), aes(gswc , elev)) +
    geom_point() +
    facet_grid( year ~ .)
