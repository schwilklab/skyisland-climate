# Soil moisture data and calculations

library(lubridate)
library(ggplot2)

source("ggplot-theme.R")

sensor.soil <- read.csv("../microclimate/soil/sensor-soil.csv")
sensors <- read.csv("../microclimate/sensors.csv")
tempsensors <- sensors$sensor[sensors$type == "T"]
for (s in unique(sensor.soil$sensor)) {
    if(! s %in% tempsensors) {
        sensor.soil$sensor[sensor.soil$sensor == s] <- sensors$pair[sensors$sensor==s]
    }
}

sensor.soil <- merge(sensor.soil,sensors, all.x=TRUE)
sensor.soil$date <- mdy(sensor.soil$date)

# calcalute gravimetric soil water content
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


ggplot(sensor.soil, aes(elev, gswc, color=mtn)) +
    geom_point() +
    facet_grid(year ~ season)

ggplot(subset(sensor.soil, year==2014), aes(elev, 100*gswc)) +
    geom_point() +
    scale_x_continuous("Elevation") +
    scale_y_continuous("Gravimetric soil water content (%)", limits = c(0, 40) ) +
    facet_grid(. ~ mtn) +
    pubtheme
ggsave("../results/plots/gswc-2014.pdf", width=col2, height=col2*0.7, unit="cm")
# Note: some bad data, negative values!

## DWS check 2014 data
s2014<- s2014 <- unique(subset(sensor.soil, year==2014)$sensor)
## missing:
tempsensors <- sensors$sensor[sensors$type == "T"]
tempsensors[ ! tempsensors %in% s2014]

## existing data with missing dry weights
subset(sensor.soil, year == 2014 & is.na(wb.soil.dry))$sensor
