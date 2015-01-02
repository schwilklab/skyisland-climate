# microclimate-top-model.R

# depends upon iButton.R (for sensor reading functions) and load-sensor-data.R
# (to load summaries into workspace intelligently)

library(ggplot2)
library(reshape2)

Sys.setenv(TZ="CST6CDT")

# helper functions
jan2feb <- function(m){
  return(m=="01" | m =="02")
}

jan2march <- function(m){
  return(m=="01" | m =="02" | m == "03")
}

dec2jan <- function(m){
  return(m=="12" | m =="1")
}

###############################################################################
## Main script starts here
###############################################################################

# output location for plots:
plot_output <- "../results/plots/"


# Holden et al 2001 PCA approach to summarize time series first

# loads alltemps, temp.daily.sum andtemp.monthly.sum
source("./load-sensor-data.R")

# melt data
temps.melted <- melt(temp.daily.sum, id.var = c("datet", "sensor"))

# cast data back to original wide formats for tmin, tmax and tmean:
tmin <- dcast(subset(temps.melted, variable=="min"), datet ~ sensor)
tmax <- dcast(subset(temps.melted, variable=="max"), datet ~ sensor)
tmean <- dcast(subset(temps.melted, variable=="mean"), datet ~ sensor)


# Still need to deal with NAs



# read list of sensors stats (elevation, waypoints, etc
sensors.raw <- read.csv("../microclimate/sensors.csv")
sensors.topo <- read.csv("../microclimate/sensors_topo.csv")
sensors <- merge(sensors.raw[,c(1,2,3,4,5,6,7)], sensors.topo, by=c("sensor"))


