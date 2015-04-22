# microclimate-top-model.R

# depends upon iButton.R (for sensor reading functions) and load-sensor-data.R
# (to load summaries into workspace intelligently)

library(ggplot2)
library(reshape2)
#library(missMDA) # for imputing missing values for PCA
#library(factoMine) # for PCA
library(pcaMethods) # see http://www.bioconductor.org/packages/release/bioc/html/pcaMethods.html

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


lengthNotNA <- plyr::colwise(function(x) { sum( ! is.na(x))})

###############################################################################
## Main script starts here
###############################################################################

# output location for plots:
plot_output <- "../results/plots/"
csv_output <- "../results/tempdata/"

## Holden et al 2001 PCA approach to summarize time series first. Use PCA to
## reduce spatial variation into a few PCA axes.

# loads alltemps, temp.daily.sum andtemp.monthly.sum
source("./load-sensor-data.R")

# read list of sensors stats (elevation, waypoints, etc
sensors.raw <- read.csv("../microclimate/sensors.csv")
#sensors.raw$X <- NULL

## merge summaries with sensor location data
temp.daily.sum <- merge(temp.daily.sum,sensors.raw)


## Try walk through on Davis Mtns (DM) data only
temp.DM <- subset(temp.daily.sum, mtn=="DM")[, 1:4]

# melt data
DM.melted <- melt(temp.DM, id.var = c("datet", "sensor"))
# cast data back to original wide formats for tmin, tmax and tmean:
DM.tmin <- dcast(subset(DM.melted, variable=="min"), datet ~ sensor)
DM.tmax <- dcast(subset(DM.melted, variable=="max"), datet ~ sensor)

## Now deal with missing values
lengthNotNA(DM.tmin)

# Throw out sensors with less than 1000 values
DM.tmin <- DM.tmin[, lengthNotNA(DM.tmin) >= 1000]
DM.tmax <- DM.tmax[, lengthNotNA(DM.tmax) >= 1000]

# Throw out rows in which all elements are NA:
DM.tmin <- DM.tmin[rowSums(is.na(DM.tmin[,-1])) != ncol(DM.tmin[,-1]),]
DM.tmax <- DM.tmax[rowSums(is.na(DM.tmax[,-1])) != ncol(DM.tmax[,-1]),]


## Run PCA
DM.tmin.PCA <- pca(DM.tmin[,-1], nPcs=5, method="ppca", center=FALSE)
DM.tmax.PCA <- pca(DM.tmax[,-1], nPcs=5, method="ppca", center=FALSE)


# merge scores back with dates to run time series analysis:
DM.tmin.scores <- cbind(data.frame(datet=DM.tmin$datet), as.data.frame(scores(DM.tmin.PCA)))
DM.tmax.scores <- cbind(data.frame(datet=DM.tmax$datet), as.data.frame(scores(DM.tmax.PCA)))


DM.tmin.loadings <- as.data.frame(loadings(DM.tmin.PCA))
DM.tmin.loadings$sensor <- rownames(DM.tmin.loadings)

DM.tmax.loadings <- as.data.frame(loadings(DM.tmax.PCA))
DM.tmax.loadings$sensor <- rownames(DM.tmax.loadings)

# merge loadings with topographical data:
# read list of sensors stats (elevation, waypoints, etc
sensors.topo <- read.csv("../microclimate/sensors_topo.csv")
sensors <- merge(sensors.raw[,c(1,2,3,4,5,6,7)], sensors.topo, by=c("sensor"))
DM.tmin.loadings <- merge(sensors, DM.tmin.loadings, by = c("sensor"))
DM.tmax.loadings <- merge(sensors, DM.tmax.loadings, by = c("sensor"))

qplot(elev, PC2, data=DM.tmin.loadings)


## Create temp csv files for Helen
write.csv(DM.tmin.scores, file.path(csv_output, "DM-tmin-scores.csv"), row.names=FALSE)
write.csv(DM.tmin.loadings, file.path(csv_output, "DM-tmin-loadings.csv"), row.names=FALSE)

write.csv(DM.tmax.scores, file.path(csv_output, "DM-tmax-scores.csv"), row.names=FALSE)
write.csv(DM.tmax.loadings, file.path(csv_output, "DM-tmax-loadings.csv"), row.names=FALSE)
          


## Read in historical data for time series analyses
source("./wx-data.R")
wx_data <- read_historical_wx()

