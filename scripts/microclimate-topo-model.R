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

# sensor lcoation and topographic data. Make available globally
sensors <- read.csv("../microclimate/sensors.csv")
sensors.topo <- read.csv("../microclimate/sensors_topo.csv")
sensors <- merge(sensors[,c(1,2,3,4,5,6,7)], sensors.topo, by=c("sensor"))
rm(sensors.topo)


# function to run PCA on iBUtton sensor (temperature) data
runPCA <- function(wdata, minlength=1000, nPC = 5) {
   # Throw out sensors with less than minlength values. 1000 works for full
   # data set. But this should be adjusted to keep missing data at <= 10% for
   # ppca method. TODO
   df <- wdata[, lengthNotNA(wdata) >= minlength]
   # Throw out rows in which all elements are NA:
   df <- df[rowSums(is.na(df[,-1])) != ncol(df[,-1]),]
   ## Run PCA
   df.PCA <- pca(df[,-1], nPcs=nPC, method="ppca", center=FALSE)
   # merge scores back with dates to run time series analysis:
   scores <- cbind(data.frame(datet=df$datet), as.data.frame(scores(df.PCA)))
   loadings <- as.data.frame(loadings(df.PCA))
   loadings$sensor <- rownames(loadings)
   loadings <- merge(sensors, loadings, by = c("sensor"))
   return(list(scores = scores, loadings = loadings))
}


getTempPCA <- function(df) {
   # first step is to get data in a wide format with once columns per sensor
   df.melted <- melt(df, id.var = c("datet", "sensor"))
   tmin <- dcast(subset(df.melted, variable=="min"), datet ~ sensor)
   tmax <- dcast(subset(df.melted, variable=="max"), datet ~ sensor)
   tmin.PCA <- runPCA(tmin)
   tmax.PCA <- runPCA(tmax)
   return(list(tmin=tmin.PCA, tmax=tmax.PCA))
}

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
## merge summaries with sensor location data
temp.daily.sum <- merge(temp.daily.sum,sensors)

## Try walk through on Davis Mtns (DM) data only
DM.PCA <- getTempPCA(subset(temp.daily.sum, mtn=="DM")[, 1:4])

# some exmaple plots on tmin
plot(DM.PCA$tmin$loadings[, c("elev", "relelev_z", "zdist_valley", "MSD", "PC1", "PC2", "PC3")])

qplot(elev, PC1, data=DM.PCA$tmin$loadings)
qplot(MSD, PC2, data=DM.PCA$tmin$loadings)
qplot(relelev_watershed_minmax, PC1, data=DM.PCA$tmin$loadings)
qplot(relelev_watershed_minmax, PC2, data=DM.PCA$tmin$loadings)


## Create temp csv files for Helen (TODO: remove)
write.csv(DM.PCA$tmin$scores, file.path(csv_output, "DM-tmin-scores.csv"), row.names=FALSE)
write.csv(DM.PCA$tmin$loadings, file.path(csv_output, "DM-tmin-loadings.csv"), row.names=FALSE)
write.csv(DM.PCA$tmax$scores, file.path(csv_output, "DM-tmax-scores.csv"), row.names=FALSE)
write.csv(DM.PCA$tmax$loadings, file.path(csv_output, "DM-tmax-loadings.csv"), row.names=FALSE)
          

## Read in historical data for time series analyses
source("./wx-data.R")
wx_data <- read_historical_wx()

