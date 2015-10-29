# microclimate-topo-PCA.R

# load iButton data and topgraphic data and run PCAs

# Holden et al 2001 PCA approach to summarize time series first. Use PCA to
# reduce spatial variation into a few PCA axes.

# depends upon iButton.R (for sensor reading functions) and load-sensor-data.R
# (to load summaries into workspace intelligently)


library(ggplot2)
library(reshape2)
# see http://www.bioconductor.org/packages/release/bioc/html/pcaMethods.html
library(pcaMethods)
library(dplyr)

# loads alltemps, temp.daily.sum and temp.monthly.sum
source("./load-sensor-data.R")
# Read in raster::Stack objects for each mtn range
source("./load_grids.R")
# sensor locations
sensors <- read.csv("../microclimate/sensors.csv", stringsAsFactors=FALSE)


extractVals1Mtn <- function(mtn, topostack) {
    sensors.mtn <- dplyr::select(filter(sensors, mtn==mtn), sensor, lon, lat)
    coords.sp <-sp::SpatialPoints(dplyr::select(sensors.mtn, lon, lat),
                              proj4string=sp::CRS(PROJ_STRING) )
    return(cbind(sensors.mtn, raster::extract(topostack, coords.sp)))
}

# TODO do for all mtn ranges once data is available
sensors.DM.topo <- extractVals1Mtn("DM", DM.topostack)
#sensors.CM <- extractVals1Mtn("CM", CM.topostack)
#sensors.GM <- extractVals1Mtn("GM", GM.topostack)

sensors.topo <- sensors %>% select(sensor, mtn) %>%
    left_join(sensors.DM.topo, by="sensor") # TODO: add CM, GM

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
   loadings <- merge(sensors.topo, loadings, by = c("sensor"))
   return(list(scores = scores, loadings = loadings))
}


getTempPCA <- function(df) {
    # first step is to get data in a wide format with once columns per sensor
    #df.melted <- melt(df, id.var = c("datet", "sensor"))
    df.cast <- df %>% tidyr::gather(variable, value, -datet, -sensor) %>%
        tidyr::spread(sensor, value)
    tmin <- df.cast %>% filter(variable=="tmin") %>% select(-variable)
    tmax <- df.cast %>% filter(variable=="tmax") %>% select(-variable)   
    tmin.PCA <- runPCA(tmin)
    tmax.PCA <- runPCA(tmax)
    return(list(tmin=tmin.PCA, tmax=tmax.PCA))
}

###############################################################################
## Main script starts here
###############################################################################

# output location for plots:
plot_output <- "../results/plots/"
data_output <- "../results/tempdata/"

# function to load the PCA scores and loadings into the workspace. To save
# running time, this checks to see if the csv files already exist and if they
# do, simply reads these rather than rerunning the PCAs. Unless force=TRUE, in
# which case the PCAs are rerun.
loadPCAData <- function(force=FALSE) {
    DM.PCA.file <- file.path(data_output, "DM-PCA.RData")
  #  CM.PCA.file <- file.path(data_output, "CM-PCA.RData")
  #  GM.PCA.file <- file.path(data_output, "GM-PCA.RData")

    if (all(file.exists(DM.PCA.file))) { #, file.exists(CM.PCA.file), file.exists(GM.PCA.file))) {
        DM.PCA <- readRDS(DM.PCA.file)
 #       GM.PCA <- readRDS(GM.PCA.file)
 #       CM.PCA <- readRDS(CM.PCA.file)
    } else {
        # run the PCA and save output to R data object
        # merge summaries with sensor location data
        temp.daily.sum.mtn <- temp.daily.sum %>% merge(dplyr::select(sensors, sensor, mtn))
        DM.PCA <- getTempPCA(subset(temp.daily.sum.mtn, mtn=="DM")[, 1:4])
  #      CM.PCA <- getTempPCA(subset(temp.daily.sum, mtn=="CM")[, 1:4])
  #      GM.PCA <- getTempPCA(subset(temp.daily.sum, mtn=="GM")[, 1:4])

        saveRDS(DM.PCA, DM.PCA.file)
   #     saveRDS(CM.PCA, CM.PCA.file)
   #     saveRDS(GM.PCA, GM.PCA.file)
    }
    return(list("DM" = DM.PCA)) # TODO should return list of all three mtn ranges
}


# some exmaple plots on tmin
##plot(DM.PCA$tmin$loadings[, c("elev", "relelev_z", "zdist_valley", "MSD", "PC1", "PC2", "PC3")])

## qplot(elev, PC1, data=DM.PCA$tmin$loadings)
## qplot(MSD, PC2, data=DM.PCA$tmin$loadings)
## qplot(relelev_watershed_minmax, PC1, data=DM.PCA$tmin$loadings)
## qplot(relelev_watershed_minmax, PC2, data=DM.PCA$tmin$loadings)
