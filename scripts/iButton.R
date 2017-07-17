# Functions for reading ibutton data

# Dylan W. Schwilk

# Script to read in ibutton data, clean the data, and return cleaned timeseries.
# This script reads from the merged-ibutton folder and data from each sensor
# must already be in a single large csv file. The python script
# `build-merged-ibutton.py` builds these single files from a directory
# heirarchy of raw ibutton files.


# Exports: read_all_sensors(). This is the only function another script will
# need to call.

library(lubridate)
library(xts)

iButtonTZ = "UTC"

MAXGAP = 60 * 12  # in minutes
TEMP_PERIOD = 30 # min

## constants
hour1 <- as.difftime(1,units="hours")
m30 <- as.difftime(30,units="mins")

####################################################
### functions for reading and cleaning ibutton data
###################################################

trunc_to_minutes <- function(x,nmin) {
  minute(x) <- nmin * (minute(x) %/% nmin)
  second(x) <- 0
  return(x)
}


strip.extension <- function(s){
    temp<-strsplit(s,".", fixed=TRUE)
    temp <-lapply(temp, function(x)x[1])
    return(unlist(temp))
}

read.sensor <-  function(filen){
    td <- read.csv(filen,sep=",", stringsAsFactors=FALSE)
    # fix any temps in farenheit
    td$Value[td$Unit == "F"] <- (5.0/9.0)  * (td$Value[td$Unit == "F"] - 32)
    td$dt <- mdy_hms(td$Date.Time, tz=iButtonTZ, truncated=1)
    td$dt <- trunc_to_minutes(td$dt,1)
  
    tempz <- xts(td$Value, td$dt)
    dupes <-duplicated(index(tempz))
    if(any(dupes)){
        #print(paste("Warning: duplicated time in sensor",filen))
        #print(tempz[dupes])
        tempz <-  tempz[!dupes]
    }
    # print(paste("Dupes removed from ", filen))

    allmins <- seq(trunc_to_minutes(start(tempz),1), end(tempz), by=as.difftime(1,unit="mins"))
    tempz <- merge(tempz, xts(, allmins)) 
    tempz <- na.approx(tempz, maxgap=MAXGAP) # linear interpolation not ideal
    extract_times <- trunc_to_minutes(index(tempz), TEMP_PERIOD)
    return(tempz[extract_times])
}

read.sensor.data <- function(sfiles){
    sensors=basename(sfiles)
    sensors = strip.extension(sensors)
    for (i in seq(along=sfiles)) {
        sensor = sensors[i]
        print(paste("reading sensor",sensor))
        tempz <- read.sensor(sfiles[i])
        assign(sensor, tempz)
    }

    # merge sensors
    print("merging time series")
    objs <- mget(sensors)
    stopifnot(length(objs) == length(sensors))
  
    big_series <- Reduce(function(...) merge(..., all=TRUE), objs)
    colnames(big_series) <- sensors
    return(big_series) 
}


read.sensor.dir <- function(d){
    sfiles <- list.files(path=d, pattern="*.csv",full.names=TRUE)
    r <- read.sensor.data(sfiles)
    return(r)
}


read_all_sensors <- function(){
    alltemps.DM <- read.sensor.dir("../microclimate/merged-ibutton/DM/T/")
    alltemps.GM <- read.sensor.dir("../microclimate/merged-ibutton/GM/T/")
    alltemps.CM <- read.sensor.dir("../microclimate/merged-ibutton/CM/T/")
    return(cbind(alltemps.CM, alltemps.DM, alltemps.GM))
}

