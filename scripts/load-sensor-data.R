# load-sensor-data.R

# loads all sensor data, summarizes, saves timeseries and summaries as .rds
# objects. Puts the following data into the global namespace:
# - alltemps (xts time series of all temperature sensors
# - temp.daily.sum (df of daily temperature stats)
# - temp.monthly.sum (df of monthly temperature stats)

# File locations for R data files (cached data)
TEMP_DATA <- "./temp-data/alltemps.rds"
DAILY_SUM <- "./temp-data/daily-sum.rds"
MONTHLY_SUM <- "./temp-data/monthly-sum.rds"
BUILD_TIMESTAMP <- "../microclimate/merged-ibutton/LAST_BUILD"

library(plyr)
library(reshape2)
library(lubridate)
library(xts)

# get_data()
# Lazy evaluation function.
# args:
#    dfile: path to file for rds R data object
#    time: time to check against.
#    func: Function to call if dfile modification time is older than time
#    ...: additional arguments handed to func.
get_data <- function(dfile, time, func, ...) {
    data.time <- ymd("2010-01-01") # earlier than any data in case file does
                                   # not even exist, below
    if(file.exists(dfile)) {
        data.time <- ymd_hms(file.info(dfile)$mtime)
    }

    if (time > data.time) {
        dots <- list(...) 
        res <- do.call(func, dots)
        saveRDS(res, file = dfile)
    } else {
        res <-  readRDS(dfile)
    }
    return(res)
}


###################################################################3
## Functions for data aggregation.  Dayly and monthly summaries
drange <- function (x) max(x) - min(x)


# Calculate number of freezes in a temperature series
numfreezes <- function(x) {
  return(sum(x <= 0,na.rm=FALSE))
}

# Turn a multi-column zoo time series into a (melted) data frame with sensor id
# as a facor column the argument 'val' is a string that becomes the column name
# for the value field (eg "temperature", "rh")
zoo2df <- function(df, val="temp"){
    t<- as.data.frame(df)
    t$date <- index(df)
    t<-melt(t, id="date")
    names(t) <- c("datet","sensor",val)
    return(t)
}


# use humidity=TRUE to calculate for humidity sensors
daily.summaries <- function(alltemps, humidity=FALSE){  
    mintemps.daily <- aggregate(alltemps,as.Date(index(alltemps)), FUN=min)
    maxtemps.daily <- aggregate(alltemps,as.Date(index(alltemps)), FUN=max)
    meantemps.daily <- aggregate(alltemps,as.Date(index(alltemps)), FUN=mean)
    dtr <- aggregate(alltemps,as.Date(index(alltemps)), FUN=drange)

    ## daily summaries:
    dailymin.df <- zoo2df(mintemps.daily,"min")
    dailymax.df <- zoo2df(maxtemps.daily,"max")
    dailymean.df <- zoo2df(maxtemps.daily,"mean")
    dtr.df <-  zoo2df(dtr,"dtr")
  
    return( Reduce(function(...) merge(..., all=T),
                   list(dailymin.df,dailymax.df,dailymean.df, dtr.df)))
}

# monthly summaries
monthly.summaries <- function(dailytemps){
    dailytemps$yearmon <- factor(format(dailytemps$date, "%Y-%m"))
    dailytemps$year    <- factor(format(dailytemps$date, "%Y"))
    dailytemps$month   <- factor(format(dailytemps$date, "%m"))
    avemax <- ddply(dailytemps, .(sensor,yearmon, year, month), summarize, avemax=mean(max))
    avemin <- ddply(dailytemps, .(sensor,yearmon, year, month), summarize, avemin=mean(min))
    meantemp <- ddply(dailytemps, .(sensor,yearmon, year, month), summarize, meant=mean(mean))
    meandtr <- ddply(dailytemps, .(sensor,yearmon, year, month), summarize, dtr = mean(dtr))
    freezes<- ddply(dailytemps, .(sensor,yearmon, year, month), summarize, nfreezes=numfreezes(min))
    meandtr$dtr[is.infinite(meandtr$dtr)] <- NA
    meandtr$dtr[is.nan(meandtr$dtr)] <- NA
  
    return(  Reduce(function(...) merge(..., all=T),
                    list(freezes,avemax,avemin,meantemp,meandtr)))
}


### main script ###
source("./iButton.R")

# get time of last iButton merge
if(! file.exists(BUILD_TIMESTAMP)) stop("Merged iButton data not found. Run build-merged-ibutton.py")
build_time <- scan(BUILD_TIMESTAMP, what="character", quiet=TRUE)
build_time <- ymd_hms(paste(build_time, collapse=" "))

alltemps <- get_data(TEMP_DATA, build_time, read_all_sensors) # retrieve the timeseries data
temp.daily.sum <- get_data(DAILY_SUM, build_time, daily.summaries, alltemps)
temp.monthly.sum <- get_data(MONTHLY_SUM, build_time, monthly.summaries, temp.daily.sum)
# preceding objects are now loaded in global namespace
