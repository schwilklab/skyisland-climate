# load-sensor-data.R

# loads all sensor data, summarizes, saves timeseries and summaries as .rds
# objects. Puts the following data into the global namespace:
# - alltemps (xts time series of all temperature sensors
# - temp.daily.sum (df of daily temperature stats)
# - temp.monthly.sum (df of monthly temperature stats)

# File locations for R data files (cached data)
TEMP_DATA <- "../results/tempdata/alltemps.rds"
DAILY_SUM <- "../results//tempdata/daily-sum.rds"
MONTHLY_SUM <- "../results/tempdata/monthly-sum.rds"
BUILD_TIMESTAMP <- "../microclimate/merged-ibutton/LAST_BUILD"

source("./data-cache.R") # loads lubridate as well

library(xts)
library(dplyr)
library(tidyr)

###################################################################3
## Functions for data aggregation.  Dayly and monthly summaries
drange <- function (x) max(x) - min(x)


# Calculate number of freezes in a temperature series
numfreezes <- function(x) {
  return(sum(x <= 0,na.rm=FALSE))
}

# Turn a multi-column zoo time series into a (melted) data frame with sensor id
# as a factor column the argument 'val' is a string that becomes the column name
# for the value field (eg "temperature", "rh")
zoo2df <- function(df, val="temp"){
    t<- as.data.frame(df)
    t$date <- index(df)
    t <- t %>% gather(sensor, val, -date) %>% setNames(c("datet", "sensor", val))
    return(t)
}


# use humidity=TRUE to calculate for humidity sensors
daily.summaries <- function(alltemps, humidity=FALSE){  
    mintemps.daily <- aggregate(alltemps,as.Date(index(alltemps)), FUN=min)
    maxtemps.daily <- aggregate(alltemps,as.Date(index(alltemps)), FUN=max)
    meantemps.daily <- aggregate(alltemps,as.Date(index(alltemps)), FUN=mean)
    dtr <- aggregate(alltemps,as.Date(index(alltemps)), FUN=drange)

    ## daily summaries:
    dailymin.df <- zoo2df(mintemps.daily,"tmin")
    dailymax.df <- zoo2df(maxtemps.daily,"tmax")
    dailymean.df <- zoo2df(meantemps.daily,"tmean")
    dtr.df <-  zoo2df(dtr,"dtr")
  
    return( Reduce(function(...) merge(..., all=T),
                   list(dailymin.df,dailymax.df,dailymean.df, dtr.df)))
}

# monthly summaries
monthly.summaries <- function(dailytemps){
    res <- dailytemps %>%
             mutate(year = year(datet), month = month(datet)) %>%
             group_by(sensor, year, month) %>%
             summarise(mtmax=mean(tmax), mtmin=mean(tmin), mtmean = mean(tmean),
                       mdtr=mean(dtr), nfreezes = numfreezes(tmin) )
    return( res %>% mutate(mdtr = ifelse(is.infinite(mdtr) | is.nan(mdtr), NA, mdtr) ) )
}


### main script ###
source("./iButton.R")

# get time of last iButton merge
if(! file.exists(BUILD_TIMESTAMP)) {
    stop("Merged iButton data not found. Run build-merged-ibutton.py")
}

build_time <- scan(BUILD_TIMESTAMP, what="character", quiet=TRUE)
build_time <- ymd_hms(paste(build_time, collapse=" "))

alltemps <- get_data(TEMP_DATA, build_time, read_all_sensors) # retrieve the timeseries data
temp.daily.sum <- get_data(DAILY_SUM, build_time, daily.summaries, alltemps)
temp.monthly.sum <- get_data(MONTHLY_SUM, build_time, monthly.summaries, temp.daily.sum)
# preceding objects are now loaded in global namespace
