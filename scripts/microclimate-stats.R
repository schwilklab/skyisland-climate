# climate-stats.R script to read in ibutton data, clean the data, and produce
# dataframes with daily and monthly climate statistics. This script reads from
# the merged-ibutton folder and data from each sensor must already be in a
# single large csv file. build-merged-ibutton.py builds these single files from
# a directory heirarchy of raw ibutton files.

#library(zoo)
library(ggplot2)
#library(chron)
library(plyr)
library(reshape2)
library(lubridate)
library(xts)


Sys.setenv(TZ="CST6CDT")
MAXGAP = 60 * 12  # in minutes
TEMP_PERIOD = 30 # min

## constants
hour1 <- as.difftime(1,units="hours")
wd.original <- getwd()

# functions in other files
source("time-change.R") #  time change functions


vp.sat.groff <- function(C){
  T = C + 273.15 
  lew <-  10.79574 * (1-273.16/T)  - 5.02800 * log10( T/273.16 ) + 1.50475E-4 * (1.0 - 10^(-8.2969 * ( T/273.16 - 1.0)) )      + 0.42873E-3 * (10^(4.76955*(1-273.16/T)) - 1) + 0.78614
  
return(10^lew)
}

vpd <- function(rh,C){
  vpsat <- vp.sat.groff(C)
  return(vpsat -  vpsat * rh/100)
}

###############################################
### functions for reading and cleaning data
###############################################


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


findgaps <- function(x, msize = 12) {
  gaps <- rle(is.na(x))
  offsets <- c(1, cumsum(head(gaps$lengths, -1)) + 1)
#  shortgaps <- which(gaps$values & (gaps$lengths <= msize)))
  longgaps <- which(gaps$values & (gaps$lengths > msize))
  return(longgaps)
}


read.sensor <-  function(filen){
  td <- read.csv(filen,sep=",")
  # fix any temps in farenheit
  td$Value[td$Unit == "F"] <- (5.0/9.0)  * (td$Value[td$Unit == "F"] - 32)
  td$dt <- mdy_hms(td$Date.Time, tz=Sys.getenv("TZ"), truncated=1)
  td$dt <- trunc_to_minutes(td$dt,1)
  
  tempz <- xts(td$Value, td$dt)
  dupes <-duplicated(index(tempz))
  if(any(dupes)){
    #print(paste("Warning: duplicated time in sensor",filen))
    #print(tempz[dupes])
    tempz <-  tempz[!dupes]
  }
  print(paste("Dupes removed from ", filen))

  allmins <- seq(trunc_to_minutes(start(tempz),1), end(tempz), by=as.difftime(1,unit="mins"))
  tempz <- merge(tempz, xts(, allmins)) 
  tempz <- na.spline(tempz, maxgap=MAXGAP)
  extract_times <-  trunc_to_minutes(index(tempz), TEMP_PERIOD)  
  return(tempz[extract_times])
  }

read.sensor.data <- function(sfiles){
  sensors=basename(sfiles)
  sensors = strip.extension(sensors)
  for (i in seq(along=sfiles)) {
   sensor = sensors[i]
   print(paste("reading sensor",sensor))
   
   tempz <- read.sensor(sfiles[i])
   colnames(tempz) <- sensor
   assign(sensor, tempz)
  }

  # merge sensors
  print("merging time series")
  objs <- mget(sensors)
  stopifnot(length(objs) == length(sensors))
  
  big_series <- Reduce(function(...) merge(..., all=T), objs)
  return(big_series) 
}


read.sensor.dir <- function(d){
  sfiles <- list.files(path=d, pattern="*.csv",full.names=TRUE)
  r<-read.sensor.data(sfiles)
  return(r)
}


###################################################################3
## Functions for data aggregation.  Dayly and monthly summaries




## meant <- function(x) { mean(x,na.rm=TRUE)}
## mint <- function(x) {min(x,na.rm=TRUE)}
## maxt <- function(x) {max(x,na.rm=TRUE)}
drange <- function (x) max(x) - min(x)


# Turn a multi-column zoo time series into a (melted) data frame with sensor id
# as a facor column the argument 'val' is a string that becomes the column name
# for the value field (eg "temperature", "rh")
zoo2df <- function(df, val="temp"){
  t<- as.data.frame(df)
  t$date <- index(df)
  t<-melt(t, id="date")
  names(t) <- c("datet","sensor",val)
  t
}

# Calculate number of freezes in a temperature series
numfreezes <- function(x) {
  return(sum(x <= 0,na.rm=FALSE))
}


# use humidity=TRUE to calcualte for humidity sensors
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
  
return( Reduce(function(...) merge(..., all=T), list(dailymin.df,dailymax.df,dailymean.df, dtr.df)))

}

monthly.summaries <- function(dailytemps){
  ### some monthly summaries
  dailytemps$yearmon <- factor(format(dailytemps$date, "%Y-%m"))
  dailytemps$year    <- factor(format(dailytemps$date, "%Y"))
  dailytemps$month   <- factor(format(dailytemps$date, "%m"))
  avemax<- ddply(dailytemps, .(sensor,yearmon, year, month), summarize, avemax=mean(max))
  avemin<- ddply(dailytemps, .(sensor,yearmon, year, month), summarize, avemin=mean(min))
  meantemp <- ddply(dailytemps, .(sensor,yearmon, year, month), summarize, meant=mean(mean))
  meandtr <- ddply(dailytemps, .(sensor,yearmon, year, month), summarize, dtr = mean(dtr))
  freezes<- ddply(dailytemps, .(sensor,yearmon, year, month), summarize, nfreezes=numfreezes(min))
  meandtr$dtr[is.infinite(meandtr$dtr)] <- NA
  meandtr$dtr[is.nan(meandtr$dtr)] <- NA
  
  return(  Reduce(function(...) merge(..., all=T), list(freezes,avemax,avemin,meantemp,meandtr)))
}

###

## date time testing

#x <- read.sensor("../merged-ibutton/GM/T/DC614.csv")


#### Main script starts here


alltemps.DM <- read.sensor.dir("../microclimate/merged-ibutton/DM/T/")
alltemps.GM <- read.sensor.dir("../microclimate/merged-ibutton/GM/T/")
alltemps.CM <- read.sensor.dir("../microclimate/merged-ibutton/CM/T/")

##  index(alltemps)[19400:19500]  gives period of time change (when it shoudl have been)

## davis mtns
temp.daily.sum.DM <- daily.summaries(alltemps.DM)
temp.monthly.sum.DM <- monthly.summaries(temp.daily.sum.DM)

## GUMO
temp.daily.sum.GM <- daily.summaries(alltemps.GM)
temp.monthly.sum.GM <- monthly.summaries(temp.daily.sum.GM)

## BIBE
temp.daily.sum.CM <- daily.summaries(alltemps.CM)
temp.monthly.sum.CM <- monthly.summaries(temp.daily.sum.CM)

## merge all ranges
temp.daily.sum <- rbind(temp.daily.sum.DM,temp.daily.sum.GM, temp.daily.sum.CM)
temp.monthly.sum <- rbind(temp.monthly.sum.DM, temp.monthly.sum.GM, temp.monthly.sum.CM)


## Relatve humidity

#allhumids.df <- zoo2df(allhumids, val="rh")
#allhumids.df<-merge(allhumids.df,alltemps.df)


jan2feb <- function(m){
  return(m=="01" | m =="02")
}



jan2march <- function(m){
  return(m=="01" | m =="02" | m == "03")
}


dec2jan <- function(m){
  return(m=="12" | m =="1")
}


#### statistcs by sensor
# read list of sensors stats (elevation, waypoints, etc
sensors.raw <- read.csv("../microclimate/sensors.csv")
#sensors.raw$X <- NULL

## merge summaries with sensor location data
temp.daily.sum <- merge(temp.daily.sum,sensors.raw)
temp.monthly.sum <- merge(temp.monthly.sum,sensors.raw)

# for graphing
bestfit <- geom_smooth(method="lm", color = "black")
##tdata = subset(temp.monthly.sum, yearmon != "2010-10")  # remove incomplete months for now

###
## freezing events Jan - March by elevation
##
tdata <- subset(temp.monthly.sum, jan2feb(temp.monthly.sum$month))
tdata <- ddply(tdata, .(year,mtn,elev), summarize, sumfreezes = sum(nfreezes,na.rm=TRUE), meanfreezes = mean(nfreezes,na.rm=TRUE), avemin = mean(avemin))

qplot(elev, meanfreezes, data=tdata, color=mtn) + facet_grid( year ~ .,scales="free") +  scale_x_continuous("Elevation (m)") + scale_y_continuous("Mean Number of freezing events Jan-March") + geom_smooth()


qplot(elev, avemin, data=tdata, color=mtn) + facet_grid( year ~ .,scales="free") +  scale_x_continuous("Elevation (m)") + scale_y_continuous("Mean min Jan-March") + geom_smooth()



qplot(datet, max, data=subset(temp.daily.sum, mtn=="CM")) + facet_grid(sensor ~ .) + scale_y_continuous("Average daily maximum temperature (C)") 




qplot(elev, nfreezes, data=subset(temp.monthly.sum, mtn=="GM"))  + facet_grid( yearmon ~ .) + scale_x_continuous("Elevation (m)") + scale_y_continuous("Number of freezing events",limits=c(0,32)) + geom_smooth() #+ bestfit 
ggsave("nfreezes.pdf")

qplot(elev, avemax,data=temp.monthly.sum,color=mtn) + facet_grid( yearmon ~ .) + scale_x_continuous("Elevation (m)") + scale_y_continuous("Average daily maximum temperature (C)") + bestfit
ggsave("dmax-by-month.pdf")                                        

#qplot(elev, avemin,data=subset(tdata,mtn=="DM")) +  facet_grid(yearmon ~ .) + scale_x_continuous("Elevation (m)") + scale_y_continuous("Average daily minimum temperature (C)") + bestfit

qplot(elev, avemin,data=subset(tdata,is.winter(tdata), color=mtn)) +  facet_grid(yearmon ~ .) + scale_x_continuous("Elevation (m)") + scale_y_continuous("Average daily minimum temperature (C)") + bestfit
ggsave("dmin-by-month.pdf")
#+ geom_text(aes(label=sensor,size=3,angle=60, position="jitter") )


qplot(elev, dtr,data=subset(tdata,mtn=="GM")) +  facet_grid( yearmon ~ .) + scale_x_continuous("Elevation (m)") + scale_y_continuous("Average daily temperature range (C)") + bestfit
ggsave("dtr.pdf")

# this really just shows when sensors appread or were stopped:
qplot(date,min, data=temp.daily.sum) + facet_grid(sensor ~ .)



qplot(elev, nfreezes, data=tdata) + scale_x_continuous("Elevation (m)") + scale_y_continuous("Number of freezing events",limits=c(0,32)) + geom_smooth() #+ bestfit 
ggsave("nfreezes-winter.pdf")



##### functions to find thawy rates

thawtime <- function(x, dt) {
  fx <- x[1:(length(x)/2)]
  if(any(is.na(x))) return(NA)
  if(min(fx, na.rm=TRUE) > -4.0) return(NA)
  m <- which.min(fx)
  e <- m
#  print(m)
#  print(length(x))
  for(i in seq(m, length(x))){
 ##    if(is.na(x[i])) {
 ## #     print("na")
 ##      return(NA)
 ##    }
    if(x[i] > 0) {
      e <- i
      break
    }
    if( i == length(x)) return(NA)
  }
  return(dt[e]-dt[m])
}

thawmins <- function(x, dt) {
  r <- thawtime(x,dt)
  if(is.na(r)) return(NA)
  units(r)<- "mins"
  return(as.numeric(r))
 }
  

temps.df.DM <- zoo2df(alltemps.DM)
temps.df.DM$day <- format(temps.df.DM$datet, "%F")
temps.df.DM$month <- format(temps.df.DM$datet, "%m")
temps.df.DM$year <- format(temps.df.DM$datet, "%y")
temps.df.DM$hour <- as.numeric(format(temps.df.DM$datet, "%H"))

 d <- subset(temps.df.DM, month == "02" | month == "01" | month=="03")

thawrates.DM <- ddply(d, .(sensor,day), summarize, thawmin = thawmins(temp,datet) )

## low eelvation sensor
 dd <- subset(thawrates.DM,sensor=="MI007") # & thawmin > 15)
quantile(dd$thawmin, c(0.0001, 0.05,0.1,0.2,0.25), na.rm=TRUE)
