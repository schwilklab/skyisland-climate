# climate-figs.R

# depends upon iButton.R (for sensor reading functions) and load-sensor-data.R
# (to load summaries into workspace intelligently)

source("ggplot-theme.R")
Sys.setenv(TZ="CST6CDT")

# helper functions

jan2feb <- function(m){
  return(m==1 | m ==2)
}

jan2march <- function(m){
  return(m==1 | m ==2 | m == 3)
}

dec2jan <- function(m){
  return(m==12 | m ==1)
}

###############################################################################
## Main script starts here
###############################################################################

# output location for plots:
plot_output <- "../results/plots/"

# loads alltemps, temp.daily.sum andtemp.monthly.sum
source("./load-sensor-data.R")

## Relatve humidity
#allhumids.df <- zoo2df(allhumids, val="rh")
#allhumids.df<-merge(allhumids.df,alltemps.df)

#### statistics by sensor
# read list of sensors stats (elevation, waypoints, etc
sensors.raw <- read.csv("../microclimate/sensors.csv")
#sensors.raw$X <- NULL

## merge summaries with sensor location data
temp.daily.sum <- merge(temp.daily.sum,sensors.raw)
temp.monthly.sum <- merge(temp.monthly.sum,sensors.raw)


## graphs ###

# for graphing
bestfit <- geom_smooth(method="lm", color = "black")
##tdata = subset(temp.monthly.sum, yearmon != "2010-10")  # remove incomplete months for now

# Daily mMaximum temperatures by mtn
qplot(elev, mtmax,data=subset(temp.monthly.sum) , color=mtn) +
    facet_grid(year ~ month, scales="free") +
    scale_x_continuous("Elevation (m)") +
    scale_y_continuous("Average daily maximum temperature (C)") +
    geom_smooth(method="lm", se=FALSE, size=1)
#    bestfit
ggsave(file.path(plot_output, "dmax-by-month.pdf"))




## freezing events Jan - March by elevation
jan_march <- temp.monthly.sum %>% filter(jan2march(month)) %>%
    group_by(sensor, year, mtn, elev) %>%
    summarise(sumfreezes = sum(nfreezes, na.rm=TRUE),
              meanfreezes = mean(nfreezes, na.rm=TRUE),
              mtmin = mean(mtmin))

jan_feb <- temp.monthly.sum %>% filter(jan2feb(month)) %>%
    group_by(sensor, year, mtn, elev) %>%
    summarise(sumfreezes = sum(nfreezes, na.rm=TRUE),
              meanfreezes = mean(nfreezes, na.rm=TRUE),
              mtmin = mean(mtmin))
#jan_march <- ddply(jan_march, .(year, mtn,elev), summarize, sumfreezes = sum(nfreezes,na.rm=TRUE), meanfreezes = mean(nfreezes,na.rm=TRUE), avemin = mean(avemin))

#jan_feb <- subset(temp.monthly.sum, jan2feb(temp.monthly.sum$month))
#jan_feb <- ddply(jan_feb, .(year, mtn,elev), summarize, sumfreezes = sum(nfreezes,na.rm=TRUE), meanfreezes = mean(nfreezes,na.rm=TRUE), avemin = mean(mtmin))

qplot(elev, meanfreezes, data=jan_march, color=mtn) +
    facet_grid( year ~ .,scales="free") +
    scale_x_continuous("Elevation (m)") +
    scale_y_continuous("Mean Number of freezing events Jan-March") +
    geom_smooth()


## sum freezes jan2march by mtn range
ggplot(jan_march, aes(elev, meanfreezes, color=factor(year))) +
    geom_point() +
    facet_grid( mtn ~ .) +
    scale_x_continuous("Elevation (m)") +
    scale_y_continuous("Number of freezing nights Jan-March") +
    geom_smooth(se=FALSE, size=1) +
    pubtheme
ggsave(file.path(plot_output, "nfreezes-jan-march.pdf"),  width=col2, units="cm")


## sum freezes jan2march for DM
ggplot(subset(jan_march, mtn=="DM"), aes(elev, meanfreezes, color=factor(year))) +
    geom_point() +
    scale_x_continuous("Elevation (m)") +
    scale_y_continuous("Number of freezing nights Jan-March") +
    geom_smooth(method="lm", se=FALSE, size=1)
ggsave(file.path(plot_output, "DM-nfreezes-jan-march.pdf"))


## Daily min jan2feb for DM
ggplot(subset(jan_feb, mtn=="DM"), aes(elev, mtmin, color=factor(year))) +
    geom_point() +
    scale_x_continuous("Elevation (m)") +
    scale_y_continuous("Average minimum temperature Jan-Feb") +
    geom_smooth(method="lm", se=FALSE, size=1)
ggsave(file.path(plot_output, "DM-avemin-jan-feb.pdf"))


qplot(elev, mtmin, data=jan_march, color=mtn) +
    facet_grid( year ~ .,scales="free") +
    scale_x_continuous("Elevation (m)") +
    scale_y_continuous("Mean min Jan-March") +
    geom_smooth()



qplot(elev, mtmin,data=subset(temp.monthly.sum, dec2jan(month)), color=mtn) +
    facet_grid(month ~ .) +
    scale_x_continuous("Elevation (m)") +
    scale_y_continuous("Average daily minimum temperature (C)") +
    geom_smooth()
ggsave(file.path(plot_output, "dmin-by-month.pdf"))


##### functions to find thaw rates

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
  

## temps.df.DM <- zoo2df(alltemps.DM)
## temps.df.DM$day <- format(temps.df.DM$datet, "%F")
## temps.df.DM$month <- format(temps.df.DM$datet, "%m")
## temps.df.DM$year <- format(temps.df.DM$datet, "%y")
## temps.df.DM$hour <- as.numeric(format(temps.df.DM$datet, "%H"))

## d <- subset(temps.df.DM, month == "02" | month == "01" | month=="03")

## thawrates.DM <- ddply(d, .(sensor,day), summarize, thawmin = thawmins(temp,datet) )

## ## test on low elevation sensor
##  dd <- subset(thawrates.DM,sensor=="MI007") # & thawmin > 15)
## quantile(dd$thawmin, c(0.0001, 0.05,0.1,0.2,0.25), na.rm=TRUE)


# test ind sensors

ggplot(data = subset(temp.daily.sum, sensor == "GP612" & as.POSIXct(datet) > mdy("01-01-2014")), aes(datet, max)) + geom_line()


diff <- merge(subset(temp.daily.sum, sensor == "GP612"), subset(temp.daily.sum, sensor == "GP610"), by = "datet")

ggplot(data = subset(diff, as.POSIXct(datet) > mdy("10-01-2014")), aes(datet, min.x - min.y)) + geom_line()

