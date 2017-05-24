## wx_data.R.R

## Provides functions to read historical historic and GCM projected weather station data.

## function read_historical_wx()
## Historical data read from dropbox. Data from NCCD in csv format daily
## summaries. Exports dataframe `hist_wx_data` to global namespace.


###############################################################################
## packages and constants

library(dplyr)
library(tidyr) # for spread()

# historical data location
hist_data_file <- "../data/noaa_wx_170517.csv"

# Projected data folder location
proj_data_dir <- "../data/downscaled_projections"

###############################################################################


## Projected data
read_proj_wx_file <- function(fname) {
  bname <- unlist(strsplit(basename(fname), ".", fixed=TRUE))
  wxd <- read.csv(fname, sep=" ")
  wxd$station <- bname[1]
  wxd$gcm <- paste(bname[2], bname[3], sep=".") # need to document these
  wxd$scenario <-  bname[4] #  (low=rcp45, high=rcp85)
  wxd$var <- bname[5] # var, tasmin, tasmax or pr
  names(wxd)[4] <- "value" # value associated with var
  return(wxd)
}


# Read each projected wx file, rbind them up, then spread so that we have three
# data columns
read_proj_wx <- function(d=proj_data_dir) {
  sfiles <- list.files(path=d, recursive = TRUE, pattern="*.txt",full.names=TRUE)
  dlist <- lapply(sfiles, read_proj_wx_file)
  wxd <- bind_rows(dlist)

  # Now rename the variables to match our project
  wxd$var[wxd$var=="tasmax"] <- "tmax"
  wxd$var[wxd$var=="tasmin"] <- "tmin"
  wxd$var[wxd$var=="pr"] <- "prcp"

  # and spread the data so we have three value columns
  wxd <- spread(wxd, var, value)
  return(wxd)
}



## Main script

# Historical data
hist_wx_data <- read.csv(hist_data_file, stringsAsFactors=FALSE, na.strings="-9999")  %>%
#  filter( !(is.na(TMIN) | is.na(TMAX) | is.na(PRCP) )) %>%
  mutate(datet = as.Date(as.character(date), "%Y%m%d" ),
         tmin = (tmin-32) * 0.5556,
         tmax = (tmax-32) * 0.5556,
         prcp = prcp * 25.4) 


stations <- read.csv("../microclimate/wx-stations.csv", stringsAsFactors=FALSE)
hist_wx_data <- left_join(hist_wx_data, stations)


# Projected data
proj_wx_data <- read_proj_wx()
proj_wx_data <- left_join(proj_wx_data, stations)

# clean up
rm(stations, hist_data_file, proj_data_dir)



# data checks

# date ranges
## hist_wx_data %>% filter(!is.na(TMIN) & !is.na(TMAX) & !is.na(PRCP)) %>% group_by(STATION_NAME) %>%
##   summarize(mindate = min(DATE), maxdate=max(DATE))
## library(ggplot2)
## ggplot(hist_wx_data, aes(datet, TMIN)) + geom_line() + facet_grid( STATION_NAME ~ .)
