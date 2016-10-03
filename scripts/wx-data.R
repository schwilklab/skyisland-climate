## wx_data.R.R

## Provides functions to read historical historic and GCM projected weather station data.

## function read_historical_wx()
## Historical data read from dropbox. Data from NCCD in csv format daily
## summaries. Export w_data to global namespace.


## function read_projected_wx()
## Read projected data


###############################################################################
## packages and constants

library(dplyr)

# historical data location
hist_data_file <- "../data/sky-island-historical-wx-data.csv"
hist_data_file_remote <- "https://www.dropbox.com/s/2nx1kuyuswhd984/sky-island-historical-wx-data.csv?raw=1"


#hist_data_file <- "../wx-station-data/sky-island-historical-wx-data.csv"

# Projected data location
proj_data <- "~/science/projects/sky-island/projects/CSC-downscaled_projections"


###############################################################################


read_proj_wx_files <- function(wfiles) {
    dflist <- list()
    
    for (i in wfiles) {
        bname <- unlist(strsplit(basename(i), ".", fixed=TRUE))
        wx       <- bname[1]
        var      <- bname[5]

        print(paste("reading wx file ", i))
        temp <- read.csv(i, sep=" ")
        temp$gcm <- paste(bname[2], bname[3], sep=".")
        temp$scenario <-  bname[4]
        if(is.null(dflist[[wx]])) {
            dflist[[wx]] <- temp
        } else {
            dflist[[wx]] <- rbind.fill(dflist[[wx]], temp)
        }
    }
    return(dflist) 
}


read_projected_wx <- function(d=proj_data) {
    vars <- c("pr", "tasmax", "tasmin")
    byvar <- list()
    for (v in vars) {
        sfiles <- list.files(path=file.path(d, v), pattern="*.txt",full.names=TRUE)
        byvar[v] <- read_proj_wx_files(sfiles)
    }
    return(byvar) # not in best form yet
}

#temp <- read_proj_wx_files(list.files(path=file.path(proj_data, "pr"), pattern="*.txt",full.names=TRUE))

#proj_wx <- read_projected_wx()

getWXData <- function() {
  if(!file.exists(hist_data_file)) {
        download.file(hist_data_file_remote, hist_data_file)
  }
  return(read.csv(hist_data_file, stringsAsFactors=FALSE, na.strings="-9999"))
}



hist_wx_data <- getWXData()  %>%
  filter( !(is.na(TMIN) | is.na(TMAX) | is.na(PRCP) )) %>%
  mutate(datet = as.Date(as.character(DATE), "%Y%m%d" ),
         TMIN = (TMIN-32) * 0.5556,
         TMAX = (TMAX-32) * 0.5556,
         PRCP = PRCP * 25.4) 




# date ranges

hist_wx_data %>% filter(!is.na(TMIN) & !is.na(TMAX) & !is.na(PRCP)) %>% group_by(STATION_NAME) %>%
  summarize(mindate = min(DATE), maxdate=max(DATE))

stations <- read.csv("../microclimate/wx-stations.csv", stringsAsFactors=FALSE)
hist_wx_data <- left_join(hist_wx_data, stations)

#ggplot(hist_wx_data, aes(datet, TMIN)) + geom_line() + facet_grid( STATION_NAME ~ .)
