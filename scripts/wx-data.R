## wx_data.R.R

## Provides functions to read historical historic and GCM projected weather station data.

## function read_historical_wx()
## Historical data read from dropbox. Data from NCCD in csv format daily
## summaries. Export w_data to global namespace.


## function read_projected_wx()
## Read projected data


###############################################################################
## packages and constants

library(repmis) # for reading data from dropbox
## Note: getting rJava working took some setting of LD paths see
## http://stackoverflow.com/questions/12872699/error-unable-to-load-installed-packages-just-now/25932828#25932828.
## My solution, create file /etc/ld.so.conf.d/java.conf with the following
## lines:
## /usr/lib/jvm/default-java/jre/lib/amd64
## /usr/lib/jvm/default-java/jre/lib/amd64/server
## Then run sudo ldconfig

# historical data location
hist_data_file <- "sky-island-historical-wx-data.csv"
hist_data_id <- "ly4s9lbv1itmh06"

# Projected data location
proj_data <- "~/science/projects/sky-island/projects/CSC-downscaled_projections"


###############################################################################
read_historical_wx <- function() {
  repmis::source_DropboxData(hist_data_file, hist_data_id,
                             sep = ",", header = TRUE,
                             na.strings = "-9999")
}



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






