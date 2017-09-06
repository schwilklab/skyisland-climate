#!/usr/bin/env Rscript

## Note: this is a command-line script so that it can be called from bash
## scripts on the supercomputer cluster and run the indepent soil water
## reconstructions in parallel


## reconstruct across historical and projected time series and landscapes:


library(parallel)
library(lubridate)

OUT_DIR <- "../results/soil"

#no_cores <- 36 # detectCores()
no_cores <- detectCores()
print(sprintf("%d cores detected", no_cores))
CLUSTER <- makeCluster(no_cores-1, type="FORK")


source("./wx-data.R")
#source("./load_grids.R") # depends on rgdal, avoid. Instead:
topodfs <- readRDS("../results/tempdata/topodfs.rds")

soilmod <- readRDS("../results/soil/soilmod.RDS")


# time period of interest:
# For now, whole year.


hist_wx_data <- dplyr::mutate(hist_wx_data, date = datet, yr = year(datet))
proj_wx_data <- dplyr::mutate(proj_wx_data, date = datet, yr = year(datet))





myrollsum <- function(x, k, fill, align = c("center", "left", "right"), ...) {
  align <- match.arg(align)

  n <- length(x)
  stopifnot(k <= n)

  ix <- switch(align,
      "left" = { 1:(n-k+1) },
      "center" = { floor((1+k)/2):ceiling(n-k/2) },
      "right" = { k:n })

  xu <- unclass(x)
  y <- xu[k:n] - xu[c(1, seq_len(n-k))] # difference from previous
  y[1] <- sum(xu[1:k])		 # find the first
  # sum precomputed differences
  rval <- cumsum(y)

  x[ix] <- rval
  x[-ix] <- NA
  return(x)
}




summarizeChunk <- function(topo_chunk, the_wx, smod = soilmod) {
  expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))
  df <- expand.grid.df(topo_chunk, the_wx)
  df$gswc <- predict(smod, newdata=df, na.action=na.pass)
  #  df <- aggregate(df["gswc"], by=df[c("x", "y")], FUN= mean) slow!
  df <- dplyr::group_by(df, x,y)
  df <- dplyr::summarize(df, gswc = mean(gswc))
  return(df)
}


makeGSWCdf <- function(themtn, thegcm=NULL, thescenario=NULL, thetimep=NULL) {
  if (is.null(thegcm)) { # historical
    thewx <- dplyr::filter(hist_wx_data, mtn==themtn & yr > 1960 & yr < 2001)
  } else if (thetimep == "ref") {
    thewx <- dplyr::filter(proj_wx_data, mtn==themtn & gcm == thegcm &
                                           scenario == thescenario &
                                           yr > 1960 & yr < 2001)
  } else if (thetimep == "2020s") {
    thewx <- dplyr::filter(proj_wx_data, mtn==themtn & gcm == thegcm &
                                           scenario == thescenario &
                                           year(datet) >= 2010 & year(datet) < 2040)
  } else if (thetimep == "2050s") {
    thewx <- dplyr::filter(proj_wx_data, mtn==themtn & gcm == thegcm &
                                           scenario == thescenario &
                                           year(datet) >= 2040 & year(datet) < 2070)
  } else if (thetimep == "2080s") {
    thewx <- dplyr::filter(proj_wx_data, mtn==themtn & gcm == thegcm &
                                           scenario == thescenario &
                                           year(datet) >= 2070 & year(datet) < 2100)
  }

  # monthly rolling average
  rollprecip <- myrollsum(thewx$prcp, 30, align = "right", fill=NA)
  rollp_wx <- data.frame(date=thewx$date, rollp=rollprecip)  

  thetopo <-   topodfs[[themtn]]
  ## BEGIN_TESTING
  # thetopo <- thetopo[1:1000,]  # testing
  ## END_TESTING
  idx   <- splitIndices(nrow(thetopo), 500)
  topolist <- lapply(idx, function(ii) thetopo[ii,,drop=FALSE])
  ans   <- clusterApply(CLUSTER, topolist, summarizeChunk, the_wx=rollp_wx, smod=soilmod)
  return(do.call(rbind, ans))
}



#### MAIN COMMAND LINE SCRIPT ###

# run from command line. Expects, mtn, gcm, scenario. If only one argument is
# passed, it will conduct the historical reconstruction for that mtn range.
args <- commandArgs(trailingOnly=TRUE)

# test data for running interactively:
#args <- c("CM", "CCSM4.r6i1p1", "rcp45", "2020s" )

# Check if there is at least one argument: if not, return an error
print("arguments: ")
print(args)
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).\n", call.=FALSE)
}

tmtn <- args[1]
if (length(args)==1) {
  # historical
  print("Reconstructing historic soil water series")
  tgcm <- NULL
  tscenario <- NULL
  ttime <- NULL
} else {
  tgcm <- args[2]
  tscenario <- args[3]
  ttime <- args[4]
}

# now run the reconstruction
oname <-  paste(tmtn, tgcm, tscenario, ttime, sep="_")
print(oname)
res <- makeGSWCdf(tmtn, tgcm, tscenario, ttime)

# save files snapshots
res <- res[complete.cases(res),]

# historical
if(is.null(tgcm) ) {
  # full
  ofile <- file.path(OUT_DIR, paste(oname, "_19612000", ".RDS", sep=""))
  print(paste("Saving:", ofile))
  saveRDS(res, ofile)
} else {
  # projected summaries
  ofile <- file.path(OUT_DIR, paste(oname, ".RDS", sep=""))
  print(paste("Saving:", ofile))
  saveRDS(res, ofile)
}

stopCluster(CLUSTER)
