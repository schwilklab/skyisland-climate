#!/usr/bin/env Rscript

## Note: this is a command-line script so that it can be called from bash
## scripts on the supercomputer cluster and run the indepent cliamte
## reconstrucitons in parallel


# example call:
# ./reconstruct-climate.R CM CCSM4.r6i1p1 rcp45

# will run reconstruction for the Chisos ("CM") the CCSM4.r6i1p GCM and
# scenario rp45



## Steering code to

# 1. Run PCAs to split iBUtton tmin and tmax records into spatial and temporal
# components

# 2. Fit random forest models to spatial components ("loadings") and
# predict these axes across the full spatial extent in each mtn range.

# 3. Fit linear models to rpedict PCA scores (temporal component) from
# historical time series wx station data

# 4. use both predictions to reconstruct predicted historical tmins and tmaxes
# across the landscapes. Then summarize these by year to save space.

# Run the model fitting below at least once:

# source("./predict-temporal.R") # provides scores.predicted
# source("./predict-spatial.R")  # provides load.predicted

# The results are in load.predictions and score.predictions. These objects are
# saved as rds files which the code below reads as necessary

#

library(tibble)
library(dplyr)
library(lubridate)

library(parallel)

no_cores <- detectCores()
print(sprintf("%d cores detected", no_cores))
cl <- makeCluster(no_cores-1, type="FORK")


## parallel matrix multiplication
matprod.par <- function(cl, A, B) {
  if (ncol(A) != nrow(B)) stop("Matrices do not conform")
  idx   <- splitIndices(nrow(A), length(cl))
  Alist <- lapply(idx, function(ii) A[ii,,drop=FALSE])
  ## ans   <- clusterApply(cl, Alist, function(aa, B) aa %*% B, B)
  ## Same as above, but faster:
  ans   <- clusterApply(cl, Alist, get("%*%"), B)
  do.call(rbind, ans)
}

# todo: might need to move if results are large:
OUT_DIR <- "../results/reconstructions"

rasterLayerToDF <- function(layer, name) {
  pl <-  as.data.frame(raster::rasterToPoints(layer))
  names(pl) <- c("x","y", name)
  return(pl)
}

## functions to retrieve PCA loadings (spatial) and scores(temporal)

## Read necessary data (except gcm predictions which we only read as necessary)
# spatial:
TEMPO_RES_DIR <- "../results/tempo_mod_results/"
load.predictions <- readRDS("../results/topo_mod_results/load_predictions.RDS")

# Fxn to retrieve PCA loadings by mtn and variable
getLoadingsDF <- function(mtn, v) {
  ploadings <- load.predictions[[mtn]][[v]]
  pc1 <- rasterLayerToDF(ploadings[["PC1"]], "PC1")
  pc2 <- rasterLayerToDF(ploadings[["PC3"]], "PC2")
  pc3 <- rasterLayerToDF(ploadings[["PC3"]], "PC3")
  res <- inner_join(inner_join(pc1, pc2),pc3)
  return(res)
}


# temporal predictions:
# historical:
hist_score_predictions <- readRDS("../results/tempo_mod_results/hist_score_predictions.RDS")

# ... and future projected:
## Fxn to retrieve predicted PCA scores (temporal component). If gcm or scnario
## are NULL, the fxn returns the historical PCA time series for that range.
getScorePredictionSeries <- function(mtn, var, gcm=NULL, scenario=NULL) {
  if (is.null(gcm)) { # assume we want historic scores
    res <- hist_score_predictions[[mtn]][[var]] # already read from file
  }
  else { # read appropriate file
    fname <- fname <- file.path(TEMPO_RES_DIR,
                           paste("proj_score_predictions", gcm, scenario, mtn, var, sep="_"))
        fname <- paste(fname, "RDS", sep=".")
    res <- readRDS(fname)
  }
  return(res)
}


### Example using full tmin tmax time series and not summarizing to annual
### bioclim variables: Example on small dataset: sensor locations only for the
### DM. This works and gives numbers highly correlated witht he original
### values! Cool. This gives an example of daily prediction values and is just
### a toy.
testRunExamplePrediction <- function() {

    tl <- load.predictions$DM$tmin
    tl.pc1 <- as.data.frame(rasterToPoints(tl$PC1))
    names(tl.pc1) <- c("x","y", "PC1")
    tl.pc2 <- as.data.frame(rasterToPoints(tl$PC2))
    names(tl.pc2) <- c("x","y", "PC2")
    tl.all <- inner_join(tl.pc1, tl.pc2)


    ## # testing on original pca data, not predictions, for working out math:
    test.load <- PCAs[["DM"]][["tmin"]]$loadings %>% dplyr::select(PC1, PC2, PC3)
    test.scores <- PCAs[["DM"]][["tmin"]]$scores %>% dplyr::select(PC1, PC2, PC3)

    # get matrices in correct dimensions
    test.scores <- as.matrix(test.scores)
    test.load <- t(as.matrix(test.load))

    #reproduce <- test.scores %*% test.load
    matprod.par(cl, test.scores, test.load)

    reproduce <- data.frame(reproduce)
    names(reproduce) <- PCAs[["DM"]][["tmin"]]$loadings$sensor
    reproduce$datet <- PCAs[["DM"]][["tmin"]]$scores$datet

    write.csv(reproduce, "../results/DM-TMIN-example-reconstruct.csv")
    return(TRUE)
}


# bioclim annual summaries
## http://www.worldclim.org/bioclim

## BIO1 = Annual Mean Temperature
## BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
## BIO3 = Isothermality (BIO2/BIO7) (* 100)
## BIO4 = Temperature Seasonality (standard deviation *100)
## BIO5 = Max Temperature of Warmest Month
## BIO6 = Min Temperature of Coldest Month
## BIO7 = Temperature Annual Range (BIO5-BIO6)
## BIO8 = Mean Temperature of Wettest Quarter
## BIO9 = Mean Temperature of Driest Quarter
## BIO10 = Mean Temperature of Warmest Quarter
## BIO11 = Mean Temperature of Coldest Quarter


# expects 1 year of data as three daily time series: tmin, tmax and precip
bioclim <- function(tmin, tmax, precip, datet) {

  if (length(datet) < 300) {
    return(c(rep(NA, 11), year(datet[1])))
  }


  # Two quick rolling functions to avoid using zoo package
  rollsum <- function(x, n=3L){
    res <- tail(cumsum(x) - cumsum(c(rep(0, n), head(x, -n))), -n + 1)
    return( c(rep(NA, n-1), res) )
  }

  rollmean <- function(x, n=3L){
    res <-  ( tail(cumsum(x) - cumsum(c(rep(0, n), head(x, -n))), -n + 1) ) / n
    return( c(rep(NA, n-1), res) )
  }

  monthlies <- data_frame(datet=datet, tmin=tmin, tmax=tmax, prcp=precip, month=month(datet)) %>%
    group_by(month) %>%
    dplyr::summarize(tmin=mean(tmin, na.rm=TRUE), tmax=mean(tmax, na.rm=TRUE),
              tmean=mean( (tmax+tmin)/2, na.rm=TRUE), prsum = sum(prcp, na.rm=TRUE))

  monthlies <- monthlies %>%
    mutate(qtmean = rollmean(x = tmean, 3),
           qpsum  = rollsum(x = prsum, 3)
           )

  BIO1 <- mean( (tmax+tmin)/2, na.rm=TRUE)
  BIO2 <- mean(tmax - tmin, na.rm=TRUE)
  BIO4 <- sd(monthlies$tmean) * 100
  BIO5 <- max(monthlies$tmax)
  BIO6 <- min(monthlies$tmin)
  BIO7 <- BIO5-BIO6
  BIO3 <- (BIO2/BIO7) * 100
  BIO8 <- monthlies$qtmean[which.max(monthlies$qpsum)]   # mean temp of wettest q
  BIO9 <- monthlies$qtmean[which.min(monthlies$qpsum)]   # mean temp of driest q
  BIO10 <- max(monthlies$qtmean, na.rm=TRUE)
  BIO11 <- min(monthlies$qtmean, na.rm=TRUE)       
  year <- year(datet[1])
  
  return(c(BIO1, BIO2, BIO3, BIO4, BIO5, BIO6, BIO7, BIO8, BIO9, BIO10, BIO11, year))
}


# one year reconstruct and summarize
summarizeOneYear <- function(tmin_scores, tmax_scores, tmin_lmat, tmax_lmat, wxprecip) {

  # The main step: matrix multiplication temporal x top PCAs:
  tmax_smat <- as.matrix(dplyr::select(tmax_scores, -datet))      
  tmaxs <- tmax_smat %*% tmax_lmat

  tmin_smat <- as.matrix(dplyr::select(tmin_scores, -datet))
  tmins <- tmin_smat %*% tmin_lmat

  ndates <- dim(tmins)[1]
  ncoords <- dim(tmins)[2]
  

  idx   <- seq_along(tmins[1,])
  tmins_list <- lapply(idx, function(ii) tmins[,ii])
  tmaxs_list <- lapply(idx, function(ii) tmaxs[,ii])

  res <- mapply(bioclim, tmin=tmins_list, tmax=tmaxs_list,
                MoreArgs = list(datet=tmin_scores$datet, precip=wxprecip),
                SIMPLIFY=FALSE)
  
  return(do.call(rbind, res))
}


## transform predicted loadings and predicted scores back to tmin and tmax values
# writes output to file. Loadings are PC axes for topgraphy, scores as PC axes
# for daily temperature values. Function expects daily scores but in full year
# chunks.
reconstructTemp <- function(mtn, tmin_scores, tmax_scores, precip_series) {
  tmin_loadings <- getLoadingsDF(mtn, "tmin")
  tmax_loadings <- getLoadingsDF(mtn, "tmax")

  ### TESTING !!!!!
  #temporary: subsample landscape for testing purposes
  ## tmin_loadings <- filter(tmin_loadings, row_number() <=100)
  ## tmax_loadings <- filter(tmax_loadings, row_number() <=100)
  ## end testing code

  nxy <- nrow(tmin_loadings)
  chunk_size=1000
  res = matrix(, nrow = 1, ncol = 14)
  for(chunk in 0:((nxy %/% chunk_size))) {
    start <- chunk*chunk_size
    end   <- min(start+chunk_size-1, nxy)
    ## print(paste("start", as.character(start)))
    ## print(paste("end", as.character(end)))
    
    # convert loadings to matrices now and once. Transpose so that rows are PC
    # axes (3) and columns are landscape positions (many)
    tmin_lmat <- t(as.matrix(dplyr::select(tmin_loadings[start:end, ], -x, -y)))
    tmax_lmat <- t(as.matrix(dplyr::select(tmax_loadings[start:end, ], -x, -y)))

    years <- unique(year(tmin_scores$datet))

    cres <- vector(mode="list", length=length(years))
    for (i in seq_along(years)) {
      print(paste(as.character(years[i]), "chunk", chunk+1, "of",1 + ( nxy %/% chunk_size)))
      tminsc <- filter(tmin_scores, year(datet)==years[i])
      tmaxsc <- filter(tmax_scores, year(datet)==years[i])
      precipyr <- filter(precip_series, datet %in% tminsc$datet) %>% select(prcp)
    
      cres[[i]] <- summarizeOneYear(tminsc, tmaxsc, tmin_lmat, tmax_lmat, precipyr[,1])
      cres[[i]] <- cbind(cres[[i]], tmin_loadings$x[start:end], tmin_loadings$y[start:end])
    }
    cres <- do.call(rbind, cres)
    res <- rbind(res, cres)
  }

  colnames(res) <- c("BIO1", "BIO2", "BIO3", "BIO4", "BIO5", "BIO6", "BIO7",
                     "BIO8", "BIO9", "BIO10", "BIO11", "year", "x", "y")
  
  return(res[-1,]) # drop first empty row
}


## OK main script here:
source("./wx-data.R") # for hist and projected precip time series needed for
                      # bioclim 8 and 9

# run from command line. Expects, mtn, gcm, scenario. If only one argument is
# passed, it will conduct the historical reconstruction for that mtn range.
args <- commandArgs(trailingOnly=TRUE)

# test data for running interactively:
args <- "CM"

# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).\n", call.=FALSE)
}

tmtn <- args[1]
if (length(args)==1) {
  # historical
  print("Reconstructing historic climate series")
  tgcm <- NULL
  tscenario <- NULL
} else {
  tgcm <- args[2]
  tscenario <- args[3]
}

# now run the reconstruction
oname <-  paste(tmtn, tgcm, tscenario, sep="_")
print(oname)
if(is.null(tgcm)) {
  precip <- filter(hist_wx_data, mtn==tmtn)
} else {
  precip <- filter(proj_wx_data, gcm==tgcm, scenario==tscenario)
}

res <- reconstructTemp(tmtn,
                       getScorePredictionSeries(tmtn, "tmin", tgcm, tscenario),
                       getScorePredictionSeries(tmtn, "tmax", tgcm, tscenario),
                       precip)


# save time period snapshots

res <- res %>% as_tibble() %>% filter(complete.cases(.))

# historical
if(is.null(tgcm) ) {
  # full
  full_hist_sum <- res %>% group_by(x,y) %>% select(-year) %>%
    summarize_each(funs(mean))
  ofile <- file.path(OUT_DIR, paste(oname, "_fullhist", ".RDS", sep=""))
  print(paste("Saving:", ofile))
  saveRDS(full_hist_sum, ofile)
  #1961-2000
  sum_1961_2000 <- res %>%  filter(year >= 1961 & year <= 2000) %>%
    group_by(x,y) %>% select(-year) %>%
    summarize_each(funs(mean))
  ofile <- file.path(OUT_DIR, paste(oname, "_19612000", ".RDS", sep=""))
  print(paste("Saving:", ofile))
  saveRDS(sum_1961_2000, ofile)
} else {
  # 2020s, 2050s, and 2080s). So, 2050s is given by the mean of 2040–2069, etc
  proj_sum_2020s <- res %>%  filter(year >= 2010 & year < 2040) %>%
    group_by(x,y) %>% select(-year) %>%
    summarize_each(funs(mean))
  ofile <- file.path(OUT_DIR, paste(oname, "_2020s", ".RDS", sep=""))
  print(paste("Saving:", ofile))
  saveRDS(proj_sum_2020s, ofile)

  # 2050s
  proj_sum_2050s <- res %>%  filter(year >= 2040 & year < 2070) %>%
    group_by(x,y) %>% select(-year) %>%
    summarize_each(funs(mean))
  ofile <- file.path(OUT_DIR, paste(oname, "_2050s", ".RDS", sep=""))
  print(paste("Saving:", ofile))
  saveRDS(proj_sum_2050s, ofile)

  # 2080s
  proj_sum_2080s <- res %>%  filter(year >= 2070 & year < 2100) %>%
    group_by(x,y) %>% select(-year) %>%
    summarize_each(funs(mean))
  ofile <- file.path(OUT_DIR, paste(oname, "_2080s", ".RDS", sep=""))
  print(paste("Saving:", ofile))
  saveRDS(proj_sum_2080s, ofile)
}

cl.close()
