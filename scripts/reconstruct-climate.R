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

#source("./predict-temporal.R") # provides scores.predicted
#source("./predict-spatial.R")  # provides load.predicted

# The results are in load.predictions and score.predictions. Tese objects are
# saved as rds files which can be loaded:
load.predictions <- readRDS("../results/topo_mod_results/load_predictions.RDS")
score.predictions <- readRDS("../results/tempo_mod_results/score_predictions.RDS")


library(tibble)
library(dismo) # for biovars()
library(dplyr)
library(lubridate)



rasterLayerToDF <- function(layer, name) {
  pl <-  as.data.frame(rasterToPoints(layer))
  names(pl) <- c("x","y", name)
  return(pl)
}

# hacky below. Sorry. Should not need to hard code pc axes names
getLoadingsDF <- function(mtn, v) {
  ploadings <- load.predictions[[mtn]][[v]]
  pc1 <- rasterLayerToDF(ploadings[["PC1"]], "PC1")
  pc2 <- rasterLayerToDF(ploadings[["PC3"]], "PC2")
  pc3 <- rasterLayerToDF(ploadings[["PC3"]], "PC3")
  res <- inner_join(inner_join(pc1, pc2),pc3)
  return(res)
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


# expects 1 year of data as two daily time series: tmin and tmax
bioclim <- function(datet, tmin, tmax) {
  
  if (length(datet) < 300) {
    return(data.frame(BIO1=NA,
                      BIO2=NA,
                      BIO4=NA,
                      BIO5=NA,
                      BIO6=NA,
                      BIO7=NA,
                      BIO3=NA,
                      BIO10=NA,
                      BIO11=NA,
                      year=NA))
  }
  
  monthlies <- as_data_frame(list(datet=datet, tmin=tmin, tmax=tmax)) %>%
    group_by(month=month(datet)) %>%
    summarize(tmin=mean(tmin, na.rm=TRUE), tmax=mean(tmax, na.rm=TRUE),
              tmean=mean( (tmax+tmin)/2), na.rm=TRUE) %>%
    mutate(qtmean = zoo::rollmean(x = tmean, 3, align = "right", fill = NA))

  d <- list()
  d$BIO1 <- mean( (tmax+tmin)/2, na.rm=TRUE)
  d$BIO2 <- mean(tmax - tmin, na.rm=TRUE)
  d$BIO4 <- sd(monthlies$tmean) * 100
  d$BIO5 <- max(monthlies$tmax)
  d$BIO6 <- min(monthlies$tmin)
  d$BIO7 <- d$BIO5-d$BIO6
  d$BIO3 <- (d$BIO2/d$BIO7) * 100

  d$BIO10 <- max(monthlies$qtmean, na.rm=TRUE)
  d$BIO11 <- min(monthlies$qtmean, na.rm=TRUE)

  d <- as_data_frame(d)
  d$year <- year(datet[1])
  return(d)
}


# one year reconstruct and summarize
summarizeOneYear <- function(tmin_scores, tmax_scores, tmin_lmat, tmax_lmat) {
  
  ## tmins <- predict_monthly(tmin_scores, tmin_lmat)
  ## tmaxs <- predict_monthly(tmax_scores, tmax_lmat)
 
  tmax_smat <- as.matrix(dplyr::select(tmax_scores, -datet))      
  tmaxs <- tmax_smat %*% tmax_lmat

  tmin_smat <- as.matrix(dplyr::select(tmin_scores, -datet))
  tmins <- tmin_smat %*% tmin_lmat

  ndates <- dim(tmins)[1]
  ncoords <- dim(tmins)[2]

  res <- vector(mode="list", length=ncoords)
  for (i in 1:ncoords) {
    res[[i]] <- bioclim(tmin_scores$datet, tmins[,i], tmaxs[,i])
  }

  return(bind_rows(res))
}



## transform predicted loadings and predicted scores back to tmin and tmax values
# writes output to file. Loadings are PC axes for topgraphy, scores as PC axes
# for daily temperature values. Function expects daily scores but in full year
# chunks.
reconstructTemp <- function(mtn) {
  tmin_loadings <- getLoadingsDF(mtn, "tmin")
  tmax_loadings <- getLoadingsDF(mtn, "tmax")

  ## temporary: subsample landscape for testing purposes
  srows <- sample(1:nrow(tmin_loadings), 1000)
  tmin_loadings <- filter(tmin_loadings, row_number() %in% srows)
  tmax_loadings <- filter(tmax_loadings, row_number() %in% srows)
  ## end testing code

  
  tmin_scores <- score.predictions[[mtn]][["tmin"]]
  tmax_scores <- score.predictions[[mtn]][["tmax"]]

  # convert loadings to matrices now and once:
  tmin_lmat <- t(as.matrix(dplyr::select(tmin_loadings, -x, -y)))
  tmax_lmat <- t(as.matrix(dplyr::select(tmax_loadings, -x, -y)))
  
  # but do scores one year at a time
#  tmin_scores <- tmin_scores %>% group_by(year = year(datet))
#  tmax_scores <- tmax_scores %>% group_by(year = year(datet))


  years <- unique(year(tmin_scores$datet))

  res <- vector(mode="list", length=length(years))
  for (i in seq_along(years)) {
    print(years[i])
#    if(years[i]==1912) browser()
    tminsc <- filter(tmin_scores, year(datet)==years[i])
    tmaxsc <- filter(tmax_scores, year(datet)==years[i])
    res[[i]] <- summarizeOneYear(tminsc, tmaxsc, tmin_lmat, tmax_lmat)
    res[[i]] <- mutate(res[[i]], x=tmin_loadings$x, y=tmin_loadings$y)
  }
  res <- bind_rows(res)
  
  ## scores_matrix <- as.matrix(dplyr::select(pscores, -datet))

  ## res <- scores_matrix %*% loadings_matrix
  ## res <- data.frame(res)
  ## names(res) <- paste(ploadings$x, ploadings$y, sep="_")
  ## res$datet <- pscores$datet
  filename <- paste("reconstruct", "_", mtn, ".csv", sep="")
  write.csv(res, file.path("../results/", filename))
   return(res)
}


### Example using full tmin tmax time series and not summarizing to annual
### bioclim variables: Example on small dataset: sensor locations only for the
### DM. This works and gives numbers highly correlated witht he original
### values! Cool.
runExamplePrediction <- function() {

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

    reproduce <- test.scores %*% test.load
    reproduce <- data.frame(reproduce)
    names(reproduce) <- PCAs[["DM"]][["tmin"]]$loadings$sensor
    reproduce$datet <- PCAs[["DM"]][["tmin"]]$scores$datet

    write.csv(reproduce, "../results/DM-TMIN-example-reconstruct.csv")
}


## OK main script here:

CM_hist_climate <- reconstructTemp("CM")
DM_hist_climate <- reconstructTemp("DM")
GM_hist_climate <- reconstructTemp("GM")

# save these results:
write.csv(CM_hist_climate, "../results/reconstruct_CM.csv", row.names=FALSE)
write.csv(DM_hist_climate, "../results/reconstruct_DM.csv", row.names=FALSE)
write.csv(GM_hist_climate, "../results/reconstruct_GM.csv", row.names=FALSE)
