## Steering code to

# 1. Run PCAs to split iBUtton tmin and tmax records into spatial and temporal
# components

# 2. Fit random forest models to spatial components ("loadings") and
# predict these axes across the full spatial extent in each mtn range.

# 3. Fit linear models to rpedict PCA scores (temporal component) from historical time series wx station data

# 4. use both predictions to reconstruct predicted historical tmins and tmaxes
# across the landscapes.

library(raster) # for rasterToPoints
library(dplyr)

#source("./predict-temporal.R") # provides scores.predicted
#source("./predict-spatial.R")  # provides load.predicted

# For hrothgar, get predicted loadings and scores:

load.predictions <- readRDS("../results/topo_mod_results/load_predictions.RDS")
score.predictions <- readRDS("../results/tempo_mod_results/score_predictions.RDS")

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


## transform predcted loadings and predicted scores back to tmin and tmax values

# writes output to file
reconstructTemp <- function(mtn, v) {
  ploadings <- getLoadingsDF(mtn, v)
  pscores <- score.predictions[[mtn]][[v]]
  # two issues: different number of pc axes. Need to fix. AND can't do matrix
  # algebra on such big matrices. Solution?

  loadings_matrix <- t(as.matrix(dplyr::select(ploadings, -x, -y)))
  scores_matrix <- as.matrix(dplyr::select(pscores, -datet))
  res <- scores_matrix %*% loadings_matrix
  res <- data.frame(res)
  names(res) <- PCAs[[mtn]][[v]]$loadings$sensor
  res$datet <- PCAs[[mtn]][[v]]$scores$datet
  filename <- paste("reconstruct", "_", mtn, "_", v, ".csv", sep="")
  write.csv(res, file.path("../results/", filename))
}



### Example on small dataset: sensor locaitons only for the DM. This works and
### gives numbers highly correlated witht he original values! Cool.
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

reconstructTemp("DM", "tmin")

