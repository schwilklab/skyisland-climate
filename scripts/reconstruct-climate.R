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
library(parallel)

#source("./predict-temporal.R") # provides scores.predicted
#source("./predict-spatial.R")  # provides load.predicted

# For hrothgar, get predicted loadings and scores:

load.predictions <- readRDS("../results/topo_mod_results/load_predictions.RDS")
score.predictions <- readRDS("../results/tempo_mod_results/score_predictions.RDS")


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

## transform predicted loadings and predicted scores back to tmin and tmax
## values. Writes output to file.
reconstructTemp <- function(mtn, v, cl, chunk_size=1500) {
  ploadings <- getLoadingsDF(mtn, v) #[1:1000,]
  pscores <- score.predictions[[mtn]][[v]]
  # two issues: different number of pc axes. Need to fix. AND can't do matrix
  # algebra on such big matrices. Solution?

  print(paste("extracting data:", mtn, v))
  loadings_matrix <- as.matrix(dplyr::select(ploadings, -x, -y))
  scores_matrix <- t(as.matrix(dplyr::select(pscores, -datet)))

  nxy <- nrow(loadings_matrix)
  
  for(chunk in 1:(nxy %/% chunk_size)) {
      start <- (chunk - 1)*chunk_size
      end   <- min(start+chunk_size, nxy)
      fname <- paste("reconstruct", "_", mtn, "_", v, "_", as.character(start),  ".csv", sep="")
      print(paste("Multiplying matrices:", mtn, v, as.character(start)))
      #res <- scores_matrix %*% loadings_matrix
      # produces matrix with dates as columns and locations (lat,lon) as rows
      res <- matprod.par(cl, loadings_matrix[start:end,], scores_matrix)
      print(paste("Creating dataframe and saving:", fname))
      res <- data.frame(res)
      names(res) <- pscores$datet 
      res$latlon <- paste(ploadings$x[start:end], ploadings$y[start:end], sep="_")
      write.csv(res, file=file.path("../results/", fname), row.names=FALSE)
  }
  print(paste("finished:", mtn, v))
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

    #reproduce <- test.scores %*% test.load
    matprod.par(cl, test.scores, test.load)

    reproduce <- data.frame(reproduce)
    names(reproduce) <- PCAs[["DM"]][["tmin"]]$loadings$sensor
    reproduce$datet <- PCAs[["DM"]][["tmin"]]$scores$datet

    write.csv(reproduce, "../results/DM-TMIN-example-reconstruct.csv")
    return(TRUE)
}


## OK main script here:

# wrapper takes vector with two strings, mtn and v:
reconstructTempWrapper <- function(i, cl) {
    reconstructTemp(i[1], i[2], cl)
}


no_cores <- detectCores()
print(sprintf("%d cores detected", no_cores))
cl <- makeCluster(no_cores-1, type="FORK")

params <- as.data.frame(t(expand.grid(c("CM", "DM", "GM"), c("tmin", "tmax"))))

# do for each parameter set in turn
lapply(params[1], reconstructTempWrapper, cl=cl)
stopCluster(cl)
