## random-forest.R
## Code to fit random forest models

## Random number seed
RSEED = 808
set.seed(RSEED)

source("./microclimate-topo-PCA.R")

TOPO_RES_DIR <- "../results/topo_mod_results/"

IND_VAR_NAMES <-  c("elev","ldist_ridge" , "ldist_valley",  "msd", "radiation","relev_l", "slope",
                 "zdist_ridge",   "zdist_valley")

library(randomForest)
library(sp)
library(dplyr)

# get the PCA scores and loadings. Function called below returns list of PCA
# objects, one for each mtn range. Use `force=TRUE` to rerun the PCAs,
# otherwise the loadings and scores are read from a data object saved in the
# results/tempdata directory.
PCAs <- loadPCAData() # remember to delete old cached data if necessary!

## To make raster maps in ggplot quickly
makeMap <- function(topolayer) {
    map <- raster::rasterToPoints(topolayer)
    map <- data.frame(map)
    names(map) <- c("x", "y", "var")
    ggplot(map, aes(x,y, fill=var)) +
        geom_raster()
}

## divide into training and test data: The importance of this is debatable
## because RF uses both boosting and bagging. There are a lot of online
## discussion boards out there debating this, and I really haven't used this in
## prior models because RF already gives accuracy statistics.
## DWS: I don't think we can afford to split up the sensors into training and
## testing with so few.
splitdf <- function(dataframe) {
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/2))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  return(list(trainset=trainset,testset=testset))
}


# Just a quick and dirty check of most important pairwise correlations Just
# checks PC1 and PC2 against all other variables and prints out pearson
# correlation coefficences greater than 0.5.
checkCorrelations <- function(mtn, var) {
    loadings <- PCAs[[mtn]][[var]]$loadings
    ## find corelated variables
    var.cors <- data.frame(cor(loadings[3:21]))
    var.cors$vars <- row.names(var.cors)
    ## PC1:
    print(paste("for mtn=", mtn, "and var=", var))
    print("PC1 correlates: ")
    print(var.cors %>% filter(abs(PC1) > 0.5) %>% dplyr::select(vars, PC1))
    ## PC2:
    print("PC2 correlates:")
    print(var.cors %>% filter(abs(PC2) > 0.5) %>% dplyr::select(vars, PC2))
        ## PC2:
    print("PC3 correlates:")
    print(var.cors %>% filter(abs(PC3) > 0.5) %>% dplyr::select(vars, PC2))
}


# fit the randomforest model
fitRandomForest <- function(df, dep.var) {
    ind.vars <- paste(IND_VAR_NAMES, collapse=" + ")
    formula <- as.formula(paste(dep.var, " ~ ", ind.vars))
    model <- randomForest(formula, data = df,
                          importance=TRUE, keep.forest=TRUE)
    return(model)
}


fitModelRunDiagnostics <- function(mtn, dep.var, axis) {
  # split and redirect output
  sink(file = file.path(TOPO_RES_DIR, paste(mtn, "_", dep.var, "_", axis, ".txt", sep="")),
       append = FALSE, split = TRUE)
  checkCorrelations(mtn, dep.var)  
  mod <- fitRandomForest(PCAs[[mtn]][[dep.var]]$loadings, axis)
  print(mod)
  png(file = file.path(TOPO_RES_DIR, paste(mtn, "_", dep.var, "_", axis, ".png", sep="")))
  varImpPlot(mod, type=1)
  dev.off()

  res <- raster::predict(topostacks[[mtn]], mod)
  sink(NULL)
  return(res)
}

  
  ## partialPlot(DM.tmin.mod, DM.tmin, "elev")
  ## partialPlot(DM.tmin.mod, DM.tmin, "relev_l")
  ## partialPlot(DM.tmin.mod, DM.tmin, "ldist_valley")
  ## partialPlot(DM.tmin.mod, DM.tmin, "radiation")
  ## partialPlot(DM.tmin.mod, DM.tmin, "ldist_valley")


predictions <- list()
for (mtn in c("CM", "DM", "GM")) {
  predictions[[mtn]] <- list()
  for (v in c("tmin", "tmax")) {
    predictions[[mtn]][[v]] <- list()
    for(a in c("PC1", "PC2", "PC3")) {
      predictions[[mtn]][[v]][[a]] <- fitModelRunDiagnostics(mtn, v, a)
    }
  }
}



## ###############################################################################
## ## DM TMIN
## ###############################################################################

## ## If we want tosplit into training and testing data:
## ## DM.tmin <- splitdf(PCAs[["DM"]][["tmin"]]$loadings)

## DM.tmin <- PCAs[["DM"]][["tmin"]]$loadings
## DM.tmin.mod <- fitRandomForest(DM.tmin, "PC1")
## DM.tmin.mod
## ## what are the important variables (via permutation)
## varImpPlot(DM.tmin.mod, type=1)
## partialPlot(DM.tmin.mod, DM.tmin, "elev")
## partialPlot(DM.tmin.mod, DM.tmin, "relev_l")
## partialPlot(DM.tmin.mod, DM.tmin, "ldist_valley")
## partialPlot(DM.tmin.mod, DM.tmin, "radiation")
## partialPlot(DM.tmin.mod, DM.tmin, "ldist_valley")

## ## Make the predicted loading surface
## DM.tmin.predPC1 <- raster::predict(DM.topostack, DM.tmin.mod)
## makeMap(DM.tmin.predPC1)
## raster::writeRaster(DM.tmin.predPC1, file=file.path(data_output, "predPC1_tmin.tif"),
##             overwrite=TRUE)

## ###############################################################################
## ## DM TMAX
## ###############################################################################
## DM.tmax <- PCAs[["DM"]][["tmax"]]$loadings
## DM.tmax.mod <- fitRandomForest(DM.tmax, "PC1")
## DM.tmax.mod
## ## what are the important variables (via permutation)
## varImpPlot(DM.tmax.mod, type=1)
## partialPlot(DM.tmax.mod, DM.tmax, "elev")
## partialPlot(DM.tmax.mod, DM.tmax, "relev_l")
## partialPlot(DM.tmax.mod, DM.tmax, "ldist_valley")
## partialPlot(DM.tmax.mod, DM.tmax, "radiation")
## partialPlot(DM.tmax.mod, DM.tmax, "ldist_ridge")

## ## Make the predicted loading surface
## DM.tmax.predPC1 <- raster::predict(DM.topostack, DM.tmax.mod)
## makeMap(DM.tmax.predPC1)
## raster::writeRaster(DM.tmax.predPC1, file=file.path(data_output, "predPC1_tmax.tif"),
##             overwrite=TRUE)



## ###############################################################################
## ## CM TMIN
## ###############################################################################

## ## If we want tosplit into training and testing data:
## ## CM.tmin <- splitdf(PCAs[["CM"]][["tmin"]]$loadings)

## CM.tmin <- PCAs[["CM"]][["tmin"]]$loadings
## CM.tmin.mod <- fitRandomForest(CM.tmin, "PC1")
## CM.tmin.mod
## ## what are the important variables (via permutation)
## varImpPlot(CM.tmin.mod, type=1)
## partialPlot(CM.tmin.mod, CM.tmin, "elev")
## partialPlot(CM.tmin.mod, CM.tmin, "relev_l")
## partialPlot(CM.tmin.mod, CM.tmin, "ldist_valley")
## partialPlot(CM.tmin.mod, CM.tmin, "radiation")
## partialPlot(CM.tmin.mod, CM.tmin, "ldist_valley")

## ## Make the predicted loading surface
## CM.tmin.predPC1 <- raster::predict(CM.topostack, CM.tmin.mod)
## makeMap(CM.tmin.predPC1)
## raster::writeRaster(CM.tmin.predPC1, file=file.path(data_output, "CM.predPC1_tmin.tif"),
##             overwrite=TRUE)

## ###############################################################################
## ## CM TMAX
## ###############################################################################
## CM.tmax <- PCAs[["CM"]][["tmax"]]$loadings
## CM.tmax.mod <- fitRandomForest(CM.tmax, "PC1")
## CM.tmax.mod
## ## what are the important variables (via permutation)
## varImpPlot(CM.tmax.mod, type=1)
## partialPlot(CM.tmax.mod, CM.tmax, "elev")
## partialPlot(CM.tmax.mod, CM.tmax, "relev_l")
## partialPlot(CM.tmax.mod, CM.tmax, "ldist_valley")
## partialPlot(CM.tmax.mod, CM.tmax, "radiation")
## partialPlot(CM.tmax.mod, CM.tmax, "ldist_ridge")

## ## Make the predicted loading surface
## CM.tmax.predPC1 <- raster::predict(CM.topostack, CM.tmax.mod)
## makeMap(CM.tmax.predPC1)
## raster::writeRaster(CM.tmax.predPC1, file=file.path(data_output, "predPC1_tmax.tif"),
##             overwrite=TRUE)
