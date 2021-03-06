#!/usr/bin/env Rscript 
## predict-spatial.R
## Authors: Helen Poulos and Dylan Schwilk

## Code to fit random forest models to predict sensor locations "loadings"
## across the landscapes. Produces a list, `load.predictions` which contains
## results by mtn range and by variable. See reconstruct-climate.R for example
## use. This code saves the model fit diagnostics under TOPO_RES_DIR defined
## below

## Random number seed
RSEED = 808
set.seed(RSEED)

## If running this on its own, you need to source the file below first, but in
## the main workflow, this is sourced from reconstruct-climate.R

source("./microclimate-topo-PCA.R") # provides PCAs object

TOPO_RES_DIR <- "../results/topo_mod_results/"

IND_VAR_NAMES <-  c("elev","ldist_ridge" , "ldist_valley",  "msd", "radiation","relelev_l", "slope")

library(randomForest)
library(caret)
library(sp)
library(dplyr)
#library(doMC)  # caret will use if loaded
#registerDoMC(cores=7)

library(xgboost) 

DO_PAIR_PLOTS <- FALSE
if (require (GGally) ) DO_PAIR_PLOTS <- TRUE # for ggpairs() # may not be
                                             # installed on hrothgar

## To make raster maps in ggplot quickly
makeMap <- function(topolayer) {
    map <- raster::rasterToPoints(topolayer)
    map <- data.frame(map)
    names(map) <- c("x", "y", "var")
    ggplot(map, aes(x,y, fill=var)) +
      geom_raster() + ylab("Latitude") + xlab("Longitude")
}

# Just a quick and dirty check of most important pairwise correlations Just
# checks PC1 -- PC3 against all other variables and prints out pearson
# correlation coefficients greater than 0.5.
checkCorrelations <- function(mtn, var) {
    loadings <- PCAs[[mtn]][[var]]$loadings
    colNums <- match(c(IND_VAR_NAMES, c("PC1", "PC2", "PC3")), names(loadings))
    df <- loadings[colNums]
    ## find corelated variables
    var.cors <- data.frame(cor(df))
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
    print(var.cors %>% filter(abs(PC3) > 0.5) %>% dplyr::select(vars, PC3))
    if(DO_PAIR_PLOTS) {
      png(file.path(TOPO_RES_DIR, paste(mtn, "_", var, "_splot", ".png", sep="")),
          height=1200, width=1200)
      g <- ggpairs(df)
      print(g)
      dev.off()
    }
}


# fit the random forest model
fitRandomForest <- function(df, dep.var) {
    ind.vars <- paste(IND_VAR_NAMES, collapse=" + ")
    formula <- as.formula(paste(dep.var, " ~ ", ind.vars))
    model <- train(formula, data = df, tuneLength = 5,
      method = "rf", metric = "RMSE",
      trControl = trainControl(method = "cv", number = 5,
                               preProc = c("center", "scale"),
                               verboseIter = FALSE)) # DWS: I suggest leaving
                                                     # this FALSE or we will
                                                     # clog the whole log file
    return(model)
}


## Fit a boosted regression tree model
fitboost <- function(df, dep.var) {
  ind.vars <- paste(IND_VAR_NAMES, collapse=" + ")
  formula <- as.formula(paste(dep.var, " ~ ", ind.vars))
  xgmodel <- train(formula, data = df, tuneLength = 5, 
                   method = "xgbTree",metric="RMSE",
                   trControl = trainControl(method = "cv", number = 5,
                                            preProc = c("center", "scale"), 
                                            verboseIter = FALSE))
  return(xgmodel)
}


## This function fits an RF model and a boosted regression tree model, prints
## some summary stats, then predicts PCA loadings based on the boost model and
## returns those predicted PCA loadings.

## TODO: add model selection code so that the prediction uses the best model
## (either modrf or modboost)
fitModelRunDiagnostics <- function(mtn, dep.var, axis) {
  print(paste(mtn, dep.var, axis))

  ## fit an RF model and save it
  print("Fitting RF model")
  modrf <- fitRandomForest(PCAs[[mtn]][[dep.var]]$loadings, axis)
  saveRDS(modrf, file.path(TOPO_RES_DIR,
                           paste(mtn, "_", v, "_", axis, "_", "RF", ".RDS", sep="")))

  # do the same with a boost model
  print("Fitting BOOST model")
  modboost <- fitboost(PCAs[[mtn]][[dep.var]]$loadings, axis)
  # save the model object:
  saveRDS(modboost, file.path(TOPO_RES_DIR,
                              paste(mtn, "_", v, "_", axis, "_", "BOOST", ".RDS", sep="")))


  # then print some summary output on each model:
  print("modrf$resample")
  print(modrf$resample)
  print("modboost$resample")
  print(modboost$resample)
  
  # And some diagnostic plots on each

  ## TODO: this code does not make sense as these two objects, modrf and modboost,
  ## do not implement a default plot() method.
  
  ## print("Producing model diagnostic plots")
  ## png(file = file.path(TOPO_RES_DIR,
  ##                      paste(mtn, "_", dep.var, "_", axis, "_", "RF", ".png", sep="")))
  ## plot(modrf) # this produces RMSE by subsample by tree depth plots
  ## dev.off()

  ## png(file = file.path(TOPO_RES_DIR,
  ##                      paste(mtn, "_", dep.var, "_", axis, "_", "BOOST", ".png", sep="")))
  ## plot(modboost) # this produces RMSE by subsample by tree depth plots
  ## dev.off()
 

  # compare fits between rf output and boosted regression tree
  resamps <- resamples(list(xgboost=modboost, RF=modrf))
  print(summary(resamps))
  modelDifferences <- diff(resamps)
  print(summary(modelDifferences))

  png(file = file.path(TOPO_RES_DIR,
                       paste(mtn, "_", dep.var, "_", axis, "_", "MODDIFFS", ".png", sep="")))
  bwplot(modelDifferences, layout = c(2, 1), scales = list(x = list(relation="free")))
  dev.off()

  ## eg bestmod <-  ?????????????
  ## temporary, just choose boost model:
  bestmod <- modboost
  
  res <- raster::predict(topostacks[[mtn]], bestmod)
  return(res)
}


## Main script
##############
load.predictions <- list()
for (mtn in c("CM", "DM", "GM")) {
  load.predictions[[mtn]] <- list()
  for (v in c("tmin", "tmax")) {
    sink(file = file.path(TOPO_RES_DIR, paste(mtn, "_", v, ".txt", sep="")),
         append = FALSE, split = TRUE)
    checkCorrelations(mtn, v)
    load.predictions[[mtn]][[v]] <- list()
    for(a in c("PC1", "PC2", "PC3")) {
      load.predictions[[mtn]][[v]][[a]] <- fitModelRunDiagnostics(mtn, v, a)
    }
  }
  sink(NULL)
}



## Provides load.predictions

# and saves:
saveRDS(load.predictions, file.path(TOPO_RES_DIR, "load_predictions.RDS"))


## ###############################################################################
## ## Build models, predict rasters of PCA loadings
## ###############################################################################

## If we want to split into training and testing data:
## DM.tmin <- splitdf(PCAs[["DM"]][["tmin"]]$loadings)




## DM.tmin <- PCAs[["DM"]][["tmin"]]$loadings
## DM.tmin.mod <- fitBoost(DM.tmin, "PC1")
## DM.tmin.mod

## ## Make the predicted loading surface
## DM.tmin.predPC1 <- raster::predict(topostacks[["DM"]], DM.tmin.mod)
## makeMap(DM.tmin.predPC1) +  guides(fill=guide_legend(title="PC1"))
## ggsave("../results/topo_mod_results/DM-PC1.png", height=10, width=10)
## raster::writeRaster(DM.tmin.predPC1, file=file.path(data_output, "DM_predPC1_tmin.tif"),
##              overwrite=TRUE)


## #DM.tmin <- PCAs[["DM"]][["tmin"]]$loadings
## DM.tmin.mod <- fitboost(DM.tmin, "PC2")
## DM.tmin.mod
## ## what are the important variables (via permutation)
## #varImpPlot(DM.tmin.mod, type=1)

## ## Make the predicted loading surface
## DM.tmin.predPC2 <- raster::predict(topostacks[["DM"]], DM.tmin.mod)
## makeMap(DM.tmin.predPC2) +  guides(fill=guide_legend(title="PC2"))
## ggsave("../results/topo_mod_results/DM-PC2.png", height=10, width=10)
## raster::writeRaster(DM.tmin.predPC2, file=file.path(data_output, "DM_predPC2_tmin.tif"),
##              overwrite=TRUE)


## CM.tmin <- PCAs[["CM"]][["tmin"]]$loadings
## CM.tmin.mod <- fitboost(CM.tmin, "PC1")
## CM.tmin.mod
## #varImpPlot(CM.tmin.mod, type=1)

## ## Make the predicted loading surface
## CM.tmin.predPC1 <- raster::predict(topostacks[["CM"]], CM.tmin.mod)
## makeMap(CM.tmin.predPC1) +  guides(fill=guide_legend(title="PC1"))
## ggsave("../results/topo_mod_results/CM-PC1.png", height=10, width=10)
## raster::writeRaster(CM.tmin.predPC1, file=file.path(data_output, "CM_predPC1_tmin.tif"),
##              overwrite=TRUE)


## CM.tmin.mod <- fitboost(CM.tmin, "PC2")
## CM.tmin.mod
## CM.tmin.predPC2 <- raster::predict(topostacks[["CM"]], DM.tmin.mod)
## makeMap(CM.tmin.predPC2) +  guides(fill=guide_legend(title="PC2"))
## ggsave("../results/topo_mod_results/CM-PC2.png", height=10, width=10)
## raster::writeRaster(CM.tmin.predPC2, file=file.path(data_output, "CM_predPC2_tmin.tif"),
##              overwrite=TRUE)




## GM.tmin <- PCAs[["GM"]][["tmin"]]$loadings
## GM.tmin.mod <- fitboost(GM.tmin, "PC1")
## GM.tmin.mod
## #varImpPlot(GM.tmin.mod, type=1)

## ## Make the predicted loading surface
## GM.tmin.predPC1 <- raster::predict(topostacks[["GM"]], GM.tmin.mod)
## makeMap(GM.tmin.predPC1) +  guides(fill=guide_legend(title="PC1"))
## ggsave("../results/topo_mod_results/GM-PC1.png", height=10, width=10)
## raster::writeRaster(CM.tmin.predPC1, file=file.path(data_output, "CM_predPC1_tmin.tif"),
##              overwrite=TRUE)


## GM.tmin.mod <- fitboost(GM.tmin, "PC2")
## GM.tmin.mod
## GM.tmin.predPC2 <- raster::predict(topostacks[["GM"]], DM.tmin.mod)
## makeMap(GM.tmin.predPC2) +  guides(fill=guide_legend(title="PC2"))
## ggsave("../results/topo_mod_results/GM-PC2.png", height=10, width=10)



