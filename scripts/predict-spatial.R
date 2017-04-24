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
IND_VAR_NAMES <-  c("elev","ldist_ridge" , "ldist_valley",  "msd", "radiation","relev_l", "slope")

library(caret)
library(sp)
library(dplyr)
#library(xgboost) # not used?

DO_PAIR_PLOTS <- FALSE
if (require (GGally) ) DO_PAIR_PLOTS <- TRUE # for ggpairs() # may not be
                                             # installed on hrothgar

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
    png(file.path(TOPO_RES_DIR, paste(mtn, "_", var, "_splot", ".png", sep="")),
        height=1200, width=1200)
    if(DO_PAIR_PLOTS) {
        g <- ggpairs(df)
        print(g)
        dev.off()
    }
}


# fit the random forest model
fitRandomForest <- function(df, dep.var) {
    ind.vars <- paste(IND_VAR_NAMES, collapse=" + ")
    formula <- as.formula(paste(dep.var, " ~ ", ind.vars))
    model <- train(formula, data = df, tuneLength = 10,
      method = "rf",
      trControl = trainControl(method = "cv", number = 10,  preProc = c("center", "scale"), verboseIter = TRUE))
    return(model)
    

}


## comment from @hpoulos:
# for some reason the print out gives an Rsquared of 1 for each model produced,
# but if you specify this print(model) it gives real Rsquared numbers. Not sure
# where to stick this into the function above I tried to put it beore the
# bracket, but that did nothing I also specified centering and scaling for
# pre-processing but the print(model) command lists "no pre-processing"

#Fit a boosted regression tree model
fitboost <- function(df, dep.var) {
  ind.vars <- paste(IND_VAR_NAMES, collapse=" + ")
  formula <- as.formula(paste(dep.var, " ~ ", ind.vars))
  xgmodel <- train(formula, data = df, tuneLength = 10,
                 method = "xgbTree",
                 trControl = trainControl(method = "cv", number = 5,
                                          preProc = c("center", "scale"),
                                          verboseIter = FALSE))
  return(xgmodel)
  
  }


fitModelRunDiagnostics <- function(mtn, dep.var, axis) {
  # split and redirect output
  print(paste(mtn, dep.var, axis))
  mod <- fitboost(PCAs[[mtn]][[dep.var]]$loadings, axis)
  # save the model object:
  saveRDS(mod, file.path(TOPO_RES_DIR, paste(mtn, "_", v, "_", axis, ".RDS", sep="")))
  # then print some summary output:

  print("mod$resample")
  print(mod$resample)
  
  png(file = file.path(TOPO_RES_DIR, paste(mtn, "_", dep.var, "_", axis, ".png", sep="")))
  plot(mod) # this produces RMSE by subsample by tree depth plots
  dev.off()

  ## for rf models:
  ## png(file = file.path(TOPO_RES_DIR, paste(mtn, "_", dep.var, "_", axis, ".png", sep="")))
  ## Any useful mode diagnostic plots?
  ## dev.off()
  res <- raster::predict(topostacks[[mtn]], mod)
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

#compare fits between rf output and boosted regresession tree
#can't get this to work. Says that object 'xgmodel' not found

## DWS: code below refers to non existent objects. Looks like objects taht were
## created in some one-off code somewhere else?

## results <- resamples(list(xgboost=res, RF=model))
## # summarize the distributions
## summary(results)
## # boxplots of results
## bwplot(results)
## # dot plots of results
## dotplot(results)

## Provides load.predictions

# and saves:
saveRDS(load.predictions, file.path(TOPO_RES_DIR, "load_predictions.RDS"))


## ###############################################################################
## ## DM TMIN
## ###############################################################################

## ## If we want to split into training and testing data:
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

