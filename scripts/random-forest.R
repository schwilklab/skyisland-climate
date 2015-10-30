## random-forest.R
## Code to fit random forest models

## Random number seed
RSEED = 808
set.seed(RSEED)

source("./microclimate-topo-PCA.R")

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
    var.cors <- data.frame(cor(loadings[3:22]))
    var.cors$vars <- row.names(var.cors)
    ## PC1:
    print(paste("for mtn=", mtn, "and var=", var))
    print("PC1 correlates: ")
    print(var.cors %>% filter(abs(PC1) > 0.5) %>% select(vars, PC1))
    ## PC2:
    print("PC2 correlates:")
    print(var.cors %>% filter(abs(PC2) > 0.5) %>% select(vars, PC2))
}


# fit the randomforest model
fitRandomForest <- function(df, dep.var) {
    ind.vars <- names(df)[5:17]
    ind.vars <- paste(ind.vars, collapse=" + ")
    formula <- as.formula(paste(dep.var, " ~ ", ind.vars))
    model <- randomForest(formula, data = DM.tmin,
                          importance=TRUE, keep.forest=TRUE)
    return(model)
}


checkCorrelations("DM", "tmin")
checkCorrelations("DM", "tmax")
## CHECK: elev, relelev_z, and relelev_watershed_minmax have high pearson
## correlations with PC2


###############################################################################
## DM TMIN
###############################################################################

## If we want tosplit into training and testing data:
## DM.tmin <- splitdf(PCAs[["DM"]][["tmin"]]$loadings)

DM.tmin <- PCAs[["DM"]][["tmin"]]$loadings
DM.tmin.mod <- fitRandomForest(DM.tmin, "PC1")
DM.tmin.mod
## what are the important variables (via permutation)
varImpPlot(DM.tmin.mod, type=1)
partialPlot(DM.tmin.mod, DM.tmin, "elev")
partialPlot(DM.tmin.mod, DM.tmin, "relelev_z")
partialPlot(DM.tmin.mod, DM.tmin, "relelev_shed")
partialPlot(DM.tmin.mod, DM.tmin, "radiation")
partialPlot(DM.tmin.mod, DM.tmin, "ldist_valley")

## Make the predicted loading surface
DM.tmin.predPC1 <- raster::predict(DM.topostack, DM.tmin.mod)
makeMap(DM.tmin.predPC1)
raster::writeRaster(DM.tmin.predPC1, file=file.path(data_output, "predPC1_tmin.tif"),
            overwrite=TRUE)

###############################################################################
## DM TMAX
###############################################################################
DM.tmax <- PCAs[["DM"]][["tmax"]]$loadings
DM.tmax.mod <- fitRandomForest(DM.tmax, "PC1")
DM.tmax.mod
## what are the important variables (via permutation)
varImpPlot(DM.tmax.mod, type=1)
partialPlot(DM.tmax.mod, DM.tmax, "elev")
partialPlot(DM.tmax.mod, DM.tmax, "relelev_z")
partialPlot(DM.tmax.mod, DM.tmax, "relelev_shed")
partialPlot(DM.tmax.mod, DM.tmax, "radiation")
partialPlot(DM.tmax.mod, DM.tmax, "ldist_valley")

## Make the predicted loading surface
DM.tmax.predPC1 <- raster::predict(DM.topostack, DM.tmax.mod)
makeMap(DM.tmax.predPC1)
raster::writeRaster(DM.tmax.predPC1, file=file.path(data_output, "predPC1_tmax.tif"),
            overwrite=TRUE)
