## random-forest.R
## Code to fit random forest models

## Random number seed
RSEED = 808
set.seed(RSEED)

source("./microclimate-topo-PCA.R") # provides PCAs object

TOPO_RES_DIR <- "../results/topo_mod_results/"

## IND_VAR_NAMES <-  c("elev","ldist_ridge" , "ldist_valley",  "msd", "radiation","relev_l", "slope",
##                  "zdist_ridge",   "zdist_valley")

IND_VAR_NAMES <-  c("elev","ldist_ridge" , "ldist_valley",  "msd", "radiation","relev_l", "slope")

library(randomForest)
library(sp)
library(dplyr)
library(GGally) # for ggpairs()

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
    g <- ggpairs(df)
    print(g)
    dev.off()
    
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
  print(paste(mtn, dep.var, axis))
  mod <- fitRandomForest(PCAs[[mtn]][[dep.var]]$loadings, axis)
  print(mod)
  png(file = file.path(TOPO_RES_DIR, paste(mtn, "_", dep.var, "_", axis, ".png", sep="")))
  varImpPlot(mod, type=1)
  dev.off()
  res <- raster::predict(topostacks[[mtn]], mod)
  return(res)
}

  
  ## partialPlot(DM.tmin.mod, DM.tmin, "elev")
  ## partialPlot(DM.tmin.mod, DM.tmin, "relev_l")
  ## partialPlot(DM.tmin.mod, DM.tmin, "ldist_valley")
  ## partialPlot(DM.tmin.mod, DM.tmin, "radiation")
  ## partialPlot(DM.tmin.mod, DM.tmin, "ldist_valley")


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