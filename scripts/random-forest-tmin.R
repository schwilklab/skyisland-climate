# random-forest.R
#
# Code to fit random forest models
library(randomForest)
library(raster)
library(sp)
library(maptools) # for readASCIIGrid

# get the PCA scores and loadings. Function called below returns list of PCA
# objects, one for each mtn range. Use `force=TRUE` to rerun the PCAs,
# otherwise the loadings and scores are read from a data object saved in the
# results/tempdata directory.
source("./microclimate-topo-PCA.R")
PCAs <- loadPCAData()
tmin <- PCAs[["DM"]]$tmin$loadings # grab DM dta only for testing. TODO: fix
names(tmin)
tmin <- data.frame(tmin)
tmin2 <- tmin[ -c(1:7,22:25) ]
names(tmin2)

#find corelated variables for PC1
cor(tmin2)
#elev, relelev_z, and relelev_watershed_minmax have high pearson correlations with PC1

#find corelated variables for PC2

tmin3 <- tmin[ -c(1:7,21,23:25) ]
cor(tmin3)
#same vars as PC1, but with opposit signs
#same vars as PC1, but with opposite signs


## divide into training and test data: The importance of this is debatable
## because RF uses both boosting and bagging. There are a lot of online discussion
##boards out there debating this, and I really haven't used this in prior models because
## RF already gives accuracy statistics.

splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/2))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}
#apply the function
splits <- splitdf(tmin2, seed=808)

#it returns a list - two data frames called trainset and testset
str(splits)

# there are 18 observations in each data frame
lapply(splits,nrow)

#view the first few columns in each data frame
lapply(splits,head)

# save the training and testing sets as data frames
training <- splits$trainset
testing <- splits$testset
summary(training)
summary(testing)
library(randomForest)

#fit the randomforest model
model <- randomForest(PC1~., data = training, importance=TRUE,keep.forest=TRUE)
print(model)
#what are the important variables (via permutation)
varImpPlot(model, type=1)
partialPlot(model, training, "elev")
partialPlot(model, training, "relelev_z")
partialPlot(model, training, "relelev_watershed_minmax")
partialPlot(model, training, "radiation")
partialPlot(model, training, "ldist_tovalley")
# load ascii grids
source("load_grids.R")

# this makes the predicted loading surface
predPC1 <- predict(topostack, model, type="response")
plot(predPC1)
writeRaster(predPC1, file=file.path(data_output, "predPC1_tmin.tif"), overwrite=TRUE)


#now for PC2
## divide into training and test data: The importance of this is debatable
## because RF uses both boosting and bagging. There are a lot of online discussion
##boards out there debating this, and I really haven't used this in prior models because
## RF already gives accuracy statistics.

#apply the function
splitspc2 <- splitdf(tmin3, seed=808)

#it returns a list - two data frames called trainset and testset
str(splitspc2)

# there are 18 observations in each data frame
lapply(splitspc2,nrow)

#view the first few columns in each data frame
lapply(splitspc2,head)


# save the training and testing sets as data frames
trainingpc2 <- splitspc2$trainset
testingpc2 <- splitspc2$testset
summary(trainingpc2)
summary(testingpc2)

#fit the randomforest model
model2 <- randomForest(PC2~., data = trainingpc2, importance=TRUE,keep.forest=TRUE)
print(model2)
#what are the important variables (via permutation)
varImpPlot(model2, type=1)
partialPlot(model2, training, "elev")
partialPlot(model2, training, "relelev_z")
partialPlot(model2, training, "relelev_watershed_minmax")
partialPlot(model2, training, "radiation")
partialPlot(model2, training, "zdist_valley")
# load ascii grids
source("load_grids.R")

# this makes the predicted loading surface
predPC2 <- predict(topostack, model2, type="response")
plot(predPC2)
writeRaster(predPC2, file=file.path(data_output, "predPC2_tmin.tif"), overwrite=TRUE)
