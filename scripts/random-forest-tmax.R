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
tmax <- PCAs[["DM"]]$tmax$loadings # grab DM dta only for testing. TODO: fix
names(tmax)
tmax <- data.frame(tmax)
tmax2 <- tmax[ -c(1:7,22:25) ]
names(tmax2)

#find corelated variables for PC1
cor(tmax2)
#radiation only

#find corelated variables for PC2

tmax3 <- tmax[ -c(1:7,21,23:25) ]
cor(tmax3)
#radiation and specific_catchment.area

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
splits <- splitdf(tmax2, seed=808)

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

#fit the randomforest model...this results in an absolutely terrible fit...
tmaxpc1_model <- randomForest(PC1~., data = training, importance=TRUE,keep.forest=TRUE)
print(tmaxpc1_model)
#what are the important variables (via permutation)
varImpPlot(tmaxpc1_model, type=1)
# load ascii grids
source("load_grids.R")

# this makes the predicted loading surface
predtmaxPC1 <- predict(topostack, tmaxpc1_model, type="response")
plot(predtmaxPC1)
writeRaster(predtmaxPC1, file=file.path(data_output, "predPC1_tmax.tif"), overwrite=TRUE)


#now for PC2
## divide into training and test data: The importance of this is debatable
## because RF uses both boosting and bagging. There are a lot of online discussion
##boards out there debating this, and I really haven't used this in prior models because
## RF already gives accuracy statistics.

#apply the function
splitspc2 <- splitdf(tmax3, seed=808)

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

#fit the randomforest model----better than PC1, but still shitty
tmaxpc2_model <- randomForest(PC2~., data = trainingpc2, importance=TRUE,keep.forest=TRUE)
print(tmaxpc2_model)
#what are the important variables (via permutation)
varImpPlot(tmaxpc2_model, type=1)
# load ascii grids
source("load_grids.R")

# this makes the predicted loading surface
predtmaxPC2 <- predict(topostack, tmaxpc2_model, type="response")
plot(predtmaxPC2)
writeRaster(predtmaxPC2, file=file.path(data_output, "predPC2_tmax.tif"), overwrite=TRUE)
