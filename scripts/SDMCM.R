## save script for reference. But DO NOT USE. All SDM code will be moved to
## CSC-sky-island-forest repo. Thgis script does not run, it refers to
## nonexistant distirbution files (Feb 2017 versions rather than updated April
## 2017 versions).

# Base script by H. Poulos

library(plotKML)
library(sp)
library(maptools)
library(spatstat)
library(raster)
library(rgdal)
CM19612000 <- readRDS("CM____19612000.rds")
CM19612000<-data.frame(CM19612000)
coordinates(CM19612000) <- ~x+y

CM19612000 <- as(CM19612000, "SpatialPixelsDataFrame")
str(CM19612000)
#if you want to plot object spplot(CM19612000)

#make the various BIO rasters for CM19162000
BIO1<-raster(CM19612000, layer=1)
summary(BIO1)

BIO2<-raster(CM19612000, layer=2)
summary(BIO2)

BIO3<-raster(CM19612000, layer=3)
summary(BIO3)

BIO4<-raster(CM19612000, layer=4)
summary(BIO4)

BIO5<-raster(CM19612000, layer=5)
summary(BIO5)

BIO6<-raster(CM19612000, layer=6)
summary(BIO6)

BIO7<-raster(CM19612000, layer=7)
summary(BIO7)

BIO8<-raster(CM19612000, layer=8)
summary(BIO8)

BIO9<-raster(CM19612000, layer=9)
summary(BIO9)

BIO10<-raster(CM19612000, layer=10)
summary(BIO10)

BIO11<-raster(CM19612000, layer=11)
summary(BIO11)

march_may_min<-raster(CM19612000, layer=12)
summary(march_may_min)

predictors<-stack(BIO1,BIO2,BIO3,BIO4,BIO5,BIO6,BIO7,BIO8,BIO9,BIO10,BIO11,march_may_min)
names(predictors)
plot(predictors)

library(dismo)

#bring in species
QUGR3 <-read.csv("QUGR3.csv", header=TRUE)
QUGR3 <- data.frame(QUGR3)
head(QUGR3)
dim(QUGR3)
QUGR3latlon<-data.frame(QUGR3$long,QUGR3$lat)
presvals<-extract(predictors, QUGR3latlon)

#generate background points from within the mask of BIO1 and plot them
set.seed(1963)
mask<-BIO1
bg <- randomPoints(mask, 100)
par(mfrow=c(1,2))
plot(!is.na(mask), legend=FALSE)
points(bg, cex=0.5)

absvals<-extract(predictors, bg)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))

#SDM models want names in text. Won't work with as.factor specification

sdmdata$pb<- as.factor(sdmdata$pb)
levels(sdmdata$pb) <- gsub("0", "absent", levels(sdmdata$pb))
levels(sdmdata$pb) <- gsub("1", "present", levels(sdmdata$pb))
head(sdmdata)

# pairs plot of the values of the climate data
# at the species occurrence sites.
  
pairs(sdmdata[,2:5], cex=0.1, fig=TRUE)


# Fit xgboost: model with 5-fold cross-validation
library(caret)
library(plyr)
library(xgboost)

#control set with 5 fold cv and 1 repeat for speed right now. Should change repeats to betweeen 3 and 5 later
control <- trainControl(method="repeatedcv", number=5, repeats=1)
seed <- 7
metric <- "Accuracy"

set.seed(seed)
boost <- train(as.factor(pb)~., data=sdmdata, method = 'xgbTree', metric=metric, trControl=control)
# Print model to console
boost

# Plot model
plot(boost)

# variable importance
boostImp <- varImp(boost, scale=TRUE)
boostImp

#make the SDM
Boost <- predict(predictors, boost)
plot(Boost)



# SVM model--support vector machines with radial basis function
library(kernlab)

svm <- train(as.factor(pb)~., data=sdmdata, method = 'svmRadial', metric=metric, trControl=control)
# Print model to console
svm
# Plot model
plot(svm)

# variable importance
svmImp <- varImp(svm, scale=TRUE)
svmImp

#make the SDM
SVM <- predict(predictors, svm)
plot(SVM)

# random forest
library(randomForest)

rf <- train(as.factor(pb)~., data=sdmdata,  method = 'rf', metric=metric, trControl=control)
# Print model to console
rf

# Plot model
plot(rf)

# variable importance
rfImp <- varImp(rf, scale=TRUE)
rfImp

#make the SDM
RF <- predict(predictors, rf)
plot(RF)

#compare accuracy among models
resamps <- resamples(list(rf = rf, boost = boost, svm=svm))
summary(resamps)
print(resamps)
diffs <- diff(resamps)
summary(diffs)
print(diffs)

#reclassify rasters so that grids have binary values of 0 for absent or 1 for present
# reclassify the values into three groups
boostbin <- reclassify(Boost, c(0,1.98,0,1.99,2,1))
svmbin <- reclassify(SVM, c(0,1.98,0,1.99,2,1))
rfbin <- reclassify(RF, c(0,1.98,0,1.99,2,1))

#make the ensemble model: values of 0--no model predicts presence, 1--1 model predicts presence, etc.

ensemble<-boostbin+svmbin+rfbin
plot(ensemble)

writeRaster(ensemble, filename = "ensemble.tif", format="GTiff", overwrite=TRUE)

