#Create SDM Data Objects, combining the mammal spatial points with the predictor sets

#Prepare workspace
rm(list=ls())
load("Natural/NaturalBrickPCA.RData")
source("SDMBuilders")

#Run PCA model
FlyingFoxData <- iterateSDM("Data/Species/FlyingFox/FlyingFox.shp", NaturalBrickPCA)


FlyingFoxModel <- sdm::sdm(Occurence ~ PC2 + PC6 + PC5 + PC1 + PC3 + PC8,
                     data=buildSDMData2("FlyingFox", NaturalBrickPCA),
                     methods=c("glm", "brt", "gam", "rpart", "cart", "fda", "glmnet"))
FlyingFoxPredictions <- ensemble(FlyingFoxModel,newdata=newPreds, setting=list(method='weighted',stat='AUC'))
writeRaster(FlyingFoxPredictions, "Analytics/PredictionsPCA.tif", overwrite=TRUE)

#Run only-flickr model
FlyingFoxModelFlickr <- sdm::sdm(Occurence ~ layer,
                                 data=buildSDMData3("FlyingFox", brick(flickr)),
                                methods=c("glm", "brt", "gam", "rpart", "cart", "fda", "glmnet"))
FlyingFoxPredictionsFlickr <- ensemble(FlyingFoxModelFlickr, newdata=brick(flickr), setting=list(method='weighted',stat='AUC'))
writeRaster(FlyingFoxPredictionsFlickr, "Analytics/PredictionsFlickr.tif", overwrite=TRUE)



