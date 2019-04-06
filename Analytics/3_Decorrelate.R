#Load helpers
rm(list=ls())
source("Helpers/RangeScaleRaster.R")
source("Helpers/Grid2Raster.R")

#Load Grid Data and sort by order
grid <- shapefile("Data/Reference/Grid/Grid_0.1.shp")
grid$id <- as.numeric(grid$id)
grid <- grid[order(grid$id),]

#Load flickr data
flickr <- read.csv("Data/Human/Flickr/flickr.csv", stringsAsFactors=FALSE)
names(flickr) <- tolower(names(flickr))
flickr <- grid2raster(flickr, log(flickr$photocount + 1))
save(flickr, file="Analytics/flickr.RData")

#Load the natural data
load("Analytics/NaturalBrick.RData")

#Generate points from the flickr raster
n<-5000
occ = data.frame(Occurence = rep(1, n))
flickrVals <- 
flickr[which(flickr[1:length(flickr)])] <- 0.000000000000000000000001
flickrOcc <- SpatialPointsDataFrame(randomPoints(flickr, n, prob=TRUE),
                                    data=occ,
                                    proj4string=crs(naturalBrick))

#Limit the generated points to those in the grid
keepIndices <- over(x=as(flickrOcc, "SpatialPoints"), y=as(grid,"SpatialPolygons"))
flickrOcc <- flickrOcc[which(!is.na(keepIndices)),]


#Add randomly generated background points
bg<-SpatialPointsDataFrame(randomPoints(naturalBrick, length(flickrOcc)), 
                           data=data.frame(Occurence=rep(0, length(flickrOcc))))
crs(bg) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
flickrOcc <- rbind(flickrOcc, bg)

#Crop the predictors to the extent of the flickr data.
naturalBrick <- crop(naturalBrick, extent(flickrOcc))

#Split data into train / test sets
n <- length(flickrOcc)
randomIndices <- sample(1:n)
train <- randomIndices[1:(floor(0.6 * n))]
test <- randomIndices[(ceiling(0.6*n)):n]
train <- flickrOcc[train,]
test <- flickrOcc[test,]

#Create a SDM ensemble and predict the flickr data from the natural data. Range scale and save the predictions
flickrSDMData <- sdm::sdmData(Occurence~., train, test, predictors=naturalBrick)
flickrModel <- sdm::sdm(Occurence ~.,
                        data=flickrSDMData,
                        methods=c("glm", "glmnet", "brt", "rpart")) #, "brt", "gam", "rpart", "cart", "fda", "glmnet"))
flickrPredictions <- ensemble(flickrModel,  newdata=naturalBrick, setting=list(method='weighted',stat='AUC'))
flickrPredictions <- resample(flickrPredictions, flickr)
flickrPredictions[which(is.na(flickrPredictions[1:length(flickrPredictions)]))] <- min(flickrPredictions[1:length(flickrPredictions)],
                                                                                       na.rm=TRUE)
save(flickrPredictions, file="Analytics/flickrPredictions.RData")
save(flickrModel, file="Analytics/flickrModel.RData")

#Find the residuals from the prediction 
flickrDecorrelated <- flickr - flickrPredictions
save(flickrDecorrelated, file="Analytics/flickrDecorrelated.RData")


