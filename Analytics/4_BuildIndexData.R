#Clear Workspace
rm(list=ls())
source("Helpers/Grid2Raster.R")

#Load Grid Data and sort by order
grid <- shapefile("Data/Reference/Grid/Grid_0.1.shp")
grid$id <- as.numeric(grid$id)
grid <- grid[order(grid$id),]

#Limit grid data to eastern Australia, for easer of testing
#grid <- (grid[which(grid$right > 150),])

#Load flickr data
load("Analytics/flickr.RData")
load("Analytics/flickrDecorrelated.RData")

#Add flickr data to grid
grid$flickrScore <- unlist(extract(flickr, grid))
grid$flickrDecorrelatedScore <- unlist(extract(flickrDecorrelated, grid))

#Read species data
flyingFox <- shapefile("Data/Species/FlyingFox/FlyingFox.shp")
kangaroo <- shapefile("Data/Species/Kangaroo/Kangaroo.shp")
koala <- shapefile("Data/Species/Koala/Koala.shp")
wombat <- shapefile("Data/Species/Wombat/Wombat.shp")

                                #Downsample the more populous species to allow poly.counts to proceed efficiently
kangaroo <- kangaroo[sample(1:length(kangaroo), 15000),]
koala <- koala[sample(1:length(koala), 15000),]
wombat <- wombat[sample(1:length(wombat), 15000),]

#Add counts of mammal data
grid$FlyingFox <- poly.counts(flyingFox, grid)
grid$Kangaroo <- poly.counts(kangaroo, grid)
grid$Koala <- poly.counts(koala, grid)
grid$Wombat <- poly.counts(wombat, grid)


#Add some PCs 
load("Analytics/NaturalBrickPCA.RData")
grid$pc1 <- unlist(extract(subset(NaturalBrickPCA, 1), grid))
grid$pc2 <- unlist(extract(subset(NaturalBrickPCA, 2), grid))
grid$pc3 <- unlist(extract(subset(NaturalBrickPCA, 3), grid))
grid$pc4 <- unlist(extract(subset(NaturalBrickPCA, 4), grid))
grid$pc5 <- unlist(extract(subset(NaturalBrickPCA, 5), grid))
grid$pc6 <- unlist(extract(subset(NaturalBrickPCA, 6), grid))


# Plain correlation
cor(grid$flickrScore, grid$FlyingFox)
cor(grid$flickrScore, grid$Koala)
cor(grid$flickrScore, grid$Kangaroo)
cor(grid$flickrScore, grid$Wombat)

# Adjusted correlation
cor(grid$flickrDecorrelatedScore, grid$FlyingFox, use="complete.obs")
cor(grid$flickrDecorrelatedScore, grid$Koala, use="complete.obs")
cor(grid$flickrDecorrelatedScore, grid$Kangaroo, use="complete.obs")
cor(grid$flickrDecorrelatedScore, grid$Wombat, use="complete.obs")

# Best PC Correlations
cor(grid$pc6, grid$FlyingFox, use="complete.obs")
cor(grid$pc1, grid$Koala, use="complete.obs")
cor(grid$pc1, grid$Kangaroo, use="complete.obs")
cor(grid$pc1, grid$Wombat, use="complete.obs")


#Predictive Power of the flickr and decorrelated score
oneModel <- function(speciesShape, predRaster){
  
  speciesShape$Occurence <- 1
  speciesShape <- speciesShape[,-c(1:2)]
  bg<-SpatialPointsDataFrame(randomPoints(predRaster, length(speciesShape)), 
                             data=data.frame(Occurence=rep(0, length(speciesShape))))
  crs(bg) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  speciesShape <- rbind(speciesShape, bg)
  
   
  n <- length(speciesShape)
  randomIndices <- sample(1:n)
  train <- randomIndices[1:(floor(0.6 * n))]
  test <- randomIndices[(ceiling(0.6*n)):n]
  train <- speciesShape[train,]
  test <- speciesShape[test,]
  
  flickrSDMData <- sdm::sdmData(Occurence~layer, train, test, predictors=predRaster)
  flickrModel <- sdm::sdm(Occurence ~layer,
                          data=flickrSDMData,
                          methods=c("glm", "glmnet", "brt", "rpart"))
  flickrModel
}

#Predictive Power of the flickr and decorrelated score
multModel <- function(speciesShape, predRaster){
  
  speciesShape$Occurence <- 1
  speciesShape <- speciesShape[,-c(1:2)]
  bg<-SpatialPointsDataFrame(randomPoints(predRaster, length(speciesShape)), 
                             data=data.frame(Occurence=rep(0, length(speciesShape))))
  crs(bg) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  speciesShape <- rbind(speciesShape, bg)
  
  
  n <- length(speciesShape)
  randomIndices <- sample(1:n)
  train <- randomIndices[1:(floor(0.6 * n))]
  test <- randomIndices[(ceiling(0.6*n)):n]
  train <- speciesShape[train,]
  test <- speciesShape[test,]
  
  flickrSDMData <- sdm::sdmData(Occurence~.,train, test, predictors=predRaster)
  flickrModel <- sdm::sdm(Occurence ~.,
                          data=flickrSDMData,
                          methods=c("glm", "glmnet", "brt", "rpart"))
  flickrModel
}

oneModel(flyingFox, flickr)
oneModel(koala, flickr)
oneModel(kangaroo, flickr)
oneModel(wombat, flickr)

oneModel(flyingFox, flickrDecorrelated)
oneModel(koala, flickrDecorrelated)
oneModel(kangaroo, flickrDecorrelated)
oneModel(wombat, flickrDecorrelated)

multModel(flyingFox, NaturalBrickPCA)
multModel(koala, NaturalBrickPCA)
multModel(kangaroo, NaturalBrickPCA)
multModel(wombat, NaturalBrickPCA)

#Write the indexing data to file
grid <- indexData
save(indexData, file="Analytics/indexData.RData")



#Distance from a "flickr cell" to check
threshold <- 1
cellWithinThresholdOfRoad <- c() 
cellWithFlyingFox <- c()


#Load helper functions
source("Helpers/ExpandGridCell.R")

#Go through each grid cell
for(i in 1:length(grid)){
  polygon <- grid[i,]
  print(i)
  
  #Track if a flickr neighbor was found
  found <- FALSE 
  
  #Start looking in the current cell
  searchIn <- polygon
  
  #If the cell has a flickr score, save it as within the treshold and 
  # Skip the rest
  if(searchIn$flickrScore > 0){
    cellWithinThresholdOfRoad <-  c(cellWithinThresholdOfRoad, 1)
    cellWithFlyingFox <- c(cellWithFlyingFox, polygon$FlyingFox > 0)
    found <- TRUE
  } else{
    
    #Set the maximum distance from a cell to look in
    currentDistance <- 0
    while(currentDistance < threshold){
      
      #Start with an empty set of new search points
      newSearchIn <- searchIn[0,]
      
      #For each cell in the current search space, add all surrounding cells
      for(i in 1:nrow(searchIn)){
        newSearchIn <- rbind(newSearchIn, searchIn[i,], expandGridCell(grid, searchIn[i,]))
      }
      
      #Filter out duplicates
      newSearchIn <- newSearchIn[!duplicated(newSearchIn$id),]
      
      #If any of the newly added cells contain a flickr cell, stop searching.
      #Save teh cell as withn that many of the road. 
      #If the cell contains a flying fox, record that.
      if(any(newSearchIn$flickrScore > 0)){
        cellWithinThresholdOfRoad <-  c(cellWithinThresholdOfRoad, 1)
        found <- TRUE
        cellWithFlyingFox <- c(cellWithFlyingFox, polygon$FlyingFox > 0)
        break
      }
      
      #The newsly identified cells are now the ones to look into
      searchIn <- newSearchIn
      
      #Idecrease teh distance you are
      currentDistance <- currentDistance + 1
    }
  }
  
  #If none was found after trying all poisslbe distances, quit out.
  if(!found) cellWithinThresholdOfRoad <-  c(cellWithinThresholdOfRoad, 0)
}
  
# Threshold of 2 - Z-score with Binomial Approximation
pD <- mean(cellWithinThresholdOfRoad)  
pF <- mean(grid$FlyingFox > 0)
pR <- mean(cellWithFlyingFox)
N <- length(grid)
tScoreFlickr <- (N*pR - (N*pF * pD)) / sqrt(N * (pD*pF)* (1-(pD*pF)))



threshold <- 1
flickrMA <- flickrDecorrelated

for(i in 1:nrow(flickrDecorrelated)){
  print(i)
  for(j in 1:ncol(flickrDecorrelated)){
    
    xIndices <- (i-threshold):(i+threshold)
    yIndices <- (j-threshold):(j+threshold)
    
    flickrMA[i,j] <- mean(flickrDecorrelated[xIndices, yIndices], na.rm=TRUE)
  }
}

grid$flickrMAScore <- unlist(extract(flickrMA, grid))


  






