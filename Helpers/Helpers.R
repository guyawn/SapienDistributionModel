#Load specified brick variables and build an SDMData object with the given species
buildSDMData <- function(speciesName, myBrickVars)
{
  
  #Pick out just the specified levels from mybrick
  preds0 <- do.call(brick, sapply(myBrickVars, function(x){
    subset(mybrick, x, pull=TRUE)
  }))
  
  #Pick out both the above, and combine them with flickr / building
  preds1 <- do.call(brick, c(sapply(myBrickVars, function(x){
    subset(mybrick, x, pull=TRUE)
  }), flickr, buildings))
  
  
  #Load the species points and set the occurence field to 1
  speciesCurrent <- shapefile(paste0(speciesName, "/", speciesName, ".shp"))
  for(j in 1:ncol(speciesCurrent))
  {
    speciesCurrent@data[,1] <- NULL
  }
  speciesCurrent$Occurence <- 1
    
  #Convert it to a sptial points data frame and crop it to the same dimensions
  #as the predictor raster
  bg<-SpatialPointsDataFrame(randomPoints(preds0, length(speciesCurrent)), 
                             data=data.frame(Occurence=rep(0, length(speciesCurrent))))
  crs(bg) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  speciesCurrent <- rbind(speciesCurrent, bg)
  crop(speciesCurrent, extent(preds0))
    
  #Split data into train / test sets
  n <- length(speciesCurrent)
  randomIndices <- sample(1:n)
  train <- randomIndices[1:(floor(0.6 * n))]
  test <- randomIndices[(ceiling(0.6*n)):n]
  train <- speciesCurrent[train,]
  test <- speciesCurrent[test,]
  
  #Return two created SDM data sets, one with each set of predictors  
  return(list(
    sdm::sdmData(Occurence~., train, test, predictors=preds0),
    sdm::sdmData(Occurence~., train, test, predictors=preds1)
    ))
  
}

#Load specified brick variables and build an SDMData object with the given species
buildSDMData2 <- function(speciesName, brickVars)
{
 
  #Load the species points and set the occurence field to 1
  speciesCurrent <- shapefile(paste0(speciesName, "/", speciesName, ".shp"))
  for(j in 1:ncol(speciesCurrent))
  {
    speciesCurrent@data[,1] <- NULL
  }
  speciesCurrent$Occurence <- 1
  
  #Convert it to a sptial points data frame and crop it to the same dimensions
  #as the predictor raster
  bg<-SpatialPointsDataFrame(randomPoints(brickVars, length(speciesCurrent)), 
                             data=data.frame(Occurence=rep(0, length(speciesCurrent))))
  crs(bg) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  speciesCurrent <- rbind(speciesCurrent, bg)
  crop(speciesCurrent, extent(brickVars))
  
  #Split data into train / test sets
  n <- length(speciesCurrent)
  randomIndices <- sample(1:n)
  train <- randomIndices[1:(floor(0.6 * n))]
  test <- randomIndices[(ceiling(0.6*n)):n]
  train <- speciesCurrent[train,]
  test <- speciesCurrent[test,]
  
  #Return two created SDM data sets, one with each set of predictors  
  return(sdm::sdmData(Occurence~., train, test, predictors=brickVars))
  
}

#Load specified brick variables and build an SDMData object with the given species
buildSDMData3 <- function(speciesName, brickVars)
{
  
  #Load the species points and set the occurence field to 1
  speciesCurrent <- shapefile(paste0(speciesName, "/", speciesName, ".shp"))
  for(j in 1:ncol(speciesCurrent))
  {
    speciesCurrent@data[,1] <- NULL
  }
  speciesCurrent$Occurence <- 1
  
  #Convert it to a sptial points data frame and crop it to the same dimensions
  #as the predictor raster
  bg<-SpatialPointsDataFrame(randomPoints(brickVars, length(speciesCurrent)), 
                             data=data.frame(Occurence=rep(0, length(speciesCurrent))))
  crs(bg) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  speciesCurrent <- rbind(speciesCurrent, bg)
  crop(speciesCurrent, extent(brickVars))
  
  #Split data into train / test sets
  n <- length(speciesCurrent)
  randomIndices <- sample(1:n)
  train <- randomIndices[1:(floor(0.6 * n))]
  test <- randomIndices[(ceiling(0.6*n)):n]
  train <- speciesCurrent[train,]
  test <- speciesCurrent[test,]
  
  #Return two created SDM data sets, one with each set of predictors  
  return(sdm::sdmData(Occurence~layer, train, test, predictors=brickVars))
  
}


#Load specified brick variables and build an SDMData object with the given species
iterateSDM <- function(speciesFile, predictorsRaster)
{
  
  #Pick out both the above, and combine them with flickr / building
  predictorNames <- names(predictorsRaster)
  predictorIndices <- 1:length(predictorNames)
  
  #Get the data for the specified species
  speciesCurrent <- shapefile(speciesFile)     #paste0(speciesName, "/", speciesName, ".shp"))
  for(j in 1:ncol(speciesCurrent))
  {
    speciesCurrent@data[,1] <- NULL
  }
  speciesCurrent$Occurence <- 1
  
  #Convert it to a sptial points data frame and crop it to the same dimensions
  #as the predictor raster
  bg<-SpatialPointsDataFrame(randomPoints(predictorsRaster, length(speciesCurrent)), 
                             data=data.frame(Occurence=rep(0, length(speciesCurrent))))
  crs(bg) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  speciesCurrent <- rbind(speciesCurrent, bg)
  speciesCurrent <- crop(speciesCurrent, extent(predictorsRaster))
  
  #Split data into train / test sets
  n <- length(speciesCurrent)
  randomIndices <- sample(1:n)
  train <- randomIndices[1:floor(0.4 * n)]
  test <- randomIndices[(floor(0.4*n)+1):floor(0.7*n)]
  validate <- randomIndices[(floor(0.7*n)+1):n]
  train <- speciesCurrent[train,]
  test <- speciesCurrent[test,]
  validate <- speciesCurrent[validate,]
  
  #Always include e
  currentIndices <- NULL
  currentNames <- predictorNames[currentIndices]
  currentPreds <- brick(subset(predictorsRaster,currentIndices,pull=TRUE))
  
  #Starting set of predictors
  currentAUC <- 0
  bestNewAUC <- currentAUC + 0.000001

  
  
  while(bestNewAUC > currentAUC)
  {
    
    currentAUC <- bestNewAUC
    bestNewAUC <- 0
    bestNewIndices <- c()
    bestNewNames <- c()
    
    
    for(i in setdiff(predictorIndices, currentIndices)) {
      
      newIndices <- c(currentIndices, i)
      newNames <- predictorNames[newIndices]
      newPreds <- do.call(brick, c(sapply(newIndices, function(x){
        subset(predictorsRaster, x, pull=TRUE)
      })))
      
      model <- paste0("Occurence ~ ", paste(newNames, collapse=" + "))
      cat(paste0("Trying ", model, "\n"))
      newData <- sdm::sdmData(formula(model), train, test, predictors=newPreds)
      newModel <- sdm::sdm(formula(model),
                               data=newData,
                               methods=c("glm", "brt", "gam", "rpart", "cart", "fda", "glmnet"))
      
      results <- capture.output(newModel)
      results <- results[28:34]
      aucs <- gsub(".*:     (0...).*", "\\1", results)
      aucs <- aucs[which(nchar(aucs) ==4)]
      newAUC <- mean(as.numeric(aucs))
      
      if(newAUC > bestNewAUC){
        bestNewAUC <- newAUC
        bestNewIndices <- newIndices  
        bestNewNames <-newNames 
      }
      
    }
    
    if(bestNewAUC > currentAUC)
    {
      currentNames <- bestNewNames
      currentIndices <- bestNewIndices
    }
      

  }
  
  
  #Return two created SDM data sets, one with each set of predictors  
  return(list(
    currentNames,
    currentIndices,
    currentAUC
  ))
  
}
