#Load specified brick variables and build an SDMData object with the given species
buildSDMDataBrick <- function(speciesName, brickVars)
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
buildSDMDataLayer <- function(speciesName, brickVars)
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
