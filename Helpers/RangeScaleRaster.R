rangeScaleRaster <- function(raster){
  
  rasterVals <- raster[1:length(raster)]
  raster[is.na(rasterVals)] <- min(rasterVals, na.rm=TRUE)
  
  rasterVals <- raster[1:length(raster)]
  raster[1:length(raster)] <- rasterVals - 
    min(rasterVals)
  
  rasterVals <- raster[1:length(raster)]
  raster[1:length(raster)] <- rasterVals /
    max(rasterVals)
  
  return(raster)
  
}