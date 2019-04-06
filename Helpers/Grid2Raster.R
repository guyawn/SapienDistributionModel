#Grid data for 
example <- shapefile("Data/Reference/Grid/Grid_0.1.shp")
gridCRS <- crs(example)

#Convert grid cell data to a raster 
grid2raster <- function(gridData, values=gridData[,5], res = 0.1)
{
  
  #Create an empty raster at the right dimension
  output <- raster(xmn=min(gridData$left),
                   xmx= max(gridData$right),
                   ymn=min(gridData$bottom),
                   ymx=max(gridData$top),
                   resolution=c(res, res))
  output[] <- 0
  
  #Go through each cell in the csv
  for(x in 1:nrow(gridData)){
    
    iDatum <- gridData[x,]
    
    #Find the center of the grid cell
    coordCenter <- SpatialPoints(cbind(mean(c(iDatum$left, iDatum$right)),
                                       mean(c(iDatum$top, iDatum$bottom))
    ))
    #Change the relevant cell in the grid to the value there.
    output[cellFromXY(output, coordCenter)] <- values[x]
  }
  
  crs(output) <- gridCRS
  return(output)
}
