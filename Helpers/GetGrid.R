
getGrid <- function(left, right, bottom, top,
                    resolution,
                    crsString="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
{
  
  proj <- CRS(crsString)
  
  longitudes <- seq(left, right, by=resolution)
  latitudes <- seq(bottom, top, by=resolution)
  
  latLong <- expand.grid(longitudes, latitudes)
  
  grid.pts <- SpatialPointsDataFrame(coords=latLong,
                                     data=latLong,
                                     proj4string = proj)
  
  gridded(grid.pts) <- TRUE
  
  grid <- as(grid.pts, "SpatialPolygons")
  
  gridspdf <- SpatialPolygonsDataFrame(grid, data=data.frame(id=row.names(grid), 
                                                             row.names=row.names(grid)))
  
  
  
}