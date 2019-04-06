#Required Libraries
require("rgbif")
require("jsonlite")
require("httr")
require("dplyr")
require("rgdal")
require("raster")



#Wrapper function to access GBIF by name
getSpecies <- function(name, nameLevel,
                       limit = 10,
                       saveName = NA,
                       crsString="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                       gridReference = NA
)
{

  if(!(nameLevel %in% c('species', 'genus'))){
    stop("Need nameLevel to be either species or genus.")
  }
  
  #Identify the species / genus ide
  nameOptions <- rgbif::name_lookup(query=name, rank=nameLevel)
  nameOptions <- nameOptions$data
  nameOptinos <- nameOptions[order(nameOptions$key),]

  #Perform the pull using the specified parameters.
  # Call is appropriate to nameLEvel
  # must have longitude and latitude, and does not have a geospatial issue. 
  if(nameLevel == "species"){  
    
    id <- nameOptions$scientificName[1]
    dataGET <- occ_search(country="AU",
                            scientificName=c(id),
                            eventDate='2000,2018',
                            hasCoordinate=TRUE,
                            hasGeospatialIssue=FALSE,
                              limit=limit)
  }
  if(nameLevel == "genus") {
    
    id <- nameOptions$genusKey[1]
    dataGET <- occ_search(country="AU",
                          genus=c(id),
                          eventDate='2000,2018',
                          hasCoordinate=TRUE,
                          hasGeospatialIssue=FALSE,
                          limit=limit)
  }

  #Pick out the coordinates and return
  dataCoords <- dataGET$data %>%
    data.frame() %>%
    dplyr::select(latitude=decimalLatitude,
                  longitude=decimalLongitude)
  
  
  dataSpatial <- SpatialPointsDataFrame(coords=dataCoords,
                               data=dataCoords,
                                     proj4string = CRS(crsString))
  
  saveName <- gsub(" ", "", saveName)
  
  if(!is.na(saveName)){
    writeOGR(dataSpatial, dsn=saveName, layer=saveName, driver="ESRI Shapefile")
} else{
  
    return(dataSpatial)
  }
  
}
