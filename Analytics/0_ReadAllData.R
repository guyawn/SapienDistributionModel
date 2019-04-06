#Load helpers
source("Helpers/Grid2Raster.R")

#Load flickr data and convert to a raster file.
# Count of images returned by the flickr geosearch API in 0.1 x 0.1 degree cells
# Log-scaled after a laplace correction
flickr <- read.csv("Data/Human/Flickr/flickr.csv", stringsAsFactors=FALSE)
names(flickr) <- tolower(names(flickr))
flickr <- grid2raster(flickr, log(flickr$photocount + 1))

#Load Eucalyptus data and convert to a raster file
eucalyptus <- read.csv("Data/Species/Eucalyptus/eucalyptus-Counts.csv", stringsAsFactors = FALSE)
names(eucalyptus) <- tolower(names(eucalyptus))
eucalyptus$numpoints <- as.numeric(gsub(",", "",eucalyptus$numpoints))
eucalyptus <- grid2raster(eucalyptus, log(eucalyptus$numpoints+1))
eucalyptus <- resample(eucalyptus, flickr)

#Load Acacia data and convert to a raster file
acacia <- read.csv("Data/Species/Acacia/acacia-Counts.csv", stringsAsFactors = FALSE)
names(acacia) <- tolower(names(acacia))
acacia$numpoints <- as.numeric(gsub(",", "",acacia$numpoints))
acacia <- grid2raster(acacia, log(acacia$numpoints+1))
acacia <- resample(acacia, flickr)

#Load building data and convert to a raster site
# Percent of area in a 0.1 x 0.1 degree cell that is covered by buildings
# Log-scaled after a laplace correction
buildings <- read.csv("Data/OSM/Buildings/area10_au-buildings multipolygons.csv", stringsAsFactors=FALSE) %>%
   group_by(left, top, right, bottom) %>%
   summarise(buildingarea = sum(area))
buildings <- grid2raster(buildings, log((buildings$buildingarea*10) + 1))
buildings <- resample(buildings, flickr)

#Read the standardized raster file, resample its layers to the flickr data and 
# put it back together
mybrick <- brick("Data/Natural/Bioclim/layers_brick_full-25.tif")
crs(mybrick) <- gridCRS
brickList <- sapply(1:length(names(mybrick)),
                      function(x){
                        resample(subset(mybrick, x, drop=TRUE), flickr)
                      })

#Read and combine the CSIRO Data
load("Data/Natural/CSIRO/CSIROBrick.RData")
crs(CSIRObrick) <- gridCRS
csiroList <- sapply(1:length(names(CSIRObrick)),
                    function(x){
                      resample(subset(CSIRObrick, x, drop=TRUE), flickr)
                    })

#Combine all natural data into a single raster brick
naturalList <- c(brickList, csiroList, acacia, eucalyptus)
naturalBrick <- do.call(brick, naturalList)
names(naturalBrick) <- c("alt","biome","ndvi","landuse","solar_ann", "evap_ann",
                    "bio_1","bio_2","bio_3","bio_4","bio_5","bio_6","bio_7","bio_8",
                    "bio_9","bio_10","bio_11","bio_12","bio_13","bio_14","bio_15",
                    "bio_16","bio_17","bio_18","bio_19", names(CSIRObrick), "acacia", "eucalyptus")
save(naturalBrick, file="Analytics/NaturalBrick.RData")

#Perform PCA on the raster brick
naturalPCA <- rasterPCA(naturalBrick)
vars <- naturalPCA$model$sdev
cumVars <- cumsum(vars / sum(vars))
naturalPCABrick <- naturalPCA$map
naturalPCABrick <- subset(naturalPCABrick, c(1:which(cumVars > 0.95)[1]))
save(naturalPCABrick, file="Analytics/NaturalPCABrick.RData")

#Add the flickr data in to the raster brick
naturalFlickrBrick <- addLayer(naturalBrick, flickr)
naturalPCAFlickrBrick <- addLayer(naturalPCABrick, flickr)

save(naturalFlickrBrick, file="Analytics/NaturalFlickrBrick.RData")
save(naturalFlickrBrick, file="Analytics/NaturalPCAFlickrBrick.RData")
