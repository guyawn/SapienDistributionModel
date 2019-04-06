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
load("Analytics/NaturalBrickPCA11.RData")
gridPCA <- grid
gridPCA$pc1 <- unlist(extract(subset(NaturalBrickPCA11, 1), grid))
gridPCA$pc2 <- unlist(extract(subset(NaturalBrickPCA11, 2), grid))
gridPCA$pc3 <- unlist(extract(subset(NaturalBrickPCA11, 3), grid))
gridPCA$pc4 <- unlist(extract(subset(NaturalBrickPCA11, 4), grid))
gridPCA$pc5 <- unlist(extract(subset(NaturalBrickPCA11, 5), grid))
gridPCA$pc6 <- unlist(extract(subset(NaturalBrickPCA11, 6), grid))
gridPCA$pc7 <- unlist(extract(subset(NaturalBrickPCA11, 7), grid))
gridPCA$pc8 <- unlist(extract(subset(NaturalBrickPCA11, 8), grid))
gridPCA$pc9 <- unlist(extract(subset(NaturalBrickPCA11, 9), grid))
gridPCA$pc10 <- unlist(extract(subset(NaturalBrickPCA11, 10), grid))
gridPCA$pc11 <- unlist(extract(subset(NaturalBrickPCA11, 11), grid))
gridPCA <- gridPCA[which(!is.na(gridPCA$pc1)),]

write.csv(gridPCA, "SpeciesDataPCA.csv", row.names=FALSE)

#Add all variables
load("Analytics/NaturalBrick.RData")
gridNatural <- grid
naturalVars <- names(naturalBrick)
naturalVars <- unlist(extract(naturalBrick, grid))

naturalVarsMat1 <- matrix(naturalVars, nrow=nrow(grid))
naturalVarsMat2 <- matrix(naturalVars, nrow=nrow(grid), byrow=TRUE)

naturalVarsDF1 <- data.frame(naturalVarsMat1)
naturalVarsDF2 <- data.frame(naturalVarsMat2)
names(naturalVarsDF1) <- names(naturalBrick)
names(naturalVarsDF2) <- names(naturalBrick)

gridNatural <- cbind(data.frame(gridNatural), naturalVarsDF2)
gridNatural <- gridNatural[which(complete.cases(gridNatural)),]


write.csv(gridNatural, "SpeciesDataNatural.csv", row.names=FALSE)

