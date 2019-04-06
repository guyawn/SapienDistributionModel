#Prepare workspace
rm(list=ls())
source("Helpers/iterateSDM.R")
load("Analytics/NaturalFlickrBrick.RData")


FlyingFoxData <- iterateSDM("Data/Species/FlyingFox/FlyingFox.shp", naturalFlickrBrick)
KangarooData <- iterateSDM("Data/Species/Kangaroo/Kangaroo.shp", naturalFlickrBrick)
WombatData <- iterateSDM("Data/Species/Wombat/Wombat.shp", naturalFlickrBrick)
SugarGliderData <- iterateSDM("Data/Species/SugarGlider/SugarGlider.shp", naturalFlickrBrick)
KoalaData <- iterateSDM("Data/Species/Koala/Koala.shp", naturalFlickrBrick)




