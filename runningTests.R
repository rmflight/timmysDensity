#' run some simple tests
runTests <- function(){
  useid <- scan("clientid.txt", "character")
  useSecret <- scan("clientsecret.txt", "character")
  clientStr <- paste("&client_id=", useid, "&client_secret=", useSecret, "&v=", as.character(now(), format="%Y%m%d"), sep="", collapse="")
  inLoc <- testNone
  locStr <- paste("ll=", inLoc$qString, sep="", collapse="")
  qStr <- paste(apiStr, locStr, srchStr, clientStr, sep="", collapse="")
  resultLocNone <- searchLoc(qStr)
  
  inLoc <- testSome
  clientStr <- paste("&client_id=", useid, "&client_secret=", useSecret, "&v=", as.character(now(), format="%Y%m%d"), sep="", collapse="")
  locStr <- paste("ll=", inLoc$qString, sep="", collapse="")
  qStr <- paste(apiStr, locStr, srchStr, clientStr, sep="", collapse="")
  resultLocSome <- searchLoc(qStr)
  
  inLoc <- testMulti
  clientStr <- paste("&client_id=", useid, "&client_secret=", useSecret, "&v=", as.character(now(), format="%Y%m%d"), sep="", collapse="")
  locStr <- paste("ll=", inLoc$qString, sep="", collapse="")
  qStr <- paste(apiStr, locStr, srchStr, clientStr, sep="", collapse="")
  resultLocMulti <- searchLoc(qStr, 0)
}


runQueryTest <- function(){
  runQueries(seq(1,100), idFile="clientid.txt", secretFile="clientsecret.txt", inFile="censusDisseminationLocData.txt", outFile="timmysLocs.txt", waitTime=60*60, maxEntryTime=5000, checkTime=100)
}

runTimeTest <- function(){
  t1 <- now()
  runQueries(seq(1,300), idFile="clientid.txt", secretFile="clientsecret.txt", inFile="censusDisseminationLocData.txt", outFile="timmysLocs.txt", waitTime=60*60, maxEntryTime=5000, checkTime=100)
  t2 <- now()
  tDiff <- difftime(t2,t1,units="s")
  tDiff / 300
}



# Actually run some queries

source("runXMLQueries.r")
runQueries(seq(40001, 52531), idFile="clientid.txt", secretFile="clientsecret.txt", inFile="censusDisseminationLocData.txt", outFile="timmysLocs.txt", waitTime=60*60, maxEntryTime=5000, checkTime=100)
cleanUpResults()


# setting up data for queries
require(XML)
gsmXML <- xmlParse("disseminationAreaLocations.gml")

# takes a set of tuples of latitude / longitude coordinates as a string, and generates a matrix where
# column 1 is latitude and column 2 is longitude
coordConvert <- function(inCoord){
  splitUp <- strsplit(inCoord, " ")[[1]]
  nCoor <- length(splitUp)
  lat <- as.numeric(splitUp[seq(1, nCoor-1, 2)])
  long <- as.numeric(splitUp[seq(2, nCoor, 2)])
  
  return(cbind(lat, long))
}

# extracts the various pieces of ID and location data from the gml file for census dissemination locations
# inNodeSet: an XML GML featureMember node, that has DAUID, and exterior and interior descriptions of area
extractData <- function(inNodeSet){
  uid <- xpathSApply(inNodeSet, "*/fme:DAUID", namespaces=xmlNamespaceDefinitions(gsmXML, simplify=T), xmlValue)
  
  extCoord <- xpathSApply(inNodeSet, "*/gml:surfaceProperty/*/gml:patches/gml:PolygonPatch/gml:exterior/gml:LinearRing/gml:posList", namespaces=xmlNamespaceDefinitions(gsmXML, simplify=T), xmlValue)
  
  intCoord <- xpathSApply(inNodeSet, "*/gml:surfaceProperty/*/gml:patches/gml:PolygonPatch/gml:interior/gml:LinearRing/gml:posList", namespaces=xmlNamespaceDefinitions(gsmXML, simplify=T), xmlValue)
  
  if (length(intCoord) != 0){
    intCoord <- coordConvert(intCoord)
  }
  
  if (length(extCoord) != 0){
    extCoord <- coordConvert(extCoord)
  }
  
  return(list(uid=uid, extCoord=extCoord, intCoord=intCoord))
  
}

# actually run the extraction on each featureMember node
locFeatures <- xpathApply(gsmXML, "//gml:featureMember", extractData)

.timeDate <- Sys.time()
.sessionInfo <- sessionInfo()
save(locFeatures, .timeDate, .sessionInfo, file="disseminationAreasLoc.RData")

# getting radius for each set of coordinates

require(plyr)

# want to check for the presence of each different type of geo feature, and then run the mean and distance calculations
# on it.
getMnDist <- function(inLocFeature){
  extMnRad <- list()
  intMnRad <- list()
  if (length(inLocFeature$extCoord[[1]]) != 0){
    extMnRad <- calcRad(inLocFeature$extCoord)
  }
  
  if (length(inLocFeature$intCoord) != 0){
    intMnRad <- calcRad(inLocFeature$intCoord)
  }
  
  return(list(extCoord=extMnRad, intCoord=intMnRad))
}


# calculate the mean location of the points, and the radius from the mean
# based on a matrix of input coordinates, with the first column being latitude
# and the second column being longitude
#
# inCoord: matrix of two columns, first column is latitude, second column is longitude
calcRad <- function(inCoord){
  # first convert degrees to radians
  radCoord <- (pi/180) * inCoord
  
  earthRad <- 6353 # note this is in km
  
  coordMn <- colMeans(radCoord)
  
  distPnt <- aaply(radCoord, 1, function(x){
    mnLat <- mean(x["lat"], coordMn["lat"])
    dLat <- x["lat"] - coordMn["lat"]
    dLon <- x["long"] - coordMn["long"]
    
    dPnts <- earthRad * sqrt(dLat^2 + (cos(mnLat) * dLon)^2)
  })
  
  mnDist <- mean(distPnt) * 1000 # our original distances are km, so convert to m
  mnLatLong <- colMeans(inCoord)
  
  return(list(mnLoc=mnLatLong, mnDist=mnDist))
}

mn.rad <- lapply(locFeatures, getMnDist)
uid <- sapply(locFeatures, function(x){x$uid})
names(mn.rad) <- uid

.sessionInfo <- sessionInfo()
.timeDate <- Sys.time()
save(mn.rad, .sessionInfo, .timeDate, file="loc.radius.RData")

# now get IDs, and chop out anything above 54 deg latitude
allLat <- sapply(mn.rad, function(x){
  x$extCoord$mnLoc["lat"]
})

keepLat <- allLat <= 53.4

mn.rad <- mn.rad[keepLat]

qRad <- sapply(mn.rad, function(x){x$extCoord$mnDist})
qLat <- sapply(mn.rad, function(x){x$extCoord$mnLoc["lat"]})
qLat <- round(qLat, 5)
qLong <- sapply(mn.rad, function(x){x$extCoord$mnLoc["long"]})
qLong <- round(qLong, 5)

qStr <- paste(qLat, qLong, sep=",")
blockAss <- rep(seq(1,53), 1000)
blockAss <- blockAss[1:length(qLong)]

locData <- data.frame(uid=names(mn.rad), qRad=qRad, qLat=qLat, qLong=qLong, qString=qStr, block=blockAss, isDone=FALSE, timLocs="", wasParsed=FALSE)

locData <- locData[(order(locData$block)),]
locData$qRad[(locData$qRad > 50000)] <- 50000

write.table(locData, file="censusDisseminationLocData.txt", sep="\t", row.names=F, col.names=T)


# analyzing the data
# timmys location analysis
options(stringsAsFactors=F)

# read in and clean up the data
timsLocs <- scan(file="timmysLocs.txt", what=character(), sep="\n")
timsLocs <- strsplit(timsLocs, ":")

timsName <- sapply(timsLocs, function(x){x[1]})
timsLat <- sapply(timsLocs, function(x){x[2]})
timsLong <- sapply(timsLocs, function(x){x[3]})

locData <- data.frame(description=timsName, lat=as.numeric(timsLat), long=as.numeric(timsLong))
hasNA <- is.na(locData[,"lat"]) | is.na(locData[,"long"])
locData <- locData[!(hasNA),]

timsStr <- c("tim hortons", "tim horton's")

hasTims <- (grepl(timsStr[1], locData$description, ignore.case=T)) | (grepl(timsStr[2], locData$description, ignore.case=T))

locData <- locData[hasTims,]

# start looking at some locations on a map

library(maps)
library(ggplot2)

canada <- map_data("world", "canada")

p <- ggplot(legend=FALSE) +
  geom_polygon( data=canada, aes(x=long, y=lat,group=group)) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_blank(),axis.text.y = element_blank()) +
  theme(axis.ticks = element_blank()) +
  xlab("") + ylab("")

sp <- locData[1, c("lat", "long")]

p2 <- p + geom_point(data=locData[,c("lat", "long")], aes(x=long, y=lat), colour="green", size=1, alpha=0.5)

print(p)
print(p2)

# this is the full map. Now what if we want to look at the individual census tracts??
load("disseminationAreasLoc.RData")

# Next steps would be to read in the actual census data, and use the area and population counts to calculate the population densities.
# In addition, using the centers, and population counts, we should be able to come up with some meaure of 
# how far away someone is from a Tim Hortons, and even come up with a distance for the closest Tim Horton's
# to come up with the minimum distance.
censusLocs <- read.table("censusDisseminationLocData.txt", sep="\t", header=T)
load("censusTotals.RData")

# calculate distances between every tims and every center of each census tract
require(geosphere)

queryLocs <- matrix(c(locData$long, locData$lat), nrow=nrow(locData), ncol=2, byrow=F) # these are the tims locations
distLocs <- matrix(c(censusLocs$qLong, censusLocs$qLat), nrow=nrow(censusLocs), ncol=2, byrow=F) # the census centers

# for each of the Tims Locations in "locData", calculate the distances to all of the centers of the census tracts.

allDists <- apply(queryLocs, 1, function(x){
  distHaversine(x, distLocs) / 1000
})

# Now for all the Tims locations, get total population within that distance, and then divide over the total population of Canada.

keepTotal <- censusID %in% censusLocs$uid
locTotal <- locTotal[keepTotal]
totPopulation <- sum(locTotal, na.rm=T)

lessDist <- seq(0, 200, 1)

sumLessDist <- function(inDist){
  isLess <- allDists < inDist
  nLess <- apply(isLess, 1, sum)
  anyLess <- nLess > 0
  sum(locTotal[anyLess], na.rm=T) / totPopulation * 100
}

percPop <- sapply(lessDist, sumLessDist)
save(percPop, lessDist, file="distancePercentages.RData")

plot(lessDist, percPop, xlim=c(0, 2))


# scan in all the unique data for each area

inCon <- file("../timhortonsDensity/98-316-XWE2011001-1501.tab", "r")
censusData <- scan(inCon, sep="\n", what="character")

ctPopulation <- grep("Population in 2011", censusData, value=TRUE)
censusID <- sapply(allSplit, function(x){x[[1]]})
censusProvince <- sapply(allSplit, function(x){x[[2]]})
locTotal <- as.numeric(sapply(allSplit, function(x){x[[7]]}))

save(censusID, censusProvince, locTotal, file="censusTotals.RData")