require(XML)
gsmXML <- xmlParse("disseminationAreaLocations.gml")


coordConvert <- function(inCoord){
	splitUp <- strsplit(inCoord, " ")[[1]]
	nCoor <- length(splitUp)
	lat <- as.numeric(splitUp[seq(1, nCoor-1, 2)])
	long <- as.numeric(splitUp[seq(2, nCoor, 2)])
	
	return(cbind(lat, long))
}

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

locFeatures <- xpathApply(gsmXML, "//gml:featureMember", extractData)

.timeDate <- Sys.time()
.sessionInfo <- sessionInfo()
save(locFeatures, .timeDate, .sessionInfo, file="disseminationAreasLoc.RData")

# getting radius for each set of coordinates

require(plyr)

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

locData <- data.frame(uid=names(mn.rad), qRad=qRad, qLat=qLat, qLong=qLong, qString=qStr, block=blockAss, isDone=FALSE, timLocs="")

write.table(locData, file="censusDisseminationLocData.txt", sep="\t", row.names=F, col.names=T)