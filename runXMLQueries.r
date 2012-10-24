
useKey <- scan("googlemapsapi.key", what="character")

tmpLoc <- mn.rad[[1]]
inRad <- signif(tmpLoc$extCoord$mnDist, 2)
radStr <- paste("&radius=", inRad, sep="", collapse="")

apiStr <- "https://maps.googleapis.com/maps/api/place/search/xml?location="
locStr <- paste(round(tmpLoc$extCoord$mnLoc, 5), sep=",", collapse=",")
srchStr <- "&types=food&name=tim%20%hortons&sensor=false&key="

constQ <- paste(apiStr, locStr, radStr, srchStr, useKey, sep="", collapse="")

tmpXML <- download.file(constQ, "tmpdat.xml")
qDat <- xmlParse("tmpdat.xml")

xmlStatus <- xpathSApply(qDat, "/PlaceSearchResponse/status", xmlValue)

if (xmlStatus[1] == "OK"){
	
}