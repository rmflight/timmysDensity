require(XML)

testNone <- data.frame(uid=1, qRad=248, qString="46.10416,-64.81636")
testSome <- data.frame(uid=1, qRad=1000, qString="44.640811,-63.574705")
testMulti <- data.frame(uid=1, qRad=5000, qString="44.640811,-63.574705")

useKey <- scan("googlemapsapi.key", what="character")
locData <- read.table("censusDisseminationLocData.txt", sep="\t", stringsAsFactors=F, header=T)
useBlock <- 1

waitPeriod <- 24 * 60 * 60 # total time allowed for queries
maxQuery <- 1000
blockData <- locData[(locData$block == useBlock),]


searchLoc <- function(keyStr, locStr, srchStr){

	constQ <- paste(apiStr, locStr, radStr, srchStr, keyStr, sep="", collapse="")
	
	tmpXML <- download.file(constQ, "tmpdat.xml")
	qDat <- xmlParse("tmpdat.xml")
	
	xmlStatus <- xpathSApply(qDat, "/PlaceSearchResponse/status", xmlValue)
	
	if (xmlStatus[1] == "OK"){
		outLat <- xpathSApply(qDat, "//lat", xmlValue)
		outLong <- xpathSApply(qDat, "//lng", xmlValue)
		outLoc <- paste(outLat, outLong, sep=",", collapse=":")
		
		xmlNext <- xpathSApply(qDat, "/PlaceSearchResponse/next_page_token", xmlValue)
		if (length(xmlNext) == 1){
			locStr <- paste("pagetoken=", xmlNext, sep="", collapse="")
			newLoc <- searchLoc(keyStr, locStr, srchStr="")
			outLoc <- paste(outLoc, newLoc, sep="", collapse=":")
		}
		
		return(outLoc)
	}
}

inLoc <- testMulti

inRad <- round(inLoc$qRad, 0)
radStr <- paste("&radius=", inRad, sep="", collapse="")

apiStr <- "https://maps.googleapis.com/maps/api/place/search/xml?"
locStr <- paste("location=", inLoc$qString, sep="", collapse="")
srchStr <- "&types=food&name=tim%20%hortons&sensor=false"
keyStr <- paste("&key=", useKey, sep="", collapse="")

resultLoc <- searchLoc(keyStr, locStr, srchStr)