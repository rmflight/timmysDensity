require(XML)

apiStr <- "https://maps.googleapis.com/maps/api/place/search/xml?"
srchStr <- "&types=food&name=tim%20%hortons&sensor=false"

testNone <- data.frame(uid=1, qRad=248, qString="46.10416,-64.81636")
testSome <- data.frame(uid=1, qRad=1000, qString="44.640811,-63.574705")
testMulti <- data.frame(uid=1, qRad=5000, qString="44.640811,-63.574705")

searchLoc <- function(inSearch){

	start.time <- Sys.time()
	tmpXML <- download.file(inSearch, "tmpdat.xml")
	qDat <- xmlParse("tmpdat.xml")
	
	xmlStatus <- xpathSApply(qDat, "/PlaceSearchResponse/status", xmlValue)
	
	if (xmlStatus[1] == "OK"){
		outLat <- xpathSApply(qDat, "//lat", xmlValue)
		outLong <- xpathSApply(qDat, "//lng", xmlValue)
		outLoc <- paste(outLat, outLong, sep=",", collapse=":")
		
		xmlNext <- xpathSApply(qDat, "/PlaceSearchResponse/next_page_token", xmlValue)
		if (length(xmlNext) == 1){
			qStr2 <- paste(inSearch, "&pagetoken=", xmlNext, sep="", collapse="")
			Sys.sleep(5) # wait for the token to become valid
			newLoc <- searchLoc(qStr2)
			outLoc <- paste(outLoc, newLoc, sep="", collapse=":")
		}
		
		return(outLoc)
	}
}



# runBlock: query an entire block of data
runBlock <- function(blockIndex, keyFile="googlemapsapi.key", inFile="censusDisseminationLocData.txt", waitTime=24*60*60, maxEntry=1000){
	
}


## Testing functions
runTests <- function(){
	inLoc <- testNone
}
# inLoc <- testNone

inRad <- round(inLoc$qRad, 0)
radStr <- paste("&radius=", inRad, sep="", collapse="")


locStr <- paste("location=", inLoc$qString, sep="", collapse="")

keyStr <- paste("&key=", useKey, sep="", collapse="")

qStr <- paste(apiStr, locStr, radStr, srchStr, keyStr, sep="", collapse="")

resultLoc <- searchLoc(qStr)