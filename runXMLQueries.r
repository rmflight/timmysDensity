require(XML)
require(lubridate)

apiStr <- "https://maps.googleapis.com/maps/api/place/search/xml?"
srchStr <- "&types=food&name=tim%20%hortons&sensor=false"

testNone <- data.frame(uid=1, qRad=248, qString="46.10416,-64.81636")
testSome <- data.frame(uid=1, qRad=1000, qString="44.640811,-63.574705")
testMulti <- data.frame(uid=1, qRad=5000, qString="44.640811,-63.574705")

searchLoc <- function(inSearch, currCount=1){

	start.time <- now()
	tmpXML <- download.file(inSearch, "tmpdat.xml")
	qDat <- xmlParse("tmpdat.xml")
	
	outLoc <- NULL
	currCount <- currCount + 1
	
	xmlStatus <- xpathSApply(qDat, "/PlaceSearchResponse/status", xmlValue)
	
	if (xmlStatus[1] == "OK"){
		outLat <- xpathSApply(qDat, "//lat", xmlValue)
		outLong <- xpathSApply(qDat, "//lng", xmlValue)
		outLoc <- paste(outLat, outLong, sep=",", collapse=":")
		
		xmlNext <- xpathSApply(qDat, "/PlaceSearchResponse/next_page_token", xmlValue)
		if (length(xmlNext) == 1){
			qStr2 <- paste(inSearch, "&pagetoken=", xmlNext, sep="", collapse="")
			Sys.sleep(5) # wait for the token to become valid
			newLoc <- searchLoc(qStr2, currCount)
			outLoc <- paste(outLoc, newLoc$outLoc, sep="", collapse=":")
			currCount <- newLoc$count
		}
		
		
	}
	return(list(outLoc=outLoc, count=currCount))
}

checkTime <- function(startTime, maxTime, currCount, maxCount){
	currDiff <- difftime(now(), startTime, units="secs")
	
	if ((currCount >= maxCount) & (currDif <= maxTime)){
		nextDate <- startTime + maxTime
		diffNext <- difftime(nextDate, now(), units="secs")
		Sys.sleep(diffNext)
		currCount <- 0
	}
	return(currCount)
}

# runBlock: query an entire block of data
runBlock <- function(blockIndex, keyFile="googlemapsapi.key", inFile="censusDisseminationLocData.txt", waitTime=24*60*60, maxEntry=1000){
	stopifnot(is.numeric(blockIndex), file.exists(keyFile), file.exists(inFile), is.numeric(waitTime), is.numeric(maxEntry))
	locData <- read.table(inFile, sep="\t", header=T, stringsAsFactors=F)
	
	useKey <- scan(keyFile, "character")
	
	nBlock <- length(blockIndex)
	getCount <- 1
	
	for (iBlock in 1:nBlock){
		locBloc <- which(locData$block == blockIndex[iBlock])
		isFalse <- locBloc[which(!(locData[locBloc, "isDone"]))]
		if (length(isFalse) > 0){
			startTime <- Sys.time()
			
			for (iFalse in isFalse){
				
				inLoc <- locData[iFalse,]
				locStr <- paste("location=", inLoc$qString, sep="", collapse="")
				keyStr <- paste("&key=", useKey, sep="", collapse="")
				qStr <- paste(apiStr, locStr, radStr, srchStr, keyStr, sep="", collapse="")
				resultLocNone <- searchLoc(qStr)
				locData[iFalse, "timLocs"] <- result
			}
			
		}
	}
	
}


## Testing functions
runTests <- function(){
	useKey <- scan("googlemapsapi.key", "character")
	inLoc <- testNone
	locStr <- paste("location=", inLoc$qString, sep="", collapse="")
	radStr <- paste("&radius=", inLoc$qRad, sep="", collapse="")
	keyStr <- paste("&key=", useKey, sep="", collapse="")
	qStr <- paste(apiStr, locStr, radStr, srchStr, keyStr, sep="", collapse="")
	resultLocNone <- searchLoc(qStr)
	
	inLoc <- testSome
	locStr <- paste("location=", inLoc$qString, sep="", collapse="")
	radStr <- paste("&radius=", inLoc$qRad, sep="", collapse="")
	keyStr <- paste("&key=", useKey, sep="", collapse="")
	qStr <- paste(apiStr, locStr, radStr, srchStr, keyStr, sep="", collapse="")
	resultLocSome <- searchLoc(qStr)
	
	inLoc <- testMulti
	locStr <- paste("location=", inLoc$qString, sep="", collapse="")
	radStr <- paste("&radius=", inLoc$qRad, sep="", collapse="")
	keyStr <- paste("&key=", useKey, sep="", collapse="")
	qStr <- paste(apiStr, locStr, radStr, srchStr, keyStr, sep="", collapse="")
	resultLocMulti <- searchLoc(qStr, 0)
}
# inLoc <- testNone




locStr <- paste("location=", inLoc$qString, sep="", collapse="")

keyStr <- paste("&key=", useKey, sep="", collapse="")

qStr <- paste(apiStr, locStr, radStr, srchStr, keyStr, sep="", collapse="")

resultLoc <- searchLoc(qStr)