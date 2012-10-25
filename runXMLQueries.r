require(XML)
require(RCurl)
require(lubridate)

apiStr <- "https://maps.googleapis.com/maps/api/place/search/xml?"
srchStr <- "&types=food&name=tim%20%hortons&sensor=false"

testNone <- data.frame(uid=1, qRad=248, qString="46.10416,-64.81636")
testSome <- data.frame(uid=1, qRad=1000, qString="44.640811,-63.574705")
testMulti <- data.frame(uid=1, qRad=5000, qString="44.640811,-63.574705")


# takes an querystring, parses it, and returns the latitudes and longitudes 
# inSearch: the query string to use
# currCount: a counter for the number of queries performed in the alloted time
# allCount: the total number of queries that have been performed
searchLoc <- function(inSearch, currCount=0, allCount=0){

	start.time <- now()
	tmpXML <- getURL(inSearch, timeout=4, ssl.verifypeer=F)
	parsedDat <- FALSE
	
	if ((length(tmpXML) > 0) && (substring(tmpXML, 1, 5) == "<?xml")){
		qDat <- xmlParse(tmpXML)
		
		outLoc <- ""
		currCount <- currCount + 1
		allCount <- allCount + 1
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
				allCount <- allCount$allCount
			}
			
			
		}
		parsedDat <- TRUE
	}
	return(list(outLoc=outLoc, count=currCount, allCount=allCount, isParse=parsedDat))
}

# checks if you have gone over the number of queries in the allotted time, and if so, forces a sleep until the time
# is up
# startTime: when did you start your calculations
# maxTime: how much time allowed for the set number of queries
# currCount: which query are you on
# maxCount: how many queries allowed in the allotted time
checkTime <- function(startTime, maxTime, currCount, maxCount){
	currDiff <- difftime(now(), startTime, units="secs")
	
	if ((currCount >= maxCount) & (currDiff <= maxTime)){
		nextDate <- startTime + maxTime
		diffNext <- difftime(nextDate, now(), units="secs")
		print("waiting until next allowed period!")
		Sys.sleep(diffNext)
		currCount <- 0
		startTime <- now()
	}
	return(list(sTime=startTime, count=currCount))
}

# runBlock: query an entire block of data
# This takes which blocks you want to work on
# blockIndex: which blocks of data to process
# keyFile: the file that has your google places api key
# inFile: what file has the tab delimited data to process
# waitTime: how long to wait before processing the next set of data
# maxEntryTime: how many entries can be processed within "waitTime"
# maxQueryAll: how many total queries you want to perform
runBlock <- function(blockIndex, keyFile="googlemapsapi.key", inFile="censusDisseminationLocData.txt", waitTime=24*60*60, maxEntryTime=1000, maxQueryAll=Inf){
	stopifnot(is.numeric(blockIndex), file.exists(keyFile), file.exists(inFile), is.numeric(waitTime), is.numeric(maxEntryTime))
	locData <- read.table(inFile, sep="\t", header=T, stringsAsFactors=F)
	
	useKey <- scan(keyFile, "character")
	keyStr <- paste("&key=", useKey, sep="", collapse="")
	
	nBlock <- length(blockIndex)
	getCount <- 0 # keeps track of how many files we have downloaded, ie how many queries did we make on the server
	allCount <- 0
	startTime <- now() # when are we starting, want to know because we can only make 1000 queries in 24 hours
	
	
	for (iBlock in 1:nBlock){
		locBloc <- which(locData$block == blockIndex[iBlock])
		isFalse <- locBloc[which(!(locData[locBloc, "isDone"]))]
		if (length(isFalse) > 0){
						
			for (iFalse in isFalse){
				
				inLoc <- locData[iFalse,]
				locStr <- paste("location=", inLoc$qString, sep="", collapse="")
				radStr <- paste("&radius=", round(inLoc$qRad), sep="", collapse="")
				qStr <- paste(apiStr, locStr, radStr, srchStr, keyStr, sep="", collapse="")
				resultLoc <- searchLoc(qStr, getCount, allCount)
				locData[iFalse, "timLocs"] <- resultLoc$outLoc
				locData[iFalse, "isDone"] <- TRUE
				locData[iFalse, "wasParse"] <- resultLoc$isParse
				getCount <- resultLoc$count
				
				write.table(locData, file=inFile, row.names=F, col.names=T, sep="\t")
				
				allCount <- resultLoc$allCount
				# print(allCount)
				# print(getCount)
				if (allCount >= maxQueryAll){
					stop("Exceeded total allowed queries!")
				}
				
				checkRes <- checkTime(startTime, waitTime, getCount, maxEntryTime)
				getCount <- checkRes$count
				startTime <- checkRes$sTime
			}
			
		}
	}
	
}


## Testing functions
runTests <- function(){
	useKey <- scan("googlemapsapi.key", "character")
	inLoc <- testNone
	locStr <- paste("location=", inLoc$qString, sep="", collapse="")
	radStr <- paste("&radius=", round(inLoc$qRad), sep="", collapse="")
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

runQueryTest <- function(){
	runBlock(1, keyFile="googlemapsapi.key", inFile="censusDisseminationLocData.txt", waitTime=60, maxEntryTime=10, maxQueryAll=20)
}
	


