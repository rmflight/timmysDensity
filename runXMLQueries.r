require(RCurl)
require(RJSONIO)
require(lubridate)

apiStr <- "https://api.foursquare.com/v2/venues/search?"
srchStr <- "&query=tim%20hortons&limit=50"

testNone <- data.frame(uid=1, qRad=248, qString="46.10416,-64.81636")
testSome <- data.frame(uid=1, qRad=1000, qString="44.640811,-63.574705")
testMulti <- data.frame(uid=1, qRad=5000, qString="44.640811,-63.574705")


# takes an querystring, parses it, and returns the latitudes and longitudes 
# inSearch: the query string to use
searchLoc <- function(inSearch){
  outLoc <- "" #default to return
	start.time <- now()
	
  try({tmpJSON <- getURL(inSearch, timeout=4, ssl.verifypeer=F)
       jsonDat <- fromJSON(tmpJSON)
       if (jsonDat$meta == 200){
         results <- sapply(jsonDat$response$venues, function(inResponse){
           outName <- inResponse$name
           outLong <- inResponse$location$lng
           outLat <- inResponse$location$lat
           outLoc <- paste(outName, outLat, outLong, sep=":", collapse=":")
         })
         outLoc <- results
       }})
    
  return(outLoc)
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

# runQueries: query a bunch of data
# This takes which locatoins you want to work on
# blockIndex: which blocks of data to process
# keyFile: the file that has your google places api key
# inFile: what file has the tab delimited data to process
# waitTime: how long to wait before processing the next set of data
# maxEntryTime: how many entries can be processed within "waitTime"
# maxQueryAll: how many total queries you want to perform
runQueries <- function(queryIndex, idFile="clientid.txt", secretFile="clientsecret.txt", inFile="censusDisseminationLocData.txt", outFile="timmysLocs.txt", waitTime=60*60, maxEntryTime=5000, checkTime=100){
	stopifnot(is.numeric(queryIndex), file.exists(idFile), file.exists(secretFile), file.exists(inFile), is.numeric(waitTime), is.numeric(maxEntryTime))
	locData <- read.table(inFile, sep="\t", header=T, stringsAsFactors=F)
	
	useid <- scan(idFile, "character")
  useSecret <- scan(secretFile, "character")
  clientStr <- paste("&client_id=", useid, "&client_secret=", useSecret, "&v=", as.character(now(), format="%Y%m%d"), sep="", collapse="")
	
	
	nQuery <- length(queryIndex)
	getCount <- 0 # keeps track of how many files we have downloaded, ie how many queries did we make on the server
	allCount <- 0
	startTime <- now() # when are we starting, want to know because we can only make 1000 queries in 24 hours
	
  useSplit <- seq(1, length(queryIndex) / checkTime)
  splitIndx <- split(queryIndex, rep(useSplit, length(queryIndex) / length(useSplit)))
  
  sapply(splitIndx, function(inIndx){
    sapply(locData$qString[inIndx], function(useLoc){
      locStr <- paste("ll=", useLoc, sep="", collapse="")
      qStr <- paste(apiStr, locStr, srchStr, clientStr, sep="", collapse="")
      resultLoc <- searchLoc(qStr)
      if (length(resultLoc > 0) & (nchar(resultLoc[1]) > 0)){
        cat(resultLoc, sep="\n", file=outFile, append=T)
      }
      
    })
    allCount <<- allCount + length(inIndx)
    getCount <<- length(inIndx) + getCount
    checkRes <- checkTime(startTime, waitTime, getCount, maxEntryTime)
    getCount <<- checkRes$count
    startTime <<- checkRes$sTime
  })
}

cleanUpResults <- function(inLocFile="timmysLocs.txt"){
  stopifnot(file.exists(inLocFile))
  allLocs <- scan(inLocFile, what=character(), sep="\n")
  allLocs <- unique(allLocs)
  cat(allLocs, file=inLocFile, sep="\n", append=F)
}

## Testing functions
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
# inLoc <- testNone

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
