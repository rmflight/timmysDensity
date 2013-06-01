#' takes an querystring, parses it, and returns the latitudes and longitudes 
#' @param inSearch the query string to use
#' @importFrom RCurl getURL
#' @importFrom RJSONIO fromJSON
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

#' checks if you have gone over the number of queries in the allotted time, and if so, forces a sleep until the time is up
#' @param startTime when did you start your calculations
#' @param maxTime how much time allowed for the set number of queries
#' @param currCount which query are you on
#' @param maxCount how many queries allowed in the allotted time
#' @return list contianing:
#' \itemList{
#'   \item{sTime: }{the start time of the queries}
#'   \item{count: }{the current count}
#' }
#' @importFrom lubridate now
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


#' run foursquare api queries
#' @param queryIndex which entries of data to process
#' @param idFile the file that has your foursquare api key
#' @param secretFile file with the foursquare clientsecret
#' @param inData the data to process
#' @param outFile where to save the results of the search
#' @param waitTime how long to wait before processing the next set of data
#' @param maxEntryTime how many entries can be processed within "waitTime"
#' @param maxQueryAll how many total queries you want to perform
#' @export
#' @details expects \code{inData} to be a data frame with \code{lat}, \code{long}, and \code{block} numeric columns. \code{lat} and \code{long} are used as the locations to query foursquare, \code{block} controls
#' @importFrom lubridate now
runQueries <- function(inData, idFile="clientid.txt", secretFile="clientsecret.txt", outFile="timmysLocs.txt", waitTime=60*60, maxEntryTime=5000, checkTime=100){
	stopifnot(is.numeric(queryIndex), file.exists(idFile), file.exists(secretFile), file.exists(inFile), is.numeric(waitTime), is.numeric(maxEntryTime))
	
  # foursquare api information
	apiStr <- "https://api.foursquare.com/v2/venues/search?"
	srchStr <- "&query=tim%20hortons&limit=50"
  
  useid <- scan(idFile, "character")
  useSecret <- scan(secretFile, "character")
  clientStr <- paste("&client_id=", useid, "&client_secret=", useSecret, "&v=", as.character(now(), format="%Y%m%d"), sep="", collapse="")
	
	inData$qString <- paste(inData$lat, inData$long, sep=",")
  
	nQuery <- nrow(inData)
	getCount <- 0 # keeps track of how many files we have downloaded, ie how many queries did we make on the server
	allCount <- 0
	startTime <- now() # when are we starting, want to know because we can only make so many queries in a particular time period
	
  useSplit <- seq(1, length(queryIndex) / checkTime)
  splitIndx <- split(queryIndex, rep(useSplit, length(queryIndex) / length(useSplit)))
  
  sapply(splitIndx, function(inIndx){
    sapply(inData$qString[inIndx], function(useLoc){
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

#' cleanup the results to make sure only unique results
#' @param inLocFile the file with the locations
#' @details takes a file containing the Tims locations, and makes sure that only unique entries are in the file. As a side effect, simply writes the data back to the same file.
#' @export
cleanUpResults <- function(inLocFile="timmysLocs.txt"){
  stopifnot(file.exists(inLocFile))
  allLocs <- scan(inLocFile, what=character(), sep="\n")
  allLocs <- unique(allLocs)
  cat(allLocs, file=inLocFile, sep="\n", append=F)
}