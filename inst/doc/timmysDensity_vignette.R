
## @knitr dataOpts
options(stringsAsFactors=F)
require(timmysDensity)
require(plyr)
require(maps)
require(ggplot2)
require(geosphere)


## @knitr loadCensusData, eval=FALSE
## censusData <- read.table("../timmysData/2011_92-151_XBB_XLSX.csv", header=F, sep=";", quote="")
## censusData <- censusData[,1:17]
## names(censusData) <- c("DBuid", "DBpop2011", "DBtdwell2011", "DBurdwell2011", "DBarea", "DB_ir2011", "DAuid", "DAlamx", "DAlamy", "DAlat",
##                        "DAlong", "PRuid", "PRname", "PRename", "PRfname", "PReabbr", "PRfabbr")
## censusData$DBpop2011 <- as.numeric(censusData$DBpop2011)
## censusData$DBpop2011[is.na(censusData$DBpop2011)] <- 0
## 
## censusData$DBtdwell2011 <- as.numeric(censusData$DBtdwell2011)
## censusData$DBtdwell2011[is.na(censusData$DBtdwell2011)] <- 0


## @knitr summarizeAreas, eval=FALSE
## uniqAreas <- unique(censusData$DAuid)
## 
## summarizeArea <- function(areaID){
##   areaData <- censusData[(censusData$DAuid == areaID),]
##   outData <- data.frame(uid=areaID, lamx=areaData[1,"DAlamx"], lamy=areaData[1,"DAlamy"], lat=areaData[1,"DAlat"], long=areaData[1,"DAlong"], pop=sum(areaData[,"DBpop2011"]), dwell=sum(areaData[,"DBtdwell2011"]), prov=areaData[1, "PRename"])
##   return(outData)
## }
## areaData <- adply(uniqAreas, 1, summarizeArea)
## .sessionInfo <- sessionInfo()
## .timedate <- Sys.time()
## write.table(areaData, file="../timmysData/areaData.txt", sep="\t", row.names=F, col.names=T)
## save(areaData, .sessionInfo, .timedate, file="../timmysData/areaDataFile.RData", compress="xz")


## @knitr generateQueries, eval=FALSE
## load("../timmysData/areaDataFile.RData")
## head(areaData)


## @knitr runQueries, eval=FALSE
## runQueries(areaData, idFile="../timmysData/clientid.txt", secretFile="../timmysData/clientsecret.txt", outFile="../timmysData/timmysLocs2.txt")


## @knitr cleanup, eval=FALSE
## cleanUpResults("../timmysData/timmysLocs2.txt")


## @knitr readTimsLocs, eval=FALSE
## # read in and clean up the data
## timsLocs <- scan(file="../timmysData/timmysLocs2.txt", what=character(), sep="\n")
## timsLocs <- strsplit(timsLocs, ":")
## 
## timsName <- sapply(timsLocs, function(x){x[1]})
## timsLat <- sapply(timsLocs, function(x){x[2]})
## timsLong <- sapply(timsLocs, function(x){x[3]})
## 
## locData <- data.frame(description=timsName, lat=as.numeric(timsLat), long=as.numeric(timsLong))
## hasNA <- is.na(locData[,"lat"]) | is.na(locData[,"long"])
## locData <- locData[!(hasNA),]
## 
## timsStr <- c("tim hortons", "tim horton's")
## 
## hasTims <- (grepl(timsStr[1], locData$description, ignore.case=T)) | (grepl(timsStr[2], locData$description, ignore.case=T))
## 
## locData <- locData[hasTims,]
## timsLocs <- locData
## rm(timsName, timsLat, timsLong, hasNA, locData, hasTims, timsStr)
## .timedate <- Sys.time()
## .sessionInfo <- sessionInfo()
## save(timsLocs, .timedate, .sessionInfo, file="../timmysData/timsLocs.RData", compress="xz")


## @knitr mapIt
data(timsLocs)
data(areaDataFile)
canada <- map_data("world", "canada")

p <- ggplot(legend=FALSE) +
  geom_polygon( data=canada, aes(x=long, y=lat,group=group)) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_blank(),axis.text.y = element_blank()) +
  theme(axis.ticks = element_blank()) +
  xlab("") + ylab("")

sp <- timsLocs[1, c("lat", "long")]

p2 <- p + geom_point(data=timsLocs[,c("lat", "long")], aes(x=long, y=lat), colour="green", size=1, alpha=0.5)

print(p2)


## @knitr maxDistance
queryLocs <- matrix(c(timsLocs$long, timsLocs$lat), nrow=nrow(timsLocs), ncol=2, byrow=F) # these are the tims locations
distLocs <- matrix(c(areaData$long, areaData$lat), nrow=nrow(areaData), ncol=2, byrow=F) # the census centers
allDists <- apply(queryLocs, 1, function(x){
  min(distHaversine(x, distLocs)) # only need the minimum value to determine 
})


## @knitr percPopulation
totPopulation <- sum(areaData$pop, na.rm=T)
lessDist <- seq(50, 51.6 * 1000, 50) # distances are in meters, so multiply by 1000 to get reasonable km

percPop <- sapply(lessDist, function(inDist){
  isLess <- allDists < inDist
  sum(areaData$pop[isLess], na.rm=T) / totPopulation * 100
})

plotDistPerc <- data.frame(distance=lessDist, population=percPop, logDist=log10(lessDist))
ggplot(plotDistPerc, aes(x=logDist, y=population)) + geom_point()


