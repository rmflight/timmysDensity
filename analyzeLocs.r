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

totPopulation <- sum(locTotal, na.rm=T)

lessDist <- seq(0, 200, 0.1)

percPop <- sapply(lessDist, function(inDist){
  isLess <- allDists < inDist
  anyLess <- apply(isLess, 1, function(x){sum(x) > 0})
  sum(locTotal[anyLess], na.rm=T) / totPopulation * 100
})

plot(lessDist, percPop, xlim=c(0, 2))