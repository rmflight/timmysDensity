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
  opts(panel.background = theme_blank()) +
  opts(panel.grid.major = theme_blank()) +
  opts(panel.grid.minor = theme_blank()) +
  opts(axis.text.x = theme_blank(),axis.text.y = theme_blank()) +
  opts(axis.ticks = theme_blank()) +
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