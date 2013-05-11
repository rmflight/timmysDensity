# scan in all the unique data

inCon <- file("../timhortonsDensity/98-316-XWE2011001-1501.tab", "r")
censusData <- scan(inCon, sep="\n", what="character")

ctPopulation <- grep("Population in 2011", censusData, value=TRUE)
censusID <- sapply(allSplit, function(x){x[[1]]})
censusProvince <- sapply(allSplit, function(x){x[[2]]})
locTotal <- as.numeric(sapply(allSplit, function(x){x[[7]]}))

save(censusID, censusProvince, locTotal, file="censusTotals.RData")