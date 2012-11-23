# Actually run some queries

source("runXMLQueries.r")
runQueries(seq(8000, 14000), idFile="clientid.txt", secretFile="clientsecret.txt", inFile="censusDisseminationLocData.txt", outFile="timmysLocs.txt", waitTime=60*60, maxEntryTime=5000, checkTime=100)
cleanUpResults()