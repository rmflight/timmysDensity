\name{runQueries}
\alias{runQueries}
\title{run foursquare api queries}
\usage{
  runQueries(inData, idFile = "clientid.txt",
    secretFile = "clientsecret.txt",
    outFile = "timmysLocs.txt", waitTime = 60 * 60,
    maxEntryTime = 5000, checkTime = 100)
}
\arguments{
  \item{queryIndex}{which entries of data to process}

  \item{idFile}{the file that has your foursquare api key}

  \item{secretFile}{file with the foursquare clientsecret}

  \item{inData}{the data to process}

  \item{outFile}{where to save the results of the search}

  \item{waitTime}{how long to wait before processing the
  next set of data}

  \item{maxEntryTime}{how many entries can be processed
  within "waitTime"}

  \item{maxQueryAll}{how many total queries you want to
  perform}
}
\description{
  run foursquare api queries
}
\details{
  expects \code{inData} to be a data frame with \code{lat},
  \code{long}, and \code{block} numeric columns. \code{lat}
  and \code{long} are used as the locations to query
  foursquare, \code{block} controls
}

