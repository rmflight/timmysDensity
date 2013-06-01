\name{cleanUpResults}
\alias{cleanUpResults}
\title{cleanup the results to make sure only unique results}
\usage{
  cleanUpResults(inLocFile = "timmysLocs.txt")
}
\arguments{
  \item{inLocFile}{the file with the locations}
}
\description{
  cleanup the results to make sure only unique results
}
\details{
  takes a file containing the Tims locations, and makes
  sure that only unique entries are in the file. As a side
  effect, simply writes the data back to the same file.
}

