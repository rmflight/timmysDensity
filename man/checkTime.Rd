\name{checkTime}
\alias{checkTime}
\title{checks if you have gone over the number of queries in the allotted time, and if so, forces a sleep until the time is up}
\usage{
  checkTime(startTime, maxTime, currCount, maxCount)
}
\arguments{
  \item{startTime}{when did you start your calculations}

  \item{maxTime}{how much time allowed for the set number
  of queries}

  \item{currCount}{which query are you on}

  \item{maxCount}{how many queries allowed in the allotted
  time}
}
\value{
  list contianing: \item{sTime: }{the start time of the
  queries} \item{count: }{the current count}
}
\description{
  checks if you have gone over the number of queries in the
  allotted time, and if so, forces a sleep until the time
  is up
}

