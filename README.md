# Tim Hortons Density

The idea of this project is to use census data and Google Maps data to figure out the density
of Tim Hortons locations in Canada based on Stats Canada census dissemination areas. This will
also allow us to combine the census data with the Tim's locations and figure out the average 
distance to a Tim Hortons location.

This work is inspired by [this post](http://www.ifweassume.com/2012/10/the-united-states-of-starbucks.html).

## Data

The dissemination areas location data was downloaded as a GML from Stats Canada
website. The census data is the CSV for dissemination areas (smallest census unit).

## Working with geographic coordinates

http://en.wikipedia.org/wiki/Geographic_coordinate_conversion
http://en.wikipedia.org/wiki/Geographical_distance

## Google Places API

https://developers.google.com/places/documentation/search


# Contributing

There are ~52000 regions that need to be queried, and the Google places API only allows 1000 queries in 24 hours. Therefore, to do collect all the Tim Hortons locations myself will take ~52 days. Therefore, I'm asking for help from others. Either to run sets of queries, or point me to another source of the data. I have divided the data into 53 blocks of 1000 entries each.

## Running queries

The querying code is written in `R`. If you want to run your own queries to contribute:

  * clone this repo, 
  * apply for a Google places API (the code assumes this will be a single line in `googlemapsapi.key`, however you can specify another file if you need to) 
  * source `runXMLQueries.r`, 
  * `runBlock(blockNumber)`, *e.g.* `runBlock(c(2,3,4))` to run multiple blocks.

The querying function writes a new version of the file after each successful query, so there isn't a problem if something goes wrong in `R` itself. The code will currently run 1000 queries in 24 hours for the specified blocks. You can easily change the number of total queries to run, the time limit for a set number of queries, and the time to wait before running more queries (see the code for which variable is which).

To add to the data contained in this repo, submit a pull request to me, and I will merge the data back in with this master copy.

## Attribution

If you decide to run some queries, feel free to add your name to the list of contribitors on this Readme file when you submit your pull request.

# Contributors

Block 1: rmflight
