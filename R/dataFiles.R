# data files
#' @name areaData
#' @docType data
#' @format RData
#' @source http://www.data.gc.ca/default.asp?lang=En&n=5175A6F0-1&xsl=datacataloguerecord&metaxsl=datacataloguerecord&formid=C87D5FDD-00E6-41A0-B5BA-E8E41B521ED0
#' @details Contains the latitudes (lat), longitudes (long), x (lamx) and y (lamy) point locations in the Lambert projection, the area id (uid), the number of dwellings (dwell), the total population (pop, based on the sum of the containing blocks), and the province of origin (prov). See the vignette for the processing performed to generate the file.


#' @name timsLocs
#' @docType data
#' @format RData
#' @source Foursquare, generated on June 1, 2013
#' @details Contains latitudes (lat), longitudes (long), and a textual description of Tim Hortons locations from Foursquare based on searching Foursquare for nearest locations to the lat and long from the areas in "areaData".