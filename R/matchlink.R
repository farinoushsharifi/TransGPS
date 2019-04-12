#' Title
#'
#' @param LatList -List of latitudes collected from a GPS recording device
#' @param LongList -List of longitudes collected from a GPS recording device
#'
#' @return The link ID for each recorded set of latitude and longitude
#' @export
#'
#' @examples
#' LatList <- c(31.67514,31.675195,31.67525,31.675304,31.675356,31.675408,31.675467,31.675517,31.675569,31.675623)
#' LongList <- c(-106.326522,-106.326367,-106.326211,-106.326058,-106.325901,-106.325739,-106.325572,-106.32541,-106.325247,-106.325092)
#' linkIDs <- matchlink(LatList,LongList)
#' linkIDs
#' 1184, 1184, 1184, 1184, 1184, 1184, 1183, 1183, 1183, 1782

matchlink <- function(LatList, LongList){

  snappedpoints <- matchtocloselink(LatList,LongList)
  for(i in 1:length(LatList)){
    #get the ID list for the few points back and forth, can estimate the
    #spent time over the distance and compare with the road speed limit,
    #see if the difference is huge or not, then decide to how to match the link

  }
}

#get the box around the whole data
getbox <- function(LatList, LongList){

  minLat <- min(LatList)
  maxLat <- max(LatList)
  minLong <- min(LongList)
  maxLong <- max(LongList)

  mapbox <- corner_bbox(minLong, minLat, maxLong, maxLat)

  return(mapbox)
}

#source the map from open street map
sourcemap <- function(LatList,LongList){

  mapbox <- getbox(LatList,LongList)
  api <- osmsource_api("https://api.openstreetmap.org/api/0.6/")
  location <- get_osm(mapbox, source = api)

  return(location)
}

#find all links in the area
findlinks <- function(LatList,LongList){

  location <- sourcemap(LatList, LongList)
  links <- find(location, way(tags(k == "highway")))
  links <- find_down(location, way(links))
  links <- subset(location, ids = links)

  return(links)
}

#first snap all points to the closest link
matchtocloselink <- function(LatList,LongList){

  links <- as_sp(findlinks(LatList,LongList), "lines")
  points <- c(LatList,LongList)
  #need to find a better function
  snappedpoints <- snapPointsToLines(points,links, maxDist =100, withAttrs = TRUE,idField = "RIA_RTE_ID")

  return(snappedpoints)
}
