library(geosphere)
library(sp)
library(rgdal)


#get the box around the whole data
get_boxes <- function(LatList, LongList, timeseq, CoordRefSys, resolution=10){

  if (length(LatList)!=length(LongList)) {
    stop("Latitude and longitude lists do not have the same length")
  }

  if (length(LatList)!=length(timeseq)) {
    stop("Latitude and time sequence lists do not have the same length")
  }

  if (any(class(timeseq[i])!=c("POSIXct","POSIXt"))){
    stop("Time Sequense in not in POSIXct or POSIXt format. You can change it using the as.POSIXct or as.POSIXlt functions")
  }

  coordtbl <- data.frame(LatList,LongList,timeseq)
  coordtbl <- coordtbl[order(coordtbl$timeseq),]

  coordtbl$LatList <- coordtbl$LatList
  coordtbl$LongList <- coordtbl$LongList
  timeseq <- coordtbl$timeseq




  minLat <- min(LatList)
  maxLat <- max(LatList)
  minLong <- min(LongList)
  maxLong <- max(LongList)

  mapbox <- corner_bbox(minLong, minLat, maxLong, maxLat)

  return(mapbox)
}


compute_distance <- function(LatList, LongList, CoordRefSys){

  latlong <- SpatialPoints(data.frame(LatList,LongList),CRS(CoordRefSys))
  latlong <- spTransform(latlong, CRS("+proj=longlat +datum=WGS84"))
  latlong <- as.data.frame(latlong)

  distvec <- rep(0,times=nrow(latlong))
  for (i in 1:(nrow(latlong)-1)) {
    distance <- distHaversine(c(latlong$LongList[i],latlong$LatList[i]),c(latlong$LongList[i+1],latlong$LatList[i+1]))
    distvec[i+1] <- distance/1000
  }

  disttbl <- data.frame("PointNumber"=seq(1,nrow(latlong),1), "Distance.km"=distvec, "CumulativeDistance.km"=cumsum(distvec))

  return(disttbl)
}
