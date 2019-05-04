
get_boxes <- function(LatList, LongList, timeseq, CoordRefSys, resolution=100){

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

  LatList <- coordtbl$LatList
  LongList <- coordtbl$LongList
  timeseq <- coordtbl$timeseq

  disttbl <- compute_distance(LatList,LongList,CoordRefSys)
  distintervals <- floor(disttbl$CumulativeDistance.km[nrow(disttbl)]/resolution)+1

  disttbl$boxcuts <- cut(disttbl$CumulativeDistance.km,distintervals)

  Latmax <- aggregate(disttbl$Latitude, list(disttbl$boxcuts), max)$x
  Latmin <- aggregate(disttbl$Latitude, list(disttbl$boxcuts), min)$x
  Longmax <- aggregate(disttbl$Longitude, list(disttbl$boxcuts), max)$x
  Longmin <- aggregate(disttbl$Longitude, list(disttbl$boxcuts), min)$x
  Timemax <- aggregate(disttbl$Time, list(disttbl$boxcuts), max)$x
  Timemin <- aggregate(disttbl$Time, list(disttbl$boxcuts), min)$x
  Distsum <- aggregate(disttbl$Distance.km, list(disttbl$boxcuts), sum)$x

  aggdatatbl <- data.frame("Max Latitude" = Latmax,"Min Latitude" = Latmin,
             "Max Longitude" = Longmax,"Min Longitude" = Longmin,
             "Start Time" = Timemin, "End Time" = Timemax, "Total Distance.km" =Distsum )

  boxlist <- list()

  for(i in 1:distintervals) {

    newbox <- center_bbox(mean(aggdatatbl$Max.Longitude[i],aggdatatbl$Min.Longitude[i]),
                mean(aggdatatbl$Max.Latitude[i],aggdatatbl$Min.Latitude[i]),
                resolution, resolution)
    boxlist <- cbind(boxlist,newbox)
  }

  colnames(boxlist) <- paste("Group",seq(1,distintervals,1),sep = "")

  return(boxlist)
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

  disttbl <- data.frame("PointNumber"=seq(1,nrow(latlong),1), "Latitude"=LatList,
                        "Longitude"=LongList, "Time"=timeseq,
                        "Distance.km"=distvec, "CumulativeDistance.km"=cumsum(distvec))

  return(disttbl)
}
