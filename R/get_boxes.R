#' Generating Bounding Boxes
#'
#' Generate a series of bounding boxes to split the long GPS recorded data into smaller
#' regions for a given \code{resolution}
#'
#' @param LatList list of latitudes collected from a GPS recording device
#' @param LongList list of longitudes collected from a GPS recording device
#' @param timeseq list of time series for GPS recording device in format \code{"\%Y-\%m-\%d \%H:\%M:\%S"}
#' @param resolution an approximation of GPS recorded distance within each desired bounding box in kilometers
#' @param offLong a positive bounding box longitudal margin in decimal degrees
#' @param offLat a positive bounding box latitudal margin in decimal degrees
#'
#' @return \code{\link{get_boxes}} return a list of bounding boxes specification table and coordinates table
#' @export
#'
#' @examples
#'
#' LatList <- c(31.67514,31.675195,31.67525,
#' 31.675304,31.675356,31.675408,31.675467,
#' 31.675517,31.675569,31.675623)
#'
#' LongList <- c(-106.326522,-106.326367,
#' -106.326211,-106.326058,-106.325901,
#' -106.325739,-106.325572,-106.32541,
#' -106.325247,-106.325092)
#'
#' timeseq <- c("2019-04-29 15:20:51",
#' "2019-04-29 15:21:03","2019-04-29 15:21:06",
#' "2019-04-29 15:21:15","2019-04-29 15:21:17",
#' "2019-04-29 15:21:32","2019-04-29 15:21:34",
#' "2019-04-29 15:21:51","2019-04-29 15:22:09",
#' "2019-04-29 15:22:36")
#'
#' timeseq <- as.POSIXct(timeseq)
#'
#' offLong=0.001
#' offLat=0.002
#'
#' #generates only one box and gives warning for the high resolution
#' get_boxes(LatList, LongList, timeseq, resolution=1000,offLong,offLat)
#'
#' #generates only one box and gives warning for the small resolution
#' get_boxes(LatList, LongList, timeseq, resolution=0.001,offLong,offLat)
#'
#' #generates two bounding boxes
#' get_boxes(LatList, LongList, timeseq, resolution=0.1,offLong,offLat)

get_boxes <- function(LatList, LongList, timeseq, resolution=100, offLong=0.001,offLat=0.001){

  if (length(LatList)!=length(LongList)) {
    stop("Latitude and longitude lists do not have the same length")
  }

  if (length(LatList)!=length(timeseq)) {
    stop("Latitude and time sequence lists do not have the same length")
  }

  if (any(class(timeseq)!=c("POSIXct","POSIXt"))){
    stop("Time Sequense in not in POSIXct or POSIXt format. You can change it using the as.POSIXct or as.POSIXlt functions")
  }

  if ((!(offLong>0))|(!(offLat>0))) {
    stop("offLong and offLat should be positive decimal degrees")
  }


  disttbl <- compute_distance(LatList,LongList,timeseq)

  LatList <- disttbl$Latitude
  LongList <- disttbl$Longitude
  timeseq <- disttbl$Time

  if (any(resolution < disttbl$Distance.km)) {
    warning("Resolution is at least smaller than one recorded time interval")
  }

  totaldist <- disttbl$CumulativeDistance.km[nrow(disttbl)]

  if (totaldist < resolution) {

    boxlist <- osmar::corner_bbox(min(LongList)-offLong,
                                 min(LatList)-offLat,
                                 max(LongList)+offLong,
                                 max(LatList)+offLat)


    boxlist <- cbind(boxlist)
    colnames(boxlist) <- "Group 1"
    disttbl$boxcuts <- 1
    warning("Resolution is too large for this area, only one box has been generated")
  } else {
    distintervals <- floor(totaldist/resolution)+1
    disttbl$boxcuts <- factor(cut(disttbl$CumulativeDistance.km,distintervals, labels = seq(1,distintervals,1)))
    disttbl$boxcuts <- factor(disttbl$boxcuts,labels = seq(1,nlevels(disttbl$boxcuts),1))

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

    boxlist <- c()
    for(i in 1:nrow(aggdatatbl)) {

      newbox <- osmar::corner_bbox(aggdatatbl$Min.Longitude[i]-offLong,
                                   aggdatatbl$Min.Latitude[i]-offLat,
                                   aggdatatbl$Max.Longitude[i]+offLong,
                                   aggdatatbl$Max.Latitude[i]+offLat)

      boxlist <- cbind(boxlist,newbox)
    }

    colnames(boxlist) <- paste("Group",seq(1,nrow(aggdatatbl),1),sep = "")

  }

  return(list("boxlist"=boxlist, "boxtable"=disttbl))
}


compute_distance <- function(LatList, LongList,timeseq){

  latlong <- data.frame(LatList,LongList,timeseq)
  latlong <- latlong[order(latlong$timeseq),]

  latlong <- latlong %>% mutate(lastLat = lag(LatList),lasttLng = lag(LongList))

  distvec <- geosphere::distHaversine(cbind(latlong$LongList,latlong$LatList),
                                       cbind(latlong$lasttLng,latlong$lastLat))/1000
  distvec[1] <- 0

  timediff <- c(1/3600,diff(as.numeric(latlong$timeseq))/3600)

  disttbl <- data.frame("PointNumber"=seq(1,nrow(latlong),1), "Latitude"=latlong$LatList,
                        "Longitude"=latlong$LongList, "Time"=latlong$timeseq,
                        "Distance.km"=distvec, "CumulativeDistance.km"=cumsum(distvec),
                        "Speed.kmph"=distvec/timediff,
                        "AverageTotalSpeed.kmph"=cumsum(distvec)/cumsum(timediff))

  return(disttbl)
}
