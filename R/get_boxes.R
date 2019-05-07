#' Generating Bounding Boxes
#'
#' Generate a list of bounding boxes along the GPS recorded data
#'
#' @param LatList A vector of size \emph{n} for latitudes collected from a GPS recording device
#' @param LongList A vector of size \emph{n} for longitudes collected from a GPS recording device
#' @param timeseq A vector of size \emph{n} for irregular time sequence of recorded GPS data in format \code{"\%Y-\%m-\%d \%H:\%M:\%S"}
#' @param resolution (Optional) Approximate value of distance within each bounding box in kilometers
#' @param offLong (Optional) A positive value of bounding box longitudal margin in decimal degrees
#' @param offLat (Optional) A positive value of bounding box latitudal margin in decimal degrees
#'
#' @return \code{\link{get_boxes}} return a list of
#' \itemize{
#'   \item A matrix of size \emph{4 X p} of bounding boxes coordinates; with rows of \code{("left","bottom","right","top")}
#'   \item A table of size \emph{n X 4} with columns of \code{("DateTime","Latitude","Longitude","boxcuts")}; \code{boxcuts} column has \emph{p} levels and shows the corresponding box for each coordinate
#' }
#'
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

get_boxes <- function(LatList, LongList, timeseq, resolution=5, offLong=0.001,offLat=0.001){

  if (length(LatList)!=length(LongList)) {
    stop("Latitude and longitude lists do not have the same length")
  }

  if (length(LatList)!=length(timeseq)) {
    stop("Latitude and time sequence lists do not have the same length")
  }

  if ((!(offLong>0))|(!(offLat>0))) {
    stop("offLong and offLat should be positive decimal degrees")
  }

  if (!(resolution>0)) {
    stop("Resolution should be positive distances in kilometers")
  }

  if ((any(class(LatList)!="numeric"))|(any(class(LongList)!="numeric"))) {
    stop("Latitude and longitude lists must be numeric")
  }

  if (any(class(timeseq)!=c("POSIXct","POSIXt"))){
    stop("Time Sequense in not in POSIXct or POSIXt format. You can change it using the as.POSIXct or as.POSIXlt functions")
  }

  if (class(resolution)!="numeric") {
    stop("Resolution must be numeric")
  }

  if ((class(offLat)!="numeric")|(class(offLong)!="numeric")) {
    stop("offLat and offLong must be numeric")
  }

  disttbl <- compute_distance(LatList,LongList,timeseq)

  LatList <- disttbl$Latitude
  LongList <- disttbl$Longitude
  timeseq <- disttbl$DateTime

  if (any(resolution < disttbl$Distance.km)) {
    warning("Resolution is at least smaller than one interval distance")
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

    Latmax <- stats::aggregate(disttbl$Latitude, list(disttbl$boxcuts), max)$x
    Latmin <- stats::aggregate(disttbl$Latitude, list(disttbl$boxcuts), min)$x
    Longmax <- stats::aggregate(disttbl$Longitude, list(disttbl$boxcuts), max)$x
    Longmin <- stats::aggregate(disttbl$Longitude, list(disttbl$boxcuts), min)$x
    Timemax <- stats::aggregate(disttbl$DateTime, list(disttbl$boxcuts), max)$x
    Timemin <- stats::aggregate(disttbl$DateTime, list(disttbl$boxcuts), min)$x
    Distsum <- stats::aggregate(disttbl$Distance.km, list(disttbl$boxcuts), sum)$x

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

  boxtable <- disttbl[,c("DateTime","Latitude","Longitude","boxcuts")]
  boxtable$boxcuts <- as.numeric(boxtable$boxcuts)
  return(list("boxlist"=as.matrix(boxlist), "boxtable"=boxtable))
}


compute_distance <- function(LatList, LongList,timeseq){

  latlong <- data.frame(LatList,LongList,timeseq)
  latlong <- latlong[order(latlong$timeseq),]

  latlong$LastLat <- c(LatList[1],LatList[1:(length(LatList)-1)])
  latlong$LastLng <- c(LongList[1],LongList[1:(length(LongList)-1)])

  distvec <- geosphere::distHaversine(cbind(latlong$LongList,latlong$LatList),
                                       cbind(latlong$LastLng,latlong$LastLat))/1000

  timediff <- c(1/3600,diff(as.numeric(latlong$timeseq))/3600)

  disttbl <- data.frame("PointNumber"=seq(1,nrow(latlong),1), "Latitude"=latlong$LatList,
                        "Longitude"=latlong$LongList, "DateTime"=latlong$timeseq,
                        "Distance.km"=distvec, "CumulativeDistance.km"=cumsum(distvec),
                        "Speed.kmph"=distvec/timediff,
                        "AverageTotalSpeed.kmph"=cumsum(distvec)/cumsum(timediff))

  return(disttbl)
}
