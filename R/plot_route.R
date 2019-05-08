#' GPS Route Plot
#'
#' @param LatList A vector of size \emph{n} for latitudes collected from a GPS recording device
#' @param LongList A vector of size \emph{n} for longitudes collected from a GPS recording device
#' @param timeseq A vector of size \emph{n} for irregular time sequence of recorded GPS data in format \code{"\%Y-\%m-\%d \%H:\%M:\%S"}
#' @param boxlist (Optional) A matrix of size \emph{4 X p} of bounding boxes coordinates with rows of \code{("left","bottom","right","top")}; it can be generated from the \code{\link{get_boxes}} function by \code{~boxlist})
#'
#' @return map of the route including the average speed contour and bounding boxes
#' @export
#'
#' @examples
#'
#' LatList <- c(31.67530, 31.67541, 31.67653,
#' 31.67674, 31.67692, 31.67781, 31.67827,
#' 31.67865, 31.68031, 31.68185, 31.68241,
#' 31.68338, 31.68434, 31.68495, 31.68702,
#' 31.68853, 31.68910, 31.68967, 31.69316, 31.69408)
#'
#' LongList <- c(-106.3261, -106.3257, -106.3218,
#' -106.3210, -106.3204, -106.3174, -106.3164,
#' -106.3156, -106.3123, -106.3092, -106.3082,
#' -106.3059, -106.3036, -106.3022, -106.2978,
#' -106.2962, -106.2957, -106.2951, -106.2919,
#' -106.2911)
#'
#' timeseq <- c("2019-04-29 15:09:59",
#' "2019-04-29 15:10:01","2019-04-29 15:10:23",
#' "2019-04-29 15:10:27","2019-04-29 15:10:30",
#' "2019-04-29 15:10:44","2019-04-29 15:10:49",
#' "2019-04-29 15:10:53","2019-04-29 15:11:10",
#' "2019-04-29 15:11:26","2019-04-29 15:11:32",
#' "2019-04-29 15:11:43","2019-04-29 15:11:53",
#' "2019-04-29 15:11:59","2019-04-29 15:12:19",
#' "2019-04-29 15:12:28","2019-04-29 15:12:31",
#' "2019-04-29 15:12:34","2019-04-29 15:12:53",
#' "2019-04-29 15:12:58")
#'
#' timeseq <- as.POSIXct(timeseq)
#'
#' plot_route(LatList,LongList,timeseq)
#'
#' InterpolatedData <- interpolate_coords(LatList,LongList,timeseq,1)
#' LatList2 <- InterpolatedData$Latitude
#' LongList2 <- InterpolatedData$Longitude
#' timeseq2 <- InterpolatedData$DateTime
#'
#' plot_route(LatList2,LongList2,timeseq2)
#'
plot_route <- function(LatList, LongList, timeseq, boxlist=as.matrix(osmar::corner_bbox(min(LongList),min(LatList),max(LongList),max(LatList)))) {

  ### Compatibility checks for the class and length of input data

  if (length(LatList)!=length(LongList)) {
    stop("Latitude and longitude lists do not have the same length")
  }

  if (length(LatList)!=length(timeseq)) {
    stop("Latitude and time sequence lists do not have the same length")
  }

  if ((any(class(LatList)!="numeric"))|(any(class(LongList)!="numeric"))) {
    stop("Latitude and longitude lists must be numeric")
  }

  if (any(class(timeseq)!=c("POSIXct","POSIXt"))){
    stop("Time Sequense in not in POSIXct or POSIXt format. You can change it using the as.POSIXct or as.POSIXlt functions")
  }

  if (ncol(boxlist)<1) {
    stop("bounding box lists do not have any data")
  }

  if ((is.matrix(boxlist)!=TRUE)|(nrow(boxlist)!=4)) {
    stop("bounding box list is not a matrix of 4 rows")
  }

  ### Computing the speed of each link

  plottbl <- compute_distance(LatList,LongList,timeseq)
  plottbl$nextLat <- c(plottbl$Latitude[2:nrow(plottbl)],plottbl$Latitude[nrow(plottbl)])
  plottbl$nextLng <- c(plottbl$Longitude[2:nrow(plottbl)],plottbl$Longitude[nrow(plottbl)])

  ### creating color and plot table

  colpal <- leaflet::colorNumeric(palette = "RdYlGn", domain = plottbl$Speed.kmph)
  plottbl$color <- colpal(plottbl$Speed.kmph)

  ### Creating a map platform

  map <- leaflet::leaflet(plottbl)
  map <- leaflet::addTiles(map,options = leaflet::tileOptions(opacity=0.7))

  ### adding various colors for speed on the links

  for (i in 1:(nrow(plottbl)-1)) {
    map <- leaflet::addPolylines(map = map, data = plottbl,
                        lng = as.numeric(plottbl[i, c('Longitude', 'nextLng')]),
                        lat = as.numeric(plottbl[i, c('Latitude', 'nextLat')]),
                        color = as.character(plottbl[i, c('color')]),
                        opacity=1, fillOpacity =1, weight = 5)
  }

  ### Adding legend and bounding boxes

  map <- leaflet::addLegend(map,position = "bottomright", pal = colpal,
                   values = plottbl$Speed.kmph,title = "Speed",opacity = 1)
  map <- leaflet::addRectangles(map, boxlist[1,],boxlist[2,],boxlist[3,],boxlist[4,],
                  opacity = 0.7, weight = 3, color = "blue",
                  fillColor = "transparent",dashArray = "1",
                  highlightOptions = leaflet::highlightOptions(weight = 3,
                  color = "navy",fillColor="transparent",opacity = 1))
  return(map)
}

### An internal function to compute distance and speed

compute_distance <- function(LatList, LongList,timeseq){

  ### Timeordering input data

  latlong <- data.frame(LatList,LongList,timeseq)
  latlong <- latlong[order(latlong$timeseq),]

  latlong$LastLat <- c(latlong$LatList[1],latlong$LatList[1:(length(latlong$LatList)-1)])
  latlong$LastLng <- c(latlong$LongList[1],latlong$LongList[1:(length(latlong$LongList)-1)])

  ### Computing the distance and time difference

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



