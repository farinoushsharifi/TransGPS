#' Coordinates Interpolation
#'
#' Generate list of interpolated latitudes and longitudes by regularization of the
#' time series for a specified increment \code{timeint}
#'
#' @param LatList list of latitudes collected from a GPS recording device
#' @param LongList list of longitudes collected from a GPS recording device
#' @param timeseq list of irregular time series for GPS recording device in
#' format \code{"2000-01-01 00:00:00"}
#' @param timeint desired time step or increment of the time sequence in seconds
#' (default value is \code{1} second)
#'
#' @return \code{interpolate_coords} return a matrix of interpolated regularized
#' time series, interpolated latitude, and interpolated longitude.
#' @export
#'
#' @examples
#' LatList <- c(31.67514,31.675195,31.67525,31.675304,31.675356,31.675408,31.675467,
#' 31.675517,31.675569,31.675623)
#' LongList <- c(-106.326522,-106.326367,-106.326211,-106.326058,-106.325901,
#' -106.325739,-106.325572,-106.32541,-106.325247,-106.325092)
#' timeseq <- c("2019-04-29 15:20:51","2019-04-29 15:21:03","2019-04-29 15:21:06",
#' "2019-04-29 15:21:15","2019-04-29 15:21:17","2019-04-29 15:21:32","2019-04-29 15:21:34",
#' "2019-04-29 15:21:51","2019-04-29 15:22:09","2019-04-29 15:22:36")
#' timeseq <- as.POSIXct(timeseq)
#' interpolate_coords(LatList,LongList,timeseq,1)   #regularize time sequence over 1 second
#' interpolate_coords(LatList,LongList,timeseq,60)   #regularize time sequence over 1 minute

interpolate_coords <- function(LatList,LongList,timeseq,timeint=1) {

  if (is.null(LatList)==TRUE) {
    stop("No latitude list have been reported")
  }

  if (is.null(LongList)==TRUE) {
    stop("No longitude list have been reported")
  }

  if (is.null(timeseq)==TRUE) {
    stop("No time sequence list have been reported")
  }

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

  timeseq <- as.numeric(timeseq)
  fulltimeseq <- seq(timeseq[1],tail(timeseq,1),by=timeint)

  if (length(fulltimeseq)<2) {
      warning("Time interval provided is so large comparing to the overall duration")
  }

  fulltimetbl <- zoo::zoo(cbind(LatList,LongList),timeseq)
  resultlist <- zoo::na.approx(fulltimetbl,xout = fulltimeseq)

  timecol <- as.POSIXct(as.numeric(as.character(rownames(as.matrix(resultlist)))),origin = "1970-01-01 00:00.00 UTC")
  resulttbl <- data.frame(timecol,matrix(resultlist, ncol = 2))
  colnames(resulttbl) <- c("DateTime", "Latitude", "Longitude")

  return(resulttbl)
}
