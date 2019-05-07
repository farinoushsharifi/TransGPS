#' Coordinates Interpolation
#'
#' Interpolate GPS coordinates by regularizing the time series for a given increment \code{timeint}
#'
#' @param LatList A vector of size \emph{n} for latitudes collected from a GPS recording device
#' @param LongList A vector of size \emph{n} for longitudes collected from a GPS recording device
#' @param timeseq A vector of size \emph{n} for irregular time sequence of recorded GPS data in format \code{"\%Y-\%m-\%d \%H:\%M:\%S"}
#' @param timeint (Optional) Value of desired time step or increment in seconds (default value is \emph{1 second})
#'
#' @return \code{\link{interpolate_coords}} return a table of size \emph{n X 3} with columns of \code{("DateTime","Latitude","Longitude")}
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
#' #regularize time sequence over 1 second
#' interpolate_coords(LatList,LongList,timeseq,1)
#'
#' #regularize time sequence over 1 minute
#' interpolate_coords(LatList,LongList,timeseq,60)
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

  if ((any(class(LatList)!="numeric"))|(any(class(LongList)!="numeric"))) {
    stop("Latitude and longitude lists must be numeric")
  }

  if (any(class(timeseq)!=c("POSIXct","POSIXt"))){
    stop("Time Sequense in not in POSIXct or POSIXt format. You can change it using the as.POSIXct or as.POSIXlt functions")
  }

  if ((class(timeint)!="numeric")&(class(timeint)!="integer")) {
    stop("Time increment should be numeric")
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
