#' Coordinates Map Matching
#'
#' Snap a set of GPS coordinates in WGS84 to the \href{https://www.openstreetmap.org}{OpenStreetMap (OSM)} data highways. This function facilitates the common process in two ways:
#' \itemize{
#'   \item Dividing the whole region into bounding boxes, which allows using the OSM data through API sourcing instead of setting up an OSM data server
#'   \item Implementation of a fast K-Nearest Neighbor method to find the closest K links to each GPS coordinate
#' }
#'
#' @param LatList A vector of size \emph{n} for latitudes collected from a GPS recording device
#' @param LongList A vector of size \emph{n} for longitudes collected from a GPS recording device
#' @param timeseq A vector of size \emph{n} for irregular time sequence of recorded GPS data in format \code{"\%Y-\%m-\%d \%H:\%M:\%S"}
#' @param k Value of maximum number of close highways; to consider for the KNN analysis
#' @param boxcuts (Optional) A vector of size \emph{n} with \emph{p} levels for the bounding box split of GPS coordinates; it can be generated from the \code{\link{get_boxes}} function by \code{~boxtable$boxcuts})
#' @param boxlist (Optional) A matrix of size \emph{4 X p} of bounding boxes coordinates with rows of \code{("left","bottom","right","top")}; it can be generated from the \code{\link{get_boxes}} function by \code{~boxlist})
#' @param resolution (Optional) Approximate value of distance within each bounding box in kilometers; to be used for \code{\link{get_boxes}}
#' @param offLong (Optional) A positive value of bounding box longitudal margin in decimal degrees; to be used for \code{\link{get_boxes}}
#' @param offLat (Optional) A positive value of bounding box latitudal margin in decimal degrees; to be used for \code{\link{get_boxes}}
#' @param osmlink (Optional) The API link for the \href{https://www.openstreetmap.org}{OpenStreetMap} data
#'
#' @return \code{\link{match_highway}} return a vector of size \emph{n} for highway link IDs based on \href{https://www.openstreetmap.org}{OpenStreetMap}
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
#' k=5
#'
#' resolution <- 0.1
#' offLong=0.001
#' offLat=0.002
#'
#' match_highway(LatList, LongList, timeseq, k,
#' resolution = resolution,offLong = offLong, offLat = offLat)
#'
#' boxcuts <- c(1,1,1,2,2,3,3,3,4,4)
#' boxlist <- cbind(c(-106.32752,31.67414,-106.28746,31.69790),
#' c(-106.26872,31.73050,-106.26627,31.77233),
#' c(-106.26915,31.77057,-106.26671,31.81236),
#' c(-106.36679,31.81582,-106.34707,31.84163))
#'
#' match_highway(LatList, LongList, timeseq,
#' k, boxcuts=boxcuts, boxlist=boxlist)

match_highway <- function (LatList, LongList, timeseq, k,
                           boxcuts = as.numeric(get_boxes(LatList,LongList,timeseq,resolution,offLong,offLat)$boxtable$boxcuts),
                           boxlist = as.matrix(get_boxes(LatList,LongList,timeseq,resolution,offLong,offLat)$boxlist),
                           resolution = 5, offLong = 0.001, offLat = 0.001,
                           osmlink = "https://api.openstreetmap.org/api/0.6/")  {

  if(missing(boxcuts) & missing(boxlist)) {
    boxout <- get_boxes(LatList,LongList,timeseq,resolution,offLong,offLat)
    boxcuts = as.numeric(boxout$boxtable$boxcuts)
    boxlist = as.matrix(boxout$boxlist)

    LatList <- boxout$boxtable$Latitude
    LongList <- boxout$boxtable$Longitude
    timeseq <- boxout$boxtable$DateTime

  } else if (!(identical(missing(boxcuts),missing(boxlist)))) {
    stop("boxcuts and boxlists should be both given to the function or let the function generate it to assure the compatibility")
  } else {
    latlong <- data.frame(LatList,LongList,timeseq, boxcuts)
    latlong <- latlong[order(latlong$timeseq),]

    LatList <- latlong$LatList
    LongList <- latlong$LongList
    timeseq <- latlong$timeseq
    boxcuts <- latlong$boxcuts
  }

  if (length(LatList)!=length(LongList)) {
    stop("Latitude and longitude lists do not have the same length")
  }

  if (length(LatList)!=length(timeseq)) {
    stop("Latitude and time sequence lists do not have the same length")
  }

  if ((k!= round(k))|(k<0)|(k==0)){
    stop("k is not acceptable, please enter a positive integer")
  }

  if (length(LatList)!=length(boxcuts)) {
    stop("Latitude and bounding box cut lists do not have the same length")
  }

  if (ncol(boxlist)<1) {
    stop("bounding box lists do not have any data")
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

  if (any(class(boxcuts)!="numeric")) {
    stop("bounding box cuts must be numeric")
  }

  if ((is.matrix(boxlist)!=TRUE)|(nrow(boxlist)!=4)) {
    stop("bounding box list is not a matrix of 4 rows")
  }

  if ((nlevels(factor(boxcuts))!=ncol(boxlist))) {
    stop("bounding box list and box cuts are not compatible in terms of number of boxes")
  }

  if (class(resolution)!="numeric") {
    stop("Resolution must be numeric")
  }

  if ((class(offLat)!="numeric")|(class(offLong)!="numeric")) {
    stop("offLat and offLong must be numeric")
  }

  latlong <- data.frame(LatList,LongList,timeseq, boxcuts)
  latlong <- latlong[order(latlong$timeseq),]

  LatList <- latlong$LatList
  LongList <- latlong$LongList
  timeseq <- latlong$timeseq
  boxcuts <- latlong$boxcuts

  api <- osmar::osmsource_api(osmlink)
  highway <-  matrix(0,nrow=length(LatList),ncol=k)


  for (i in 1:ncol(boxlist)) {

      mapbox <- osmar::corner_bbox(boxlist[1,i],boxlist[2,i],boxlist[3,i],boxlist[4,i])
      location <- osmar::get_osm(mapbox, source = api)

      hwaysdata <- subset(location, way_ids = osmar::find(location, osmar::way(osmar::tags(k == "highway"))))
      hways <- osmar::find(hwaysdata, osmar::way(osmar::tags(k == "name")))
      hways <- osmar::find_down(location, osmar::way(hways))
      hwaysdata <- subset(location, ids = hways)

      hwaynodetbl <- hwaysdata$nodes$attrs

      if (k>nrow(hwaynodetbl)) {
        warning("k is bigger than the available nodes")
        k <- nrow(hwaynodetbl)
      }
      if ((nrow(hwaynodetbl)==0)|(length(LongList[boxcuts==i])==0)) {
        warning("There is at least one point with no nodes close")
        next()
      }

      nnmat <- RANN::nn2(cbind(hwaynodetbl$lon, hwaynodetbl$lat),
                         cbind(LongList[boxcuts==i], LatList[boxcuts==i]),k)
      nnmat <- matrix(hwaynodetbl[matrix(nnmat$nn.idx,ncol = k),1],ncol = k)

      for(j in 1:nrow(nnmat)){
        hwaylist <- unique(osmar::find_up(hwaysdata, osmar::node(nnmat[j,]))$way_ids)
        count <- length(hwaylist)
        if (count>k) {
          count <- k
        }
        highway[boxcuts==i,][j,1:k] <- hwaylist[1:k]
      }
  }

  linklist <- highway[,1]

  k1lag <- c(highway[1,1],highway[1:(nrow(highway)-1),1])
  k1lead <- c(highway[2:nrow(highway),1],highway[nrow(highway),2])


  for(ind in 1:nrow(highway)) {
    if(is.na(highway[ind,2])==FALSE) {
      if ((highway[ind,1]!=k1lag[ind])&
          (k1lag[ind]==k1lead[ind])) {
        if (any(highway[ind,2:k][!is.na(highway[ind,2:k])]==k1lag[ind])) {
          linklist[ind] <- k1lag[ind]
        }
      }
    }
  }

  return("HighwayLinkID"=linklist)
}



