

source_osmdata <- function (LatList, LongList, timeseq, resolution, type, osmlink2, k) {

  if (missing(osmlink)) {

    osmlink <- "https://api.openstreetmap.org/api/0.6/"
  }

  api <- osmsource_api(osmlink)

  boxout <- get_boxes(LatList,LongList,timeseq,resolution)
  boxlist <- boxout$boxlist
  boxtable <- boxout$boxtable
  LatList <- boxtable$Latitude
  LongList <- boxtable$Longitude
  timeseq <- boxtable$Time

  if (type == "Regional") {

    for (i in 1:ncol(boxlist)) {

      mapbox <- osmar::corner_bbox(boxlist[1,i],boxlist[2,i],boxlist[3,i],boxlist[4,i])
      location <- osmar::get_osm(mapbox, source = api)
      # hways <- subset(location, way_ids = osmar::find(location, osmar::way(osmar::tags(k == "highway"))))
      # hways <- osmar::find(hways, osmar::way(osmar::tags(k == "name")))
      # hways <- osmar::find_down(location, osmar::way(hways))
      # hways <- subset(location, ids = hways)
      #
      # nodes_coordtbl <- data.frame(hways$nodes$attrs)

      ats <- location$nodes$attrs
      # distmat <- data.frame(geosphere::distm(cbind(ats$lon, ats$lat),
      #                       cbind(LongList[boxtable$boxcuts==i], LatList[boxtable$boxcuts==i]))/1000,
      #                       row.names = ats$id)
      # colnames(distmat) <- boxtable$PointNumber[boxtable$boxcuts==i]

      nn2(cbind(ats$lon, ats$lat),
          cbind(LongList[boxtable$boxcuts==i], LatList[boxtable$boxcuts==i]),k=15)

      for (j in 1:ncol(distmat)) {
        hwylist <- list(ats[order(distmat[,j]),1][1:k])
        boxtable$Highways[boxtable$boxcuts==i][j] <- hwylist
      }

    }

  }


} else if (type == "Global") {

  if (boxlist!=NULL) {
    stop("For the Global Analysis, only Latitudes and Longitudes should be provided, Not boxlists")
  }
  if ((LatList==NULL)|(LongList==NULL)) {
    stop("For the Global Analysis, Latitude and Longitude lists should be provided")
  }

} else {
  stop("Type of analysis is not provided")
}
}
