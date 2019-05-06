

match_highway <- function (LatList, LongList, k,
                           boxcuts = get_boxes(LatList,LongList,timeseq,resolution,offLong,offLat)$boxtable$boxcuts,
                           boxlist = get_boxes(LatList,LongList,timeseq,resolution,offLong,offLat)$boxlist,
                           resolution = 10, offLong = 0.001, offLat = 0.001,
                           osmlink = "https://api.openstreetmap.org/api/0.6/") {

  if (length(LatList)!=length(LongList)) {
    stop("Latitude and longitude lists do not have the same length")
  }

  if (length(LatList)!=length(boxcuts)) {
    stop("Latitude and bounding box cut lists do not have the same length")
  }

  if ((k!= round(k))|(k<0)|(k==0)){
    stop("k is not acceptable, please enter a positive integer")
  }

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


      nnmat <- RANN::nn2(cbind(hwaynodetbl$lon, hwaynodetbl$lat),
                         cbind(LongList[boxcuts==i], LatList[boxcuts==i]),k)
      nnmat <- matrix(hwaynodetbl[matrix(nnmat$nn.idx,ncol = k),1],ncol = k)

      for(j in 1:nrow(nnmat)){
        hwaylist <- unique(osmar::find_up(hwaysdata, node(nnmat[j,]))$way_ids)
        count <- length(hwaylist)
        if (count>k) {
          count <- k
        }
        highway[boxcuts==i,][j,1:k] <- hwaylist[1:k]
      }
  }


  highway <- cbind("PointNumber"=seq(1,length(LatList)),as.data.frame(highway))

  highway <- highway%>%mutate(k1lag = lag(V1), k1lead = lead(V1))
  PointID <- highway[(highway$V1!=highway$k1lag)&(highway$k1lag==highway$k1lead),]$PointNumber
  PointID <- PointID[!is.na(PointID)]

  highway$V1[PointID] <- highway$k1lag[PointID]

  highway <- list(highway$V1)

  return("HighwayLinkID"=highway)
}



