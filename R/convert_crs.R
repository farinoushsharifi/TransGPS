#' Conversion to a New Coordinate Reference System
#'
#' Convert the coordinates of the recorded GPS data in an initial CRS to any desired CRS
#'
#' @param LatList A vector of size \emph{n} for latitudes collected from a GPS recording device
#' @param LongList A vector of size \emph{n} for longitudes collected from a GPS recording device
#' @param InitProj A character string of initial projection arguments; the arguments must be entered exactly as in the \href{https://proj4.org/usage/projections.html}{PROJ.4 documentation}
#' @param NextProj (Optional) A character string of final projection arguments; the arguments must be entered exactly as in the \href{https://proj4.org/usage/projections.html}{PROJ.4 documentation} (if \code{NULL}, the \href{https://www.openstreetmap.org}{OpenStreetMap} projection will be assumed)
#'
#' @return \code{\link{convert_crs}} return a list of 2 vectors of size \emph{n} for the converted latitude and longitude
#' @export
#'
#' @examples
#'
#' LatList <- c(45.86880,45.86887,45.86924,
#' 45.87158,45.87014,45.86923,45.86808,
#' 45.87177,45.87020,45.88010)
#'
#' LongList <- c(7.173500,7.172540,7.171636,
#' 7.180180,7.178070,7.177229,7.175240,
#' 7.181409,7.179299,7.172235)
#'
#' InitProj <- "+init=epsg:4326"
#'
#' #converts the GPS coordinates to OSM CRS
#' convert_crs(LatList,LongList,InitProj)
#'
#' NextProj <- "+init=epsg:4121 +proj=longlat +ellps=GRS80
#' +datum=GGRS87 +no_defs +towgs84=-199.87,74.79,246.62"
#'
#' #converts the GPS coordinates to a selected projection
#' convert_crs(LatList,LongList,InitProj,NextProj)
#'
convert_crs <- function(LatList,LongList,InitProj,NextProj = NULL) {

  ### Compatibilit checks on the class and length of the input

  if (length(LatList)!=length(LongList)) {
    stop("Latitude and longitude lists do not have the same length")
  }

  if ((any(class(LatList)!="numeric"))|(any(class(LongList)!="numeric"))) {
    stop("Latitude and longitude lists must be numeric")
  }

  ### Compatibilit checks on the class of initial projection

  if (class(InitProj)!="character") {
    stop("InitProj should be strings of characters")
  }

  if (is.na(InitProj)==TRUE) {
    stop("InitProj should be a valid projection strings")
  }

  if (class(sp::CRS(InitProj))[1]!="CRS") {
    stop("InitProj is not a valid projection string")
  } else {
    InitCRS <- sp::CRS(InitProj)
  }

  ### Compatibilit checks on the class of next projection and assignment of a new one

  if (is.null(NextProj)!=TRUE) {
    if(class(NextProj)!="character") {
      stop("NextProj should be strings of characters")
    } else {
      if (is.na(NextProj)==TRUE) {
        stop("NextProj should be a valid projection string")
      } else {
        if (class(sp::CRS(NextProj))[1]!="CRS") {
          stop("NextProj is not a valid projection string")
        } else {
          NextCRS <- sp::CRS(NextProj)
        }
      }
    }
  } else {
    NextCRS <- osmar::osm_crs()
  }

  ### Creating a table and linking the coordinates to initial projection

  latlong <- data.frame(lon=LongList, lat=LatList)
  sp::coordinates(latlong) <- c("lon", "lat")
  sp::proj4string(latlong) <- InitCRS

  ### Transforming the coordinates to the secondary projection

  latlong <- sp::spTransform(latlong, NextCRS)

  ### Returning a list of projected coordinates

  return(list("Latitude"=latlong@coords[,2], "Longitude"=latlong@coords[,1]))

}
