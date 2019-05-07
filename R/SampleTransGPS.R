#' Sample GPS Data for an El Paso trip
#'
#' Data is from a study in El Paso. It shows movement of a vehicle
#' in 1546 observations. This data includes tha latitude and longitude
#' of the vehicle at each time stamp.
#'
#' @docType data
#'
#' @usage data(SampleTransGPS)
#'
#' @format An object of class \code{"data.frame"}.
#'
#' @keywords datasets
#'
#'
#' @examples
#' data(SampleTransGPS)
#' times <- SampleTransGPS$First.Time
#' plot(SampleTransGPS$Longitude,SampleTransGPS$Latitude)

"SampleTransGPS"
