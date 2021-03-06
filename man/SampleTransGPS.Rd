% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SampleTransGPS.R
\docType{data}
\name{SampleTransGPS}
\alias{SampleTransGPS}
\title{Sample GPS Data for an El Paso trip}
\format{An object of class \code{"data.frame"}.}
\usage{
data(SampleTransGPS)
}
\description{
Data is from a study in El Paso. It shows movement of a vehicle
in 1546 observations. This data includes tha latitude and longitude
of the vehicle at each time stamp.
}
\examples{
data(SampleTransGPS)
times <- SampleTransGPS$First.Time
plot(SampleTransGPS$Longitude,SampleTransGPS$Latitude)
}
\keyword{datasets}
