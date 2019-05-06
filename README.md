TransGPS Package
================
Farinoush Sharifi
May 2019

## TransGPS: Toward a better analysis of GPS data

This package is developed to ease the tedious procedure of GPS data
cleaning and map matching. The functions provided here focus from the
very basic regularization and interpolation of the GPS coordinates
toward creating bounding boxes and accessing the well-known
[OpenStreetMap(OSM)](https://www.openstreetmap.org) data for the purpose
of map matching.

Creating bounding boxes allows the users to access OSM data without any
challenges of developing a data server. This package also provides an
interactive platform for the users to find the desired bounding boxes on
the map. Finally, the user can find the best link match for each GPS
coordinate.

## Installation

``` r
##devtools::install_github("farinoushsharifi/TransGPS")
```

## Usage

``` r
library(TransGPS)
```

### Coordinates Interpolation

``` r
LatList <- c(31.67514, 31.675195, 31.67525, 31.675304, 31.675356, 
    31.675408, 31.675467, 31.675517, 31.675569, 31.675623)

LongList <- c(-106.326522, -106.326367, -106.326211, -106.326058, 
    -106.325901, -106.325739, -106.325572, -106.32541, -106.325247, 
    -106.325092)

timeseq <- c("2019-04-29 15:20:51", "2019-04-29 15:21:03", "2019-04-29 15:21:06", 
    "2019-04-29 15:21:15", "2019-04-29 15:21:17", "2019-04-29 15:21:32", 
    "2019-04-29 15:21:34", "2019-04-29 15:21:51", "2019-04-29 15:22:09", 
    "2019-04-29 15:22:36")

timeseq <- as.POSIXct(timeseq)

# regularize time sequence over 1 second
interp_sec <- interpolate_coords(LatList, LongList, timeseq, 
    1)

# regularize time sequence over 1 minute
interp_min <- interpolate_coords(LatList, LongList, timeseq, 
    60)
```

### Generate Bounding Boxes

![](README_files/figure-gfm/pressure2-1.png)<!-- -->

### Iteration Over Bounding Boxes

![](README_files/figure-gfm/pressure3-1.png)<!-- -->

### Matching The OSM Link IDs to GPS Coordinates

![](README_files/figure-gfm/pressure4-1.png)<!-- -->
