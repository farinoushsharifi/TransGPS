TransGPS Package
================
Farinoush Sharifi

  - [TransGPS: Toward a better analysis of GPS
    data](#transgps-toward-a-better-analysis-of-gps-data)
  - [Installation](#installation)
  - [Usage](#usage)
      - [1. Coordinates Conversion to New
        CRS](#coordinates-conversion-to-new-crs)
      - [2. Coordinates Interpolation](#coordinates-interpolation)
      - [3. Generate Bounding Boxes](#generate-bounding-boxes)
      - [4. Plot Link Speed and Bounding
        Boxes](#plot-link-speed-and-bounding-boxes)
      - [5. Matching The OSM Link IDs to GPS
        Coordinates](#matching-the-osm-link-ids-to-gps-coordinates)
  - [Details](#details)

## TransGPS: Toward a better analysis of GPS data

This package is developed to ease the tedious procedure of GPS data
cleaning and map matching. The functions provided here focus from the
very basic regularization and coordinates conversion of the GPS data
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
## devtools::install_github('farinoushsharifi/TransGPS')
```

## Usage

``` r
library(TransGPS)

### Load Sample Data
data("SampleTransGPS")
```

The first 10 rows of the data can be seen here:

    ##    Latitude Longitude First.Time
    ## 1  31.67514 -106.3265 3:09:56 PM
    ## 2  31.67519 -106.3264 3:09:57 PM
    ## 3  31.67525 -106.3262 3:09:58 PM
    ## 4  31.67530 -106.3261 3:09:59 PM
    ## 5  31.67536 -106.3259 3:10:00 PM
    ## 6  31.67541 -106.3257 3:10:01 PM
    ## 7  31.67547 -106.3256 3:10:02 PM
    ## 8  31.67552 -106.3254 3:10:03 PM
    ## 9  31.67557 -106.3252 3:10:04 PM
    ## 10 31.67562 -106.3251 3:10:05 PM

### 1\. Coordinates Conversion to New CRS

Conversion of the GPS coordinates into a new CRS is the base to initiate
any GPS analysis studies. `convert_crs` takes the initial latitude and
longitude of a list of points and convert to any new CRS. The default
value for the final projection is `"+proj=longlat +ellps=WGS84
+datum=WGS84 +no_defs +towgs84=0,0,0"`, which the CRS corresponding to
OSM data (and the CRS for the `SampleTransGPS` data).

``` r
# define the conversion function inputs
LatList1 <- SampleTransGPS$Latitude
LongList1 <- SampleTransGPS$Longitude
timeseq1 <- SampleTransGPS$First.Time
timeseq1 <- as.POSIXct(timeseq1, format = "%I:%M:%S %p")

InitProj <- "+proj=longlat +ellps=WGS84 
+datum=WGS84 +no_defs +towgs84=0,0,0"
NextProj <- "+init=epsg:28992"

# convert from the initial CRS to a new one
convlist <- convert_crs(LatList1, LongList1, InitProj, NextProj)
```

The first 10 rows of the converted data can be seen here:

    ##    convlist.Latitude convlist.Longitude
    ## 1            6439795           -8105449
    ## 2            6439771           -8105439
    ## 3            6439747           -8105430
    ## 4            6439723           -8105420
    ## 5            6439698           -8105410
    ## 6            6439673           -8105401
    ## 7            6439647           -8105390
    ## 8            6439622           -8105381
    ## 9            6439597           -8105372
    ## 10           6439573           -8105362

### 2\. Coordinates Interpolation

Intepolation of coordinates over a desired time sequence is so useful in
case of irregular time sequence or better visualization of data on
contour maps. Function `interpolate_coords` takes the lists of latitudes
and longitudes over an irregular time sequence and interpolates
coordinates.

``` r
# taking a sample of data for an irregular time sequence
rowIDs <- sample(1:nrow(SampleTransGPS), 200)
SampleTransGPS_ir <- SampleTransGPS[rowIDs, ]
```

There is no need to sort the data befpre using the
`interpolation_coords` function. The first 10 rows can be seen here:

    ##      Latitude Longitude First.Time
    ## 1315 31.83582 -106.3567 3:29:49 PM
    ## 1144 31.82915 -106.3636 3:32:13 PM
    ## 628  31.79017 -106.2679 3:20:23 PM
    ## 339  31.72052 -106.2679 3:15:34 PM
    ## 548  31.77133 -106.2677 3:19:03 PM
    ## 497  31.75896 -106.2676 3:18:12 PM
    ## 1464 31.84105 -106.3241 3:27:03 PM
    ## 1356 31.83911 -106.3231 3:25:56 PM
    ## 653  31.79599 -106.2680 3:20:48 PM
    ## 382  31.73013 -106.2672 3:16:17 PM

This data can be interpolated using function `interpolate_coord`

``` r
# define the interpolation function inputs
LatList2 <- SampleTransGPS_ir$Latitude
LongList2 <- SampleTransGPS_ir$Longitude
timeseq2 <- SampleTransGPS_ir$First.Time
timeseq2 <- as.POSIXct(timeseq2, format = "%I:%M:%S %p")
timeint <- 2  ###2 seconds

# interpolate the GPS coordinates
interp_data <- interpolate_coords(LatList2, LongList2, timeseq2, timeint)
```

The first 10 rows of this interpolated datacan be seen here:

    ##               DateTime Latitude Longitude
    ## 1  2019-05-07 15:10:28 31.67680 -106.3208
    ## 2  2019-05-07 15:10:30 31.67693 -106.3204
    ## 3  2019-05-07 15:10:32 31.67706 -106.3200
    ## 4  2019-05-07 15:10:34 31.67719 -106.3195
    ## 5  2019-05-07 15:10:36 31.67733 -106.3191
    ## 6  2019-05-07 15:10:38 31.67746 -106.3187
    ## 7  2019-05-07 15:10:40 31.67759 -106.3182
    ## 8  2019-05-07 15:10:42 31.67772 -106.3178
    ## 9  2019-05-07 15:10:44 31.67785 -106.3174
    ## 10 2019-05-07 15:10:46 31.67799 -106.3170

### 3\. Generate Bounding Boxes

While working with specific few roads rather than the whole network,
generating regions and boxes around the corridors helps to better focus
on the route, instead of the whole area, and remove unnecessary areas or
roads from the data. So, accessing the OSM data can be easier, less
challenging, and can be done in small chunks. `get_boxes` function
splits the corridors in smaller regions by considering a maximum route
distance of `resolution` within each box. `offLong` and `offLat`are the
margins of each box from the closest coordinate point. The output gives
the ID of box (`boxcuts`) for each point as well as the coordinates of
each box (`boxlists`).

``` r
# define resolutions and margins
resolution = 5
offLong = 0.001
offLat = 0.002

# create boxes
boxout <- get_boxes(LatList1, LongList1, timeseq1)
boxcuts <- boxout$boxtable$boxcuts
boxlist <- boxout$boxlist
```

### 4\. Plot Link Speed and Bounding Boxes

To better study the bounding boxes generated and estimate the
`resolution` and margins, `plot_route` function is provided. It also has
the option to show the average speed of the vehicle on the corridor.

![](README_files/figure-gfm/plot-1.png)<!-- -->

### 5\. Matching The OSM Link IDs to GPS Coordinates

The process of matching the GPS points to the OSM data links or features
can so challenging and inaccurate. The function `match_highway` in this
package helps with this process in multiple ways. Firstly, it splits the
network in few boxes and reduce the tension of accessing the whole OSM
data at once or setting up a server for “planet.osm” data. Secondly, it
considers `k` close points rather than the closest point to the
coordinate. Finally, it tries to keep the route choice
consistency.

``` r
IDList <- match_highway(LatList = LatList1, LongList = LongList1, timeseq = timeseq1, 
    k = 5, boxcuts = boxcuts, boxlist = boxlist)
```

The output of this function is the list of OSM highway link IDs assigned
to each
    point.

    ##  [1] 145425374 145425374 145425374 145425374 145425374 145425374 145425374
    ##  [8] 145425374 145425374 145425374

## Details

For more information on TransGPS Package, please access the package
documentations or vignettes. Please feel free to contact the author at
