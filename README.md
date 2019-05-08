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
devtools::install_github("farinoushsharifi/TransGPS")
```

## Usage

``` r
library(TransGPS)
```

### 1\. Coordinates Conversion to New CRS

Conversion of the GPS coordinates into a new CRS is the base to initiate
any GPS analysis studies. `convert_crs` takes the initial latitude and
longitude of a list of points and convert to any new CRS. The default
value for the final projection is `"+proj=longlat +ellps=WGS84
+datum=WGS84 +no_defs +towgs84=0,0,0"`, which the CRS corresponding to
OSM data (and the CRS for the `SampleTransGPS` data).

``` r
# convert from the initial CRS projection to a new one
convlist <- convert_crs(LatList = LatList, LongList = LongList, InitProj = InitProj, 
    NextProj = NextProj)
```

### 2\. Coordinates Interpolation

Intepolation of coordinates over a desired time sequence is so useful in
case of irregular time sequence or better visualization of data on
contour maps. Function `interpolate_coords` takes the lists of latitudes
and longitudes over an irregular time sequence and interpolates
coordinates.

``` r
# interpolate the GPS coordinates over 3 seconds
interp_data <- interpolate_coords(LatList = LatList, LongList = LongList, 
    timeseq = timeseq, timeint = 3)
```

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
# create bounding boxes
boxout <- get_boxes(LatList = LatList, LongList = LongList, timeseq = timeseq, 
    resolution = resolution, offLong = offLong, offLat = offLat)
boxcuts <- boxout$boxtable$boxcuts
boxlist <- boxout$boxlist
```

### 4\. Plot Link Speed and Bounding Boxes

To better study the bounding boxes generated and estimate the
`resolution` and margins, `plot_route` function is provided. It also has
the option to show the average speed of the vehicle on the corridor.

``` r
# plot the link speeds and bounding boxes
plot_route(LatList = LatList, LongList = LongList, timeseq = timeseq, boxlist = boxlist)
```

### 5\. Matching The OSM Link IDs to GPS Coordinates

The process of matching the GPS points to the OSM data links or features
can so challenging and inaccurate. The function `match_highway` in this
package helps with this process in multiple ways. Firstly, it splits the
network in few boxes and reduce the tension of accessing the whole OSM
data at once or setting up a server for “planet.osm” data. Secondly, it
considers `k` close points rather than the closest point to the
coordinate. Finally, it tries to keep the route choice consistency.

``` r
# match each points to an OSM highway
IDList <- match_highway(LatList = LatList, LongList = LongList, timeseq = timeseq, 
    k = 5, boxcuts = boxcuts, boxlist = boxlist)
```

## Details

For more information on TransGPS Package, please access the package
documentations or vignettes. Please feel free to contact the author.
