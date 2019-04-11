### The following example shows how to apply the match.link, link.locate, and plot.route for a list of GPS coordinates provided from a GPS device.

# 1. Install and load the library
require(MapMatching)


# 2. Read the GPS coordinates from the table: first column is the Latitude, second column is the longitude
GPStbl <- read.delim("TripGPS.txt")

# 3. Take a list of GPS coordinates, match each location to the correct link and output the list of link IDs
GPStbl$LinkID <- match.link(GPStbl$Latitude, GPStbl$Longitude)

# 4. Find the correct location on the link
GPStbl$ActualLat <- link.locate(GPStbl$Latitude, GPStbl$Longitude)$ActualLat
GPStbl$ActualLong <- link.locate(GPStbl$Latitude, GPStbl$Longitude)$ActualLong

# 5. Plot the area with the correct location on the road links
routeplot <- plot.route(GPStbl$Latitude, GPStbl$Longitude, type="Recorded")
routeplot <- plot.route(GPStbl$ActualLat, GPStbl$ActualLong, type="Actual")
