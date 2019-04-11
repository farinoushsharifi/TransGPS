### The following example shows how to apply the matchlink, linklocate, and plotroute for a list of GPS coordinates provided from a GPS device.

# 1. Install and load the library
# require(MapMatching)
#
# # 2. Read the GPS coordinates from the table: first column is the Latitude, second column is the longitude
# GPStbl <- read.delim("TripGPS.txt")
#
# # 3. Take a list of GPS coordinates, match each location to the correct link and output the list of link IDs
# GPStbl$LinkID <- matchlink(GPStbl$Latitude, GPStbl$Longitude)
#
# # 4. Find the correct location on the link
# GPStbl$ActualLat <- linklocate(GPStbl$Latitude, GPStbl$Longitude)$ActualLat
# GPStbl$ActualLong <- linklocate(GPStbl$Latitude, GPStbl$Longitude)$ActualLong
#
# # 5. Plot the area with the correct location on the road links
# routeplot <- plotroute(GPStbl$Latitude, GPStbl$Longitude, type="Recorded")
# routeplot <- plotroute(GPStbl$ActualLat, GPStbl$ActualLong, type="Actual")
