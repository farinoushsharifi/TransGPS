

library(leaflet)

GPSplot <- function(LatList,LongList,boxout) {

  pal <- colorNumeric(
    palette = "Accent",
    domain = boxout$boxtable$Latitude)

  m <- leaflet(boxout$boxtable) %>%
    addTiles(options = tileOptions(opacity=0.7)) %>%
    addPolylines(lng=LongList, lat=LatList, fillColor = ~pal(Latitude),
                 opacity=1, fillOpacity =1, weight = 3,color = ~pal(Latitude)) %>%
    addRectangles(boxlist[1,],boxlist[2,],boxlist[3,],boxlist[4,],
                  color="green",fillColor = "green",opacity = 0.5,fillOpacity = 0.5,
                  highlightOptions = highlightOptions(
                    stroke = TRUE,
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE)) %>%
    addLegend("bottomright", pal = pal, values = boxout$boxtable$Speed.km.h,
              title = "Speed",
              opacity = 1) %>% addMeasure(    position = "bottomleft",
                                              primaryLengthUnit = "meters",
                                              primaryAreaUnit = "sqmeters",
                                              activeColor = "#3D535D",
                                              completedColor = "#7D4479")

  return(m)
}

png(paste(i,'test.png', sep = ''))

map <- iris[i * c(1, 2, 3),]

print(barchart(map$Petal.Length,
               main = i,
               horizontal = FALSE,
               col = ifelse(map$Petal.Length > 0,
                            b_clr[1],
                            b_clr[2]),
               ylab = "Impact Value"))
dev.off()

get_boxes_interactive <- function(LatList, LongList, timeseq){

  response <- 2

  while (response==2) {
    resolution <- as.numeric(readline(prompt="Enter the approximate route distance within each box in kilometers: "))
    offLong <- as.numeric(readline(prompt="Enter the longitudal margin around each box: "))
    offLat <- as.numeric(readline(prompt="Enter the latitudal margin around each box: "))

    boxout <- get_boxes(LatList, LongList, timeseq, resolution, offLong,offLat)

    print(GPSplot(LatList,LongList,boxout))

    response <- menu(c("Yes", "No"), title="Are the resolution and margins for the bounding boxes acceptable?")
  }

}
