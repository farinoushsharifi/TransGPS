GPSplot <- function(boxout,save=FALSE) {

  # pal <- colorRampPalette(green2red(20))
  colpal <- colorNumeric(palette = "RdYlGn", domain = boxout$boxtable$Speed.km.h)
  color <- colpal(boxout$boxtable$Speed.km.h)

  plottbl <- boxout$boxtable %>%
    mutate(nextLat = lead(Latitude),nextLng = lead(Longitude),color = color)

  map <- leaflet(plottbl) %>% addTiles(options = tileOptions(opacity=0.7))

  for (i in 1:(nrow(plottbl)-1)) {
    map <- addPolylines(map = map, data = plottbl,
                        lng = as.numeric(plottbl[i, c('Longitude', 'nextLng')]),
                        lat = as.numeric(plottbl[i, c('Latitude', 'nextLat')]),
                        color = as.character(plottbl[i, c('color')]),
                        opacity=1, fillOpacity =1, weight = 5)
  }

  map <- map %>%
    addRectangles(boxout$boxlist[1,],boxout$boxlist[2,],boxout$boxlist[3,],boxout$boxlist[4,],
                  opacity = 0.7, weight = 3, color = "blue",
                  fillColor = "transparent",dashArray = "1",
                  highlightOptions = highlightOptions(weight = 3,
                  color = "navy",fillColor="transparent",opacity = 1)) %>%
    addLegend(position = "bottomright", pal = colpal, values = boxout$boxtable$Speed.km.h,
              title = "Speed",opacity = 1) %>%
    addMeasure(position = "bottomleft", primaryLengthUnit = "meters",
               primaryAreaUnit = "sqmeters",activeColor = "#3D535D",
               completedColor = "#7D4479")


  return(m)
}

library(mapview)
library(grDevices)
library(colorRamps)
library(RColorBrewer)
library(dplyr)

library(leaflet)
library(htmlwidgets)
library(webshot)

## save html to png
saveWidget(map, "temp.html", selfcontained = FALSE)
webshot("temp.html", file = "Rplot.png",
        cliprect = "viewport")

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
