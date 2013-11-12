## ELIAS GEBREMESKEL
## DATE 11.11.2013
## Creat Re-producible map
###############################################################################
setwd("M:/Geo_script") ## workin Directory
datadir<- c("data") ## Data Source
datadir <- ("M:/Geo_script")
library(rasta)
library(raster)
  create_map <- function(country = "NLD", level = 2 ){  
  data <- raster::getData("GADM", country = country, level = level)
  map <- data[data$NAME_1 == "Gelderland",]
  plot(map, bg = "white", axes=T)
  plot(map, lwd = 3, border = "black", add=T)
  grid()
  box()
  invisible(text(getSpPPolygonsLabptSlots(province),
                 labels = as.character(province$NAME_2), cex = 0.7, col = "blue", font = 0.5))
  mtext(side = 3, line = 1, "Provincial Map of Gelderland", cex = 2)
  mtext(side = 1, "Longitude", line = 2.5, cex=1.1)
  mtext(side = 2, "Latitude", line = 2.5, cex=1.1)
}
create_map("NLD",2)
#### 