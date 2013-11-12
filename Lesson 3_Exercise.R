## ELIAS GEBREMESKEL
## date 11.11.2013
## Kenya Temprature Anomaly
## Lesson 3 EXERCISE
################################################################################
setwd("M:/Geo_script") ## workin Directory
datadir<- c("data") ## Data Source
datadir <- ("M:/Geo_script") 
## Important Packages
library(rasta)
library(raster)
library(maptools)
library(spatstat) 
## Dawnloding the raster data of temprature anamoly
rasterfile <- system.file("extdata", "anom.2000.03.tiff", package ="rasta") # downloading file
temp <- raster(rasterfile) # raster temprature anomaly
plot(temp) # plot temprature
## Down load shapefile of kenya
download.file("http://rasta.r-forge.r-project.org/kenyashape.zip",
              file.path(datadir, "kenyashape.zip")) 
unzip(file.path(datadir, "kenyashape.zip"), exdir = datadir) ## Unzip shapefile
kenya <- readOGR(dsn = datadir, layer = "kenya") ## read shapfile
boundary <- extent(kenya) # extent of the shapefile
## Croping the raster using the shapefile extent
crop <- crop(temp, kenya) 
plot(crop)
plot(kenya, add=TRUE)
# convert to spatial points data frame
temp.spdf<-as(temp,"SpatialPointsDataFrame")
temp.spdf = temp.spdf[!is.na(temp.spdf[[1]]),] # elminate NA values
### Random sample points(RS)
RS <- sampleRandom(temp, size=30, na.rm=FALSE, ext=kenya,
             cells=30, rowcol=TRUE, xy=TRUE, sp=TRUE, asRaster= FALSE)
RS.spdf<-as(RS,"SpatialPointsDataFrame")
plot(RS.spdf,add =TRUE,col ="blue", pch = 19, cex =0.50, pos = 4) # ploting 30 random sampling over the raster temprature
## Assign temprature value for the random sampling by extracting from the pixels of temprature anomaly(POINT DATA) 
assigntemp <- extract(temp, RS.spdf, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
        fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE)
## calculate median and Standard Deviatio
median <-median(assigntemp) ## median of the random value  of temprature anamoly
sd <- sd(assigntemp) ## SD of the random value  of temprature anamoly
## text labelings
addlabel <- invisible(text(RS,labels = as.character(round(assigntemp,digits = 2), cex =0.8, col = "black", font = 0.05)))
        mtext(side = 3, line = 1, "Kenya Temprature Anomaly ", cex = 1.2)
        mtext(side = 2, "Latitude", line = 2.5, cex=1.1)
        mtext(side = 1, "Longitude", line = 2.5, cex=1.1)
        text(42.7, -3.5,paste( "Median = ",round(median, 1), "\nStd.Dev = ", round(sd, 2)),adj=c(0, 0), cex = 0.9) ## text labeling of the functions of SD and Median






