
library(rgdal) 
library(rgeos) 
library(rasta) 
library(sp) 
library(ggplot2)
library(rasta)
setwd("M:/Geo_script")

############################################
pnt1_xy <- cbind(5.6660, 51.9872)
pnt2_xy <- cbind(5.6643, 51.9668)
coords <- rbind(pnt1_xy, pnt2_xy)
prj_string_WGS <- CRS("+proj=longlat +datum=WGS84")
mypoints <- SpatialPoints(coords, proj4string=prj_string_WGS)

class(mypoints)
str(mypoints)
mydata <- data.frame(cbind(id = c(1,2),
                           Name = c("my description 1",
                                    "my description 2")))
   mypointsdf <- SpatialPointsDataFrame(
        coords, data = mydata,
         proj4string=prj_string_WGS) 
class(mypointsdf)
names(mypointsdf)
str(mypointsdf)
spplot(mypointsdf, zcol="Name", col.regions = c("red", "blue"),
       xlim = bbox(mypointsdf)[1, ]+c(-0.01,0.01),
       ylim = bbox(mypointsdf)[2, ]+c(-0.01,0.01),
       scales= list(draw = TRUE))
                     
                     ######## LINES Data
simple_line <- Line(coords)
lines_obj <- Lines(list(simple_line), "1")
spatlines <- SpatialLines(list(lines_obj), proj4string=prj_string_WGS)
line_data <- data.frame(Name = "straight line", row.names="1")
mylinesdf <- SpatialLinesDataFrame(spatlines, line_data) 
 class(mylinesdf)
str(mylinesdf)
   spplot(mylinesdf, col.regions = "blue",
   xlim = bbox(mypointsdf)[1, ]+c(-0.01,0.01),
    ylim = bbox(mypointsdf)[2, ]+c(-0.01,0.01),
    scales= list(draw = TRUE))                     
###### Lines convert in to KML
writeOGR(mypointsdf, file.path("M:/Geo_script/Lesson4","mypointsGE.kml"),
                               "mypointsGE", driver="KML", overwrite_layer=TRUE)  
 writeOGR(mylinesdf, file.path("M:/Geo_script/Lesson4","mylinesGE.kml"),
                             "mylinesGE", driver="KML", overwrite_layer=TRUE)   
## Digitaizing in GE 
 myroute <- readOGR(file.path("M:/Geo_script/Lesson4","route.kml"), "route.kml")
 myroute@proj4string <- prj_string_WGS
names(myroute)  
myroute$Description <- NULL
#myroutedf <- rbind(mylinesdf, myroute)
spplot(myroute, col.regions = "blue",
xlim = bbox(mylinesdf)[1, ]+c(-0.01,0.01),
ylim = bbox(mylinesdf)[2, ]+c(-0.01,0.01),
scales= list(draw = TRUE))
writeOGR(myroute, file.path("Lesson4", "myrouteCopy.kml"),
                      "myrouteGE", driver="KML", overwrite_layer=TRUE)
#### Projections
 prj_string_RD <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889
+k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.2369,50.0087,465.658,
 -0.406857330322398,0.350732676542563,-1.8703473836068,4.0812 +units=m +no_defs")
mylinesRD <- spTransform(mylinesdf, prj_string_RD)
## Computing a length
  mylinesdf$length <- gLength(mylinesRD, byid=T)
 writeOGR(mylinesRD, file.path("Lesson4", "mylinesRD.kml"),
                              "mylinesRDGE", driver="KML", overwrite_layer=TRUE)   
########POLYGONES
mypointsRD <- spTransform(mypointsdf, prj_string_RD)
pnt1_rd <- coordinates(mypointsRD)[1,]
pnt2_rd <- coordinates(mypointsRD)[2,]  
ang <- pi*0:200/100
            circle1x <- pnt1_rd[1] + cos(ang) * mylinesdf$length[1]
                     circle1y <- pnt1_rd[2] + sin(ang) * mylinesdf$length[1]
                     circle2x <- pnt2_rd[1] + cos(ang) * mylinesdf$length[1]
                     circle2y <- pnt2_rd[2] + sin(ang) * mylinesdf$length[1]
                     circle1 <- Polygons(list(Polygon(cbind(circle1x, circle1y))),"1")
                     circle2 <- Polygons(list(Polygon(cbind(circle2x, circle2y))),"2")
                     spcircles <- SpatialPolygons(list(circle1, circle2), proj4string=prj_string_RD)
                     circledat <- data.frame(mypointsRD@data, row.names=c("1", "2"))
                     circlesdf <- SpatialPolygonsDataFrame(spcircles, circledat)
 ## BUFFERing
                     buffpoint <- gBuffer(mypointsRD[1,], width=mylinesdf$length[1], quadsegs=25)
                     mydiff <- gDifference(circlesdf[1,], buffpoint)
                     gArea(mydiff)
                     myintersection <- gIntersection(circlesdf[1,], buffpoint)
                     gArea(myintersection)
                     print(paste("The difference in area =", round(100 * gArea(mydiff) /
                                                                      gArea(myintersection),3), "%"))
                     spplot(circlesdf, zcol="Name", col.regions=c("gray60", "gray40"),
                            sp.layout=list(list("sp.points", mypointsRD, col="red", pch=19, cex=1.5),
                                           list("sp.lines", mylinesRD, lwd=1.5)))
   ## PoTATO Harvesting
  download.file("http://rasta.r-forge.r-project.org/kroonven.csv", "kroonven.csv")
  borne_data = read.table("kroonven.csv", sep = ",", header = TRUE)
  names(borne_data)
 coordinates(borne_data) <- c("lon.degr.","lat.degr.")
 borne_data@proj4string <- prj_string_WGS
  # transform to planar coordinate system
 all_rd <- spTransform(borne_data, prj_string_RD)
           dimnames(all_rd@coords)[[2]] <- c("x", "y")
 # plot the point data
          spplot(all_rd, zcol="yield.ton.ha.", colorkey=T, zlim=c(0,100),
                 col.regions=c(bpy.colors(25), rep("yellow", 75)), pch=19,
                 cex=0.25, main="Recorded yields ton/ha")                
 # add datetime attribute using POSIX class 
 all_rd$datetime <- as.POSIXct(paste(paste(all_rd$year, all_rd$month, all_rd$day,
    sep="-"), paste(all_rd$hr, all_rd$min, all_rd$sec,
     sep=":")), tz="Europe/Andorra")
head( all_rd$datetime)
  # coerce spatial data frame to data frame
     all_rd <- as.data.frame(all_rd)
     head( all_rd,5)
                     # make sure points are temporally ordered
 all_rd <- all_rd[order(all_rd$datetime),]
  head(all_rd,5)  
##### Load RASTA                               
 library(rasta)
sp_lines_df <- CreateHarvestTracks(all_rd, prj_string_RD,maxdist = 10)
names(sp_lines_df)
spplot(sp_lines_df, zcol="ID", lwd=1.5, col.regions =
         bpy.colors(nlevels(sp_lines_df$ID)))

#######################################################################
################################
### Assignment 
## creating 3 m geometric buffer swath of the the truck lines for harvesting
##  flatline curve block.
 sp_polys <- gBuffer(sp_lines_df,byid=F, width=0.5*sp_lines_df$width,
                       capStyle="FLAT") 
# fill small holes by swelling and shrinking 
sp_polys <- gBuffer(sp_polys, byid=T,id=rownames(sp_polys), width = 2.0) 
sp_polys <- gBuffer(sp_polys, byid=T,id=rownames(sp_polys), width = -2.0)
## create a Spatial Polygons Dataframe of the swath truck(block) of the harvesting truck
sp_polys_df <- SpatialPolygonsDataFrame(sp_polys, sp_lines_df@data)

# Remove line segments that are within already harvested swaths using
   # gDifference
   tmp_lines <- sp_lines_df@lines # just a list with geometries
 for (i in 2:length(sp_lines_df)){
   tmline <- sp_lines_df[i,]$datim
   for (j in 1:(i-1)){
    tmpoly <- sp_polys_df[j,]$datim
     if (difftime(tmline, tmpoly, units = "secs") > 0){
      tmp_line <- SpatialLines(tmp_lines[i], prj_string_RD)
       if (gIntersects(tmp_line, sp_polys_df[j,])){
         # compute difference
         tmp_lines[[i]] <- gDifference(tmp_line, sp_polys_df[j,])@lines[[1]]
         tmp_lines[[i]]@ID <- sp_lines_df[i,]@lines[[1]]@ID
        }
       }
     }
   }
 tmp_lines <- SpatialLines(tmp_lines, prj_string_RD)
 cln_lines_df <- SpatialLinesDataFrame(tmp_lines, sp_lines_df@data)

## FuNCTION
#buffering the  cleaned lines in order to create harvest blocks.
cln_lines_poly <- gBuffer(sp_lines_df,byid=T, width=0.5*sp_lines_df$width,
                    capStyle="FLAT")
#Fill small holes of the blocks by swelling and shrinking;
cln_lines_poly <- gBuffer(cln_lines_poly, byid=T,id=rownames(cln_lines), width = 2.0)
cln_lines_poly <- gBuffer(cln_lines_poly, byid=T,id=rownames(cln_lines), width = -2.0)
cln_lines_poly_df <- SpatialPolygonsDataFrame(cln_lines_poly, cln_lines_poly@data)
spplot(cln_lines_poly_df , zcol="ID", lwd=1.5, col.regions =
         bpy.colors(nlevels(cln_lines_poly_df$ID)))
#compute yield per hectare for the blocks and addinng the value to the spatial polygones
yieldperha <- cln_lines_poly_df$loads/(gArea(cln_lines_poly_df) / 10000)
cln_lines_poly_df$tonha <- yieldperha
spplot(cln_lines_poly_df, zcol="tonha",lwd=1.5)
# Export the spatial polygons data frame to display polygon boundaries in Google Earth
 prj_string_WGS <- CRS("+proj=longlat +datum=WGS84")
cln_linestoGE <- spTransform(cln_lines_poly_df, prj_string_WGS)
# Write in GE
writeOGR(cln_linestoGE, file.path("lesson4", "cln_linestoGE.kml"),"yieldha", driver="KML", overwrite_layer=TRUE)

## End

