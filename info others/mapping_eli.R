---
#title: "Sampling stations"
#author: "VLIZ"
#date: "6-8 December 2017"
#output: html_document
---
#In this topic we will plot the sample region of the Lifewatch - Jerico Next cruise of May 8-12 2017.
#Besides the generic R functions, there are many specialized [packages](https://cran.r-project.org/web/views/Spatial.html) to visualize and analyze spatial data.

#Goal of this document is to help you visualize the spatial aspect of the biotic and abiotic parameters measured during the cruise.
  
##### 1. Making a map of the Southern North Sea, English Channel and the Thames estuary ####
  
#Combining **ggplot** with the R packages that facilitate the handling of geospatial data, as [maptools](https://cran.r-project.org/web/packages/maptools/maptools.pdf), [rgdal](https://cran.r-project.org/web/packages/rgdal/rgdal.pdf), [rgeos](https://cran.r-project.org/web/packages/rgeos/rgeos.pdf) and [mapproj](https://cran.r-project.org/web/packages/mapproj/mapproj.pdf), enables the construction of simple and complex maps.


library(maptools)
library(rgdal)
library(rgeos)
library(mapproj)
library(ggplot2)
library(dplyr)
library(base)
library(lubridate)


Using these packages we will construct a simple map of the sampled region (Southern Bight, Thames estuary and Eastern English Channel).

#### 1.1 Read shapefiles ####

#A common data format for geospatial data is the shapefile. In a shapefile, geospatial data are described as points, lines and polygons and can be analysed as such in GIS software. [Marine Regions](http://www.marineregions.org/) provides access to many shapefiles through the [gazetteer](http://www.marineregions.org/gazetteer.php?p=search). The folder **Mapping** contains some useful shapefiles.

list.files("Mapping")


#The world_bay_gulf folder and file contains the shapefile of the [Southern Bight](http://www.marineregions.org/gazetteer.php?p=details&id=2399). The seavox_v16 folder and file contains the shapefile of the [Thames estuary](http://www.marineregions.org/gazetteer.php?p=details&id=24195).
#The iho folder and file contains the shapefile of the [English Channel](http://www.marineregions.org/gazetteer.php?p=details&id=2389).
#Below you can find a ways to import it into the R Environment. The recommended **readOGR()** recognizes the type of spatial data automatically.

bight <- readOGR("Mapping/world_bay_gulf", layer = "world_bay_gulf")
channel <-readOGR("Mapping/iho", layer = "iho")
thames <-readOGR("Mapping/seavox_v16", layer = "seavox_v16")


#Looking at the class, structure and plot of the shapefile allows to better understand the data type. Output is not given in this document due its large size

class(bight)
class(channel)
class(thames)
str(bight)
str(thames)
str(channel)
plot(bight)
plot(channel)
plot(thames)

#### 1.2 Plot spatial data in ggplot ####

#The **fortify()** function allows to easily convert an object of a spatial class to a regular dataframe. Though this transformation is not necessary for making a map in R, it is vital to map data in **ggplot**.

bightfort <- fortify(bight)
str(bightfort)
thamesfort <- fortify(thames)
channelfort <- fortify(channel)

#Now, let's start exploring these spatial data with ggplot! In order to plot the Southern Bight, Thames estuary and English Channel, we want a plot that connects the different points in our fortified data frame.

ggplot() + geom_path(data = bightfort, aes(x = long, y = lat, group=group))+
geom_path(data = thamesfort, aes(x = long, y = lat, group=group))+
geom_path(data = channelfort, aes(x = long, y = lat, group=group))


#### 1.3 Add layers to the plot ####
 
#Now, we can add more information on this plot, for example the Economic Exclusive zones for UK, The Netherlands, France and Belgium.

Beez <- readOGR("Mapping/Belgian eez/eez.shp")
Feez <- readOGR("Mapping/French eez/eez.shp")
Eeez <- readOGR("Mapping/United Kingdom eez/eez.shp")
Deez <- readOGR("Mapping/Dutch eez/eez.shp")
Beezfort <- fortify(Beez)
Feezfort <- fortify(Feez)
Eeezfort <- fortify(Eeez)
Deezfort <- fortify(Deez)

#Now, we can put the plot with all its layers and make a function of it so it can easily be used later on 

plot_map <- function() {
ggplot() + 
coord_map(xlim = c(0,4), ylim = c(50,52.5)) +
geom_polygon(aes(x=long, y=lat, group=group), data = channelfort, fill = "white") +
geom_polygon(aes(x=long, y=lat, group=group), data = thamesfort, fill = "white") +
geom_polygon(aes(x=long, y=lat, group=group), data = bightfort, fill = "white")+
geom_path(data = bightfort, aes(x = long, y = lat, group=group))+
geom_path(data = thamesfort, aes(x = long, y = lat, group=group))+
geom_path(data = channelfort, aes(x = long, y = lat, group=group))+
geom_path(data = Beezfort, aes(x = long, y = lat, group=group))+
geom_path(data = Feezfort, aes(x = long, y = lat, group=group))+
geom_path(data = Eeezfort, aes(x = long, y = lat, group=group))+
geom_path(data = Deezfort, aes(x = long, y = lat, group=group))+
theme_bw() +
theme(panel.background = element_rect(fill = "cornsilk1"),
panel.grid.major = element_line(linetype ="blank"),
panel.grid.minor = element_line(linetype = "blank"),
axis.title = element_blank(),
axis.text = element_text(size = 8))
}
plot_map()

#### 1.4 Add metadata of cruise to the map ####
# Start with the sampling stations
#First the metadata file containing all information gathered on board of the RV Simon Stevin. From this file the station name and coordinates can be retrieved

meta<-read.csv("Cruise_data_2017.csv", header=T, sep=",")
stat<-select(meta, c("StationCode","Latitude", "Longitude"))

#Now the stations need to be added to the map with a label

plot_map() + 
geom_point(data=stat, aes(x=Longitude, y=Latitude),size=2, colour="plum2")+geom_text(data=stat, aes(Longitude+0.1, Latitude, label=StationCode), hjust=0, size=2)

#Other abiotic and biotic data can be found in the meta file
#Data is retrieved from the
#UW: Underway data can be found on MIDAS
#CTD: Conductivity, Temperature and Depth sensor
#Nutrients: Analysed by NIOZ, stored on MIDAS
#Pigments: Analysed by UGent-PAE, stored on MIDAS

meta<-read.csv("Cruise_data_2017.csv", header=T, sep=",")
str(meta)

#A first exploration with the str() function already indicates that StartDate and EndDate are not labelled as date-time object.

meta$StartDate<- parse_date_time(meta$StartDate , orders = "d/m/y H:M")
meta$EndDate<- parse_date_time(meta$EndDate , orders = "d/m/y H:M")

#### 1.5 add bathymetry to map ####
#Bathymetry data can be retrieved from [Emodnet](http://portal.emodnet-bathymetry.eu/)

library(raster)
r <- raster("emodnet_mean.asc")
bathy <- data.frame(coordinates(r))
bathy$z <- as.vector(r)
bathy$x <- round(bathy$x, digits = 2)
bathy$y <- round(bathy$y, digits = 2)
require(dplyr)
bathy <- summarise(group_by(bathy, x, y),
z = mean(z))

#Data contains also land data; outliers; missing data. Needs to be filtered out
bathy$z<-ifelse(bathy$z >60,"NA",bathy$z)
bathy$z<-ifelse(bathy$z <0, "NA", bathy$z)
bathy$z<-as.numeric(bathy$z)
bathy<-na.omit(bathy)
#write.table(bathy, "bathy.txt")

#The bathymetry can also be selected for the specific station points
stat$x <- round(stat$Longitude, digits = 2)
stat$y <- round(stat$Latitude, digits = 2)
stat <- left_join(stat, bathy)
stat <- select(stat, -x, -y)

#rename z to depth
colnames(stat)[4] <- "Depth"
meta<-left_join(meta, stat)


#Now the bathymetry can be plotted onto our map
plot_map() +geom_tile(aes(x=x, y=y, fill = -z), data = bathy)+
scale_fill_gradient(limits = c(0,-55))+
geom_point(data = stat, aes(x=Longitude,y=Latitude), size = 1.9, colour = "plum2")+
geom_text(data=stat, aes(Longitude +0.1, Latitude, label = StationCode), hjust=0, size=2)


