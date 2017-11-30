---
#title: "Active reciever network"
#author: "VLIZ"
#date: "13-11-2017"
#output: html_document
---
#In this topic we will plot the sample region of the Lifewatch - Belgian receiver network.
#Besides the generic R functions, there are many specialized [packages](https://cran.r-project.org/web/views/Spatial.html) to visualize and analyze spatial data.

#Goal of this document is to help you visualize the receiver network.
  
##### 1. Making a map of the Southern North Sea, English Channel and the Thames estuary ####
  
#Combining **ggplot** with the R packages that facilitate the handling of geospatial data, as [maptools](https://cran.r-project.org/web/packages/maptools/maptools.pdf), [rgdal](https://cran.r-project.org/web/packages/rgdal/rgdal.pdf), [rgeos](https://cran.r-project.org/web/packages/rgeos/rgeos.pdf) and [mapproj](https://cran.r-project.org/web/packages/mapproj/mapproj.pdf), enables the construction of simple and complex maps.


library(maptools)
library(ggmap)
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
rivers <-readOGR("Mapping/Rivers", layer = "BNLF_Water_2004")
buurlanden <-readOGR("Mapping/Buurlanden", layer = "B_Buurlanden_2008")
netherlands_coast <- readOGR("Mapping/netherlands_coast", layer = "world_countries_coasts")
boundary <-readOGR(dsn=path.expand("Mapping/Belgium-boundary-land"), layer = "Belgium_land")


#Looking at the class, structure and plot of the shapefile allows to better understand the data type. Output is not given in this document due its large size

class(bight)
class(channel)
str(bight)
str(channel)
plot(bight)
plot(channel)
plot(rivers)
plot(buurlanden)
plot(netherlands_coast)

#### 1.2 Plot spatial data in ggplot ####

#The **fortify()** function allows to easily convert an object of a spatial class to a regular dataframe. Though this transformation is not necessary for making a map in R, it is vital to map data in **ggplot**.

bightfort <- fortify(bight)
str(bightfort)
riversfort <- fortify(rivers)
channelfort <- fortify(channel)
buurlandenfort <- fortify(buurlanden)
netherlands_coastfort <- fortify(netherlands_coast)
boundaryfort <- fortify(boundary)

#### 1.3 Add layers to the plot ####

#Now, we can add more information on this plot, for example the Economic Exclusive zones for UK, The Netherlands, France and Belgium.

# EEZ 
Beez <- readOGR("Mapping/Belgian eez/eez.shp")
Feez <- readOGR("Mapping/French eez/eez.shp")
Eeez <- readOGR("Mapping/United Kingdom eez/eez.shp")
Deez <- readOGR("Mapping/Dutch eez/eez.shp")
Beezfort <- fortify(Beez)
Feezfort <- fortify(Feez)
Eeezfort <- fortify(Eeez)
Deezfort <- fortify(Deez)


# Sandbanks
sandbanks <- readOGR("Mapping/Sandbanks/Southernbight_banks.shp")
sandfort <- fortify(sandbanks)

# Belgian border

#### 1.4 Make a funtion of your map
# If you have your basic map. Make a fucntion of it, to easily add/update this map.

# option 1 --> use Google maps
BG_map <- ggmap(get_map(location = c(lon = 3, lat = 51),
                        maptype = "satellite", zoom = 8))

plot_BG_map <- function() {
BG_map+geom_path(data = Beezfort, aes(x = long, y = lat, group=group), col = "gray18")
}  

plot_BG_map()

# option 2 --> use shapefiles
plot_map <- function() {
ggplot() + 
  theme_bw() +
  theme(panel.background = element_rect(fill = "gray87"),
        panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"),
        axis.title = element_blank(),
        axis.text = element_text(size = 16)) +
  coord_map(xlim = c(2,6), ylim = c(50.5,52)) +
  geom_polygon(aes(x=long, y=lat, group=group), data = bightfort, fill = "white") +
  geom_polygon(aes(x=long, y=lat, group=group), data = netherlands_coastfort, fill = "gray87")+
  geom_path(data = riversfort, aes(x = long, y = lat, group=group), col = "gray98")+
  geom_path(data = Beezfort, aes(x = long, y = lat, group=group), col = "gray18")+
  geom_path(data = sandfort, aes (x= long, y= lat, group=group), col = "gray87")
}  

plot_map()

#### 1.4 Add deployment information ####
# open deployments
setwd ("/data/home/janr/Short Note Belgian Receiver Network/Mapping")
stations<-read.csv("ETN_stations-open-deployments_BRN.csv", header=T, sep=",")
head (stations)
class (stations$collectioncode)

deployments <- read.csv("deployments_detections_counts.csv", header =T, sep=",")
head (deployments)

species <- read.csv("deployments-species-individuals-count.csv", header =T, sep=",")
head (species)

#Now the stations need to be added to the map 
#In the future They will be downloaded from the ETn database through the datacall

plot_map() + 
geom_point(data=stations, aes(x=stn_long, y=stn_lat, colour="station_name"),size=2)
ggsave("map_active.png")

#### 1.5 add statistics to each station on the map ####

## Total # of detections
# First Summarize deployments per station
station_count <- deployments %>%  
  group_by(station_name) %>% 
  mutate(sum_deploy = sum(detection_count))%>%
  distinct(station_name,.keep_all=TRUE)

head(station_count)

# Thereafter plot the detections
plot_map() + geom_point(data = station_count, aes(deploy_long,deploy_lat, size = sum_deploy), colour = "red")

## Overlay Pie-chart
# first select your data
species_station <- species %>%  
  group_by(station_name, scientific_name) %>% 
  mutate(sum_ind = sum(individual_count))%>%
  distinct(station_name,.keep_all=TRUE)

head (species_all)

# then link data of station_count to species_station
species_all <- left_join(species_station, station_count)


# Next step: define your pie-charts
test <- filter(species_all,grepl("bpns|BPNS",station_name))

# Pie charts in ggplot maps per receiver
## 
# https://cran.r-project.org/web/packages/scatterpie/vignettes/scatterpie.html

library(scatterpie)
library (reshape)


# Cast the data to have a column per species
species_cast <- cast(species_all, deploy_lat + deploy_long +station_name~scientific_name, value='sum_ind', drop= FALSE)
colnames(species_cast)

# create radius
n <- nrow(species_cast)
species_cast$radius <- 6 * abs(rnorm(n))


## try to make a pie chart scatter plot... Does not work yet
plot <- plotmap() +  geom_scatterpie(aes(x=deploy_lat, y=deploy_long, group=station_name), data=species_cast, 
                  cols=c('Alosa fallax','Anguilla anguilla','Coregonus lavaretus','Cyprinus carpio', 
                         'Gadus morhua', 'Lampetra fluviatilis', 'Leuciscus idus', 'Petromyzon marinus', 
                         'Pleuronectes platessa', 'Rutilus rutilus', 'Salmo salar', 'Silurus glanis', 
                         'Squalius cephalus'),color=NA, alpha=.8) +
  coord_equal()

plot + geom_scatterpie_legend(species_cast$radius, x=-160, y=-55)



pie <- ggplot(test, aes(x="", y=sum_ind, fill=scientific_name))+
  ggtitle("Individuals per species per station")+
  geom_bar(width = 1, stat = "identity")+
  facet_wrap(~station_name)+
  coord_polar("y", start=0)

pie
#### 1.6 add bathymetry to map ####
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


