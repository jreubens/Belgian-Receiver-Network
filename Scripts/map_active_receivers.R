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

#clear environment
rm(list = ls())

# source functions used in this script
#setwd ("/data/home/janr/Belgian-Receiver-Network/Scripts")
#source("scale_bar_function.R")
source('./Scripts/scale_bar_function.R')

# get info on your session: R-version, packages attached
sessionInfo()

# install extra needed packages (not yet attached)
library(maptools)
library(ggmap)
library(rgdal)
library(rgeos)
library(mapproj)
library(ggplot2)
library(dplyr)
library(base)
library(lubridate)
library(ggsn)    # Package to create scale bar
library(png)     # Package to upload arrow png
library(cowplot) # Package to add the arrow png to the map
detach("dplyr")
detach("package:dplyr", unload=TRUE)

#Using these packages we will construct a simple map of the sampled region (Southern Bight, Thames estuary and Eastern English Channel).

#### 1.1 Read shapefiles ####

#A common data format for geospatial data is the shapefile. In a shapefile, geospatial data are described as points, lines and polygons and can be analysed as such in GIS software. [Marine Regions](http://www.marineregions.org/) provides access to many shapefiles through the [gazetteer](http://www.marineregions.org/gazetteer.php?p=search). The folder **shapefiles** contains some useful shapefiles.

# have a look at https://cran.r-project.org/web/packages/mregions/vignettes/mregions.html!

list.files("Shapefiles")


#The world_bay_gulf folder and file contains the shapefile of the [Southern Bight](http://www.marineregions.org/gazetteer.php?p=details&id=2399). The seavox_v16 folder and file contains the shapefile of the [Thames estuary](http://www.marineregions.org/gazetteer.php?p=details&id=24195).
#The iho folder and file contains the shapefile of the [English Channel](http://www.marineregions.org/gazetteer.php?p=details&id=2389).
#Below you can find a ways to import it into the R Environment. The recommended **readOGR()** recognizes the type of spatial data automatically.

#setwd ("/data/home/janr/Belgian-Receiver-Network/Files/Shapefiles")
bight <- readOGR("./Files/Shapefiles/world_bay_gulf", layer = "world_bay_gulf")
channel <-readOGR("./Files/Shapefiles/iho", layer = "iho")
rivers <-readOGR("./Files/Shapefiles/Rivers", layer = "BNLF_Water_2004")
buurlanden <-readOGR("./Files/Shapefiles/Buurlanden", layer = "B_Buurlanden_2008")
netherlands_coast <- readOGR("./Files/Shapefiles/netherlands_coast", layer = "world_countries_coasts")
boundary <-readOGR("./Files/Shapefiles/Belgian-border", layer = "bnful")


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
#channelfort <- fortify(channel)
#buurlandenfort <- fortify(buurlanden)
netherlands_coastfort <- fortify(netherlands_coast)
boundaryfort <- fortify(boundary)

#### 1.3 Add layers to the plot ####

#Now, we can add more information on this plot, for example the Economic Exclusive zones for UK, The Netherlands, France and Belgium.

# EEZ 
Beez <- readOGR("./Files/Shapefiles/Belgian eez/eez.shp")
#Feez <- readOGR("French eez/eez.shp")
#Eeez <- readOGR("United Kingdom eez/eez.shp")
#Deez <- readOGR("Dutch eez/eez.shp")
Beezfort <- fortify(Beez)
#Feezfort <- fortify(Feez)
#Eeezfort <- fortify(Eeez)
#Deezfort <- fortify(Deez)


# Sandbanks
#sandbanks <- readOGR("Sandbanks/Southernbight_banks.shp")
#sandfort <- fortify(sandbanks)


#### 1.4 Make a funtion of your map ####
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
  #geom_path(data = sandfort, aes (x= long, y= lat, group=group), col = "gray87")+
  geom_path(data = boundaryfort,aes (x= long, y= lat, group =group), col = "gray22")+
  scalebar(Beezfort, location = "bottomleft", dist = 50, st.size=5, height=0.05, dd2km = TRUE, model = 'WGS84')
}  

plot_map()

#setwd ("/data/home/janr/Belgian-Receiver-Network/Figures")
ggsave("./Figures/plot_map.png")

#### 1.5 Add deployment information ####
# open deployments
#setwd ("/data/home/janr/Belgian-Receiver-Network/Files/Metadata")
stations<-read.csv("./Files/Metadata/ETN_stations-open-deployments_BRN.csv", header=T, sep=",")
head (stations)
class (stations$collectioncode)
dplyr::glimpse(stations)
# Don't take Demer stations into account (project ended)
stations <- stations %>%
  filter(station_name == "albert"|
         station_name == "zeeschelde"|
         station_name == "bpns"|
         station_name == "ws1"|
         station_name == "ws2"|
         station_name == "ws3"|
         station_name == "Dijle")

deployments <- read.csv("./Files/Metadata/deployments_detections_counts.csv", header =T, sep=",")
head (deployments)

species <- read.csv("./Files/Metadata/deployments-species-individuals-count.csv", header =T, sep=",")
head (species)

#Now the stations need to be added to the map 
#In the future They will be downloaded from the ETn database through the datacall
# Add png of arrow to plot (dirty coding...)
img <-  readPNG("./Figures/arrow.png")     
ger <- grid::rasterGrob(img, interpolate=TRUE)
map <- plot_map() + 
      geom_point(data=stations, aes(x=stn_long, y=stn_lat),size=2)
h <- ggdraw(map)                                # Overlay png on plot
h + draw_grob(ger, 0.905, 0.755, 0.10, 0.15)    # Tweak the position


#ggsave("./Figures/map_active_PV.png")
# Instead of ggsave(), manually save plot (export button in plot window) as svg file and move scale bar with inkscape to wanted position


plot_map() + 
  geom_point(data=stations, aes(x=stn_long, y=stn_lat, colour=station_name),size=2)
ggsave("./Figures/map_active_050218_colouredprojects_PV.png")

#Demer coordinaten verwijderen  --> zie lijn 151

# Add North arrow and scalebar to map
#plot_map() + geom_point(data=stations, aes(x=stn_long, y=stn_lat, colour=station_name),size=2) +
#             scale_bar(lon = 5.2, lat = 50.1, distance_lon = 20, distance_lat = 5, distance_legend = 10, dist_unit = "km")

    # if you don't want north arrow: add 'orientation=FALSE'
## Remark: komt er momenteel niet op, doordat je met verschillende lagen werkt... --> hoe oplossen?

#### 1.5 add statistics to each station on the map ####

## Total # of detections
# First Summarize deployments per station
station_count <- deployments %>%  
  group_by(station_name) %>% 
  mutate(sum_deploy = sum(detection_count))%>%
  distinct(station_name,.keep_all=TRUE)

head(station_count)

# Save and filter what is needed for BRN
#setwd ("/data/home/janr/Belgian-Receiver-Network/Files/Output")
write.csv(station_count, file = "./Files/Output/station_count.csv")

#setwd ("/data/home/janr/Belgian-Receiver-Network/Files/Metadata")
station_count_BRN <- read.csv("./Files/Metadata/station_count_BRN.csv", header =T, sep=",")


# Thereafter plot the detections
# Add png of arrow to plot (dirty coding...)
img <-  readPNG("./Figures/arrow.png")     
ger <- grid::rasterGrob(img, interpolate=TRUE)
map <- plot_map() + 
  geom_point(data = station_count_BRN, aes(deploy_long,deploy_lat, size = det_count), colour = "red")
h <- ggdraw(map)                                # Overlay png on plot
h + draw_grob(ger, 0.905, 0.755, 0.10, 0.15)    # Tweak the position

#ggsave("map_detectionsperstation_041217.png")
# Instead of ggsave(), manually save plot (export button in plot window) as svg file and move scale bar with inkscape to wanted position


## Overlay Pie-chart
# first select your data
species_station <- species %>%  
  group_by(station_name, scientific_name) %>% 
  mutate(sum_ind = sum(individual_count))%>%
  distinct(station_name,.keep_all=TRUE)


# then link data of station_count to species_station
species_all <- left_join(species_station, station_count)
head (species_all)

# Next step: define your pie-charts
# test <- filter(species_all,grepl("bpns|BPNS",station_name))

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

cols2use <- c('Alosa fallax', 'Anguilla anguilla', 'Coregonus lavaretus', 
              'Cyprinus carpio', 'Gadus morhua', 'Lampetra fluviatilis', 
              'Leuciscus idus', 'Petromyzon marinus', 'Pleuronectes platessa', 
              'Rutilus rutilus', 'Salmo salar', 'Silurus glanis', 
              'Squalius cephalus')

## try to make a pie chart scatter plot... Does not work yet
plot <- plotmap() +  geom_scatterpie(aes(x=deploy_lat, y=deploy_long, group=station_name), data=species_cast, 
                  cols=cols2use,color=NA, alpha=.8) +
  coord_equal()

plot + geom_scatterpie_legend(species_cast$radius, x=-160, y=-55) # geen idee wat x en y hier betekenen..

## tweede methode 
#pie <- ggplot(test, aes(x="", y=sum_ind, fill=scientific_name))+
  ggtitle("Individuals per species per station")+
  geom_bar(width = 1, stat = "identity")+
  facet_wrap(~station_name)+
  coord_polar("y", start=0)

#pie



## occurence facet
# We want to make an tabe with both info on tagged species and info on detected species.
# The tagged species will be the values in the table, the detected species will be shown by a color palet (the darker, the more detected...)
# We will use a type of heatmap to do so...
# See https://learnr.wordpress.com/2010/01/26/ggplot2-quick-heatmap-plotting/ and https://stackoverflow.com/questions/14290364/heatmap-with-values-ggplot2 for info

  
library(tidyverse)    # tidyverse is een pakket van packages waarin o.a. tibble en dplyr zitten, dus niet nodig om die nog eens apart in te laden
#library (tibble)
#library(dplyr)
  
# Example data of nba players 
nba <- read.csv("http://datasets.flowingdata.com/ppg2008.csv")
nba.m <- melt(nba)


# our table
# First prepare datasets to correct format
#setwd ("/data/home/janr/Belgian-Receiver-Network/Files/Metadata")
species_det <- read.csv("./Files/Metadata/speciesperyear_count_update.csv", header =T, sep=";")
species_tag <- read.csv("./Files/Metadata/speciesperyear_tagged.csv", header = T, sep=",")

# Change "salmo salar" into "Salmo salar"
table(species_det$scientific_name)
species_det$scientific_name[species_det$scientific_name == "salmo salar"] <- "Salmo salar"

table(species_tag$scientific_name)
species_tag$scientific_name[species_tag$scientific_name == "salmo salar"] <- "Salmo salar"



species_tag2 <- species_tag %>%
  filter(project != "Amsterdam")%>%
  group_by(year,scientific_name)%>%
  summarise(Tagged = sum(count))

species_det2 <- species_det %>%
  group_by(year, scientific_name)%>%
  summarise(Observed = sum(individual_count))
    
table <- full_join(species_det2, species_tag2)
table <- table %>%
  filter( scientific_name != "Sentinel")%>%
  filter(scientific_name != "Built-in")%>%
  filter(scientific_name != "Sync tag")

table[is.na(table)] <- 0

# Change column name
colnames(table) [1] <- "Year"      # Capital 'Y'
colnames(table) [2] <- "Species"   # Change to "Species"

heatmap <- ggplot(table, aes(Year, Species)) + 
      geom_tile(aes(fill = Observed), colour = "white") + 
      scale_fill_gradient2(low = "white",high = "steel blue")+
      geom_text(aes(label = Tagged)) +
      theme_grey(base_size = 11)  +
      theme(axis.title=element_text(size=14)) +
      theme(axis.text=element_text(size=12)) +
      theme(axis.text.y=element_text(face="italic"))       # Set species names in italic
heatmap

#setwd ("/data/home/janr/Belgian-Receiver-Network/Figures")
ggsave("./Figures/occurence.facet050218_PV.png")




#improve layout

heatmap + theme_grey(base_size = 11)  +
    labs(x = "", y = "") +
    theme(axis.text. = element_text(size = 11, angle = 330, colour = "grey50"))

