#### 0. Mapping in R ####
# see: https://cran.r-project.org/web/views/Spatial.html
# for all options related to mapping, we will used ggplot

#### 1. Making a map of the BPNS ####
# To start, we will construct a basic map of the BPNS, which we can later use to plot our own data on top of it. 

#### 1.1 Read shapefiles ####
# The folder Mapping contains shapefiles, downloaded from Marine Regions.
list.files("Data day 1/Mapping")

#Reading shape files in R is possible in different ways, for example with the packages maptools or rgdal.
library(rgdal)
install.packages("maptools")
library(maptools)
library(mapproj)

# The world_bay_gulf folder and file contains the shapefile of the Southern Bight. Below you can find three ways to import it into the R Environment.
#bight <- readShapePoly("Data day 1/Mapping/world_bay_gulf/world_bay_gulf.shp")
bight <- readOGR("Data day 1/Mapping/world_bay_gulf/world_bay_gulf.shp")
bight <- readOGR("Data day 1/Mapping/world_bay_gulf", layer = "world_bay_gulf")

# Looking at the class, structure and plot of the shapefile allows to better understand the data type.
class(bight)
str(bight)
plot(bight)

#### 1.2 Plot spatial data in ggplot ####
# In order to use this data in ggplot, we will transform the shapefile into a data frame.
bightfort <- fortify(bight)
str(bightfort)

# Plot a basic map of the Southern Bight. Take care to 
ggplot() + geom_path(data = bightfort, aes(x = long, y = lat))
ggplot() + geom_path(data = bightfort, aes(x = long, y = lat, group = group))

#### 1.3 Add layers to the plot ####
# Now, we can add more information on this plot.The Belgian EEZ for example.
eez <- readOGR("Data day 1/Mapping/eez/eez.shp")
eezfort <- fortify(eez)

ggplot() + 
  geom_path(data = bightfort, aes(x = long, y = lat, group = group)) +
  geom_path(data = eezfort, aes(x = long, y = lat, group = group))

# With coord_map, ggplot recognizes the plot is a map and will define the length of the axes accordingly. With coord_map, you can also define the limits of the axes.
ggplot() + 
  coord_map() +
  geom_path(data = bightfort, aes(x = long, y = lat, group = group)) +
  geom_path(data = eezfort, aes(x = long, y = lat, group = group))

ggplot() + 
  coord_map(xlim = c(2,3.5), ylim = c(51,52)) +
  geom_path(data = bightfort, aes(x = long, y = lat, group = group)) +
  geom_path(data = eezfort, aes(x = long, y = lat, group = group))

# Themes in R allow for prettier maps!
ggplot() + 
  theme_bw() +
  theme(panel.background = element_rect(fill = "#0093b4"),
        panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"),
        axis.title = element_blank(),
        axis.text = element_text(size = 16)) +
  coord_map(xlim = c(2,3.5), ylim = c(51,52)) +
  geom_path(data = bightfort, aes(x = long, y = lat, group = group)) +
  geom_path(data = eezfort, aes(x = long, y = lat, group = group))

ggplot() + 
  theme_bw() +
  theme(panel.background = element_rect(fill = "#0093b4"),
        panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"),
        axis.title = element_blank(),
        axis.text = element_text(size = 16)) +
  coord_map(xlim = c(2,3.5), ylim = c(51,52)) +
  geom_polygon(aes(x=long, y=lat, group=group), data = bightfort, fill = "white") +
  geom_path(data = bightfort, aes(x = long, y = lat, group = group)) +
  geom_path(data = eezfort, aes(x = long, y = lat, group = group))

# Apparently, there is an issue with the Dutch peninsula on the map.
netherlands_coast <- readOGR("Data day 1/Mapping/netherlands_coast/world_countries_coasts.shp")
netherlands_coastfort <- fortify(netherlands_coast)
ggplot() + 
  theme_bw() +
  theme(panel.background = element_rect(fill = "#0093b4"),
        panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"),
        axis.title = element_blank(),
        axis.text = element_text(size = 16)) +
  coord_map(xlim = c(2,3.5), ylim = c(51,52)) +
  geom_polygon(aes(x=long, y=lat, group=group), data = bightfort, fill = "white") +
  geom_polygon(aes(x=long, y=lat, group=group), data = netherlands_coastfort, fill = "#0093b4") +
  geom_path(data = bightfort, aes(x = long, y = lat, group = group)) +
  geom_path(data = eezfort, aes(x = long, y = lat, group = group))

# Next up: add sand banks!
banks <- readOGR("Data day 1/Mapping/banks/banks.shp")
banksfort <- fortify(banks)
ggplot() + 
  theme_bw() +
  theme(panel.background = element_rect(fill = "#0093b4"),
        panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"),
        axis.title = element_blank(),
        axis.text = element_text(size = 16)) +
  coord_map(xlim = c(2,3.5), ylim = c(51,52)) +
  geom_polygon(aes(x=long, y=lat, group=group), data = bightfort, fill = "white") +
  geom_polygon(aes(x=long, y=lat, group=group), data = netherlands_coastfort, fill = "#0093b4") +
  geom_path(data = bightfort, aes(x = long, y = lat, group = group)) +
  geom_path(data = eezfort, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(x=long, y=lat, group=group), data = banksfort, fill = "#016483")

# More sand banks!
bankszeeland <- readOGR("Data day 1/Mapping/Zeeland banks/banks.shp")
bankscoastal <- readOGR("Data day 1/Mapping/Coastal banks/banks.shp")
bankshinder <- readOGR("Data day 1/Mapping/Hinder banks/banks.shp")
bankszeelandfort <- fortify(bankszeeland)
bankscoastalfort <- fortify(bankscoastal)
bankshinderfort <- fortify(bankshinder)

ggplot() + 
  theme_bw() +
  theme(panel.background = element_rect(fill = "#0093b4"),
        panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"),
        axis.title = element_blank(),
        axis.text = element_text(size = 16)) +
  coord_map(xlim = c(2,3.5), ylim = c(51,52)) +
  geom_polygon(aes(x=long, y=lat, group=group), data = bightfort, fill = "white") +
  geom_polygon(aes(x=long, y=lat, group=group), data = netherlands_coastfort, fill = "#0093b4") +
  geom_path(data = bightfort, aes(x = long, y = lat, group = group)) +
  geom_path(data = eezfort, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(x=long, y=lat, group=group), data = banksfort, fill = "#016483") +
  geom_polygon(aes(x=long, y=lat, group=group), data = bankszeelandfort, fill = "#016483") +
  geom_polygon(aes(x=long, y=lat, group=group), data = bankscoastalfort, fill = "#016483") +
  geom_polygon(aes(x=long, y=lat, group=group), data = bankshinderfort, fill = "#016483")

# We have now created a simple map of the BPNS (with some nice VLIZ colours). 

#### 1.4 Compile the code to construct the map in a function ####
# A function to load all shape files and plot the map.
load_shapes_plot_map <- function() {
  banks <- readOGR("Data day 1/Mapping/banks/banks.shp")
  eez <- readOGR("Data day 1/Mapping/eez/eez.shp")
  bight <- readOGR("Data day 1/Mapping/world_bay_gulf/world_bay_gulf.shp")
  bankszeeland <- readOGR("Data day 1/Mapping/Zeeland banks/banks.shp")
  bankscoastal <- readOGR("Data day 1/Mapping/Coastal banks/banks.shp")
  bankshinder <- readOGR("Data day 1/Mapping/Hinder banks/banks.shp")
  netherlands_coast <- readOGR("Data day 1/Mapping/netherlands_coast/world_countries_coasts.shp")
  
  # Fortify shapefiles to use them in ggplot2
  eezfort <- fortify(eez)
  banksfort <- fortify(banks)
  bightfort <- fortify(bight)
  bankszeelandfort <- fortify(bankszeeland)
  bankscoastalfort <- fortify(bankscoastal)
  bankshinderfort <- fortify(bankshinder)
  netherlands_coastfort <- fortify(netherlands_coast)
  netherlands_coastfort <- filter(netherlands_coastfort, lat >51.36)
  
  # Make base map plot with receivers
  ggplot() + 
    coord_map(xlim = c(2,3.5), ylim = c(51,52)) + #important! this makes that the polygons are ok
    theme_bw() +
    theme(panel.background = element_rect(fill = "#0093b4"),
          panel.grid.major = element_line(linetype = "blank"),
          panel.grid.minor = element_line(linetype = "blank"),
          axis.title = element_blank(),
          axis.text = element_text(size = 16)) +
    geom_polygon(aes(x=long, y=lat, group=group), data = bightfort, fill = "white") +
    geom_polygon(aes(x=long, y=lat, group=group), data = netherlands_coastfort, fill = "#0093b4") +  
    geom_path(aes(x=long, y=lat, group=group), data = bightfort) +
    geom_polygon(aes(x=long, y=lat, group=group), data = banksfort, fill = "#016483") +
    geom_polygon(aes(x=long, y=lat, group=group), data = bankszeelandfort, fill = "#016483") +
    geom_polygon(aes(x=long, y=lat, group=group), data = bankscoastalfort, fill = "#016483") +
    geom_polygon(aes(x=long, y=lat, group=group), data = bankshinderfort, fill = "#016483") +
    geom_path(aes(x=long, y=lat), data = eezfort, colour = "black")
}

# Load all shapefiles in the environment and construct the map seperately. This is the favoured option when you plot several maps.
banks <- readOGR("Data day 1/Mapping/banks/banks.shp")
eez <- readOGR("Data day 1/Mapping/eez/eez.shp")
bight <- readOGR("Data day 1/Mapping/world_bay_gulf/world_bay_gulf.shp")
bankszeeland <- readOGR("Data day 1/Mapping/Zeeland banks/banks.shp")
bankscoastal <- readOGR("Data day 1/Mapping/Coastal banks/banks.shp")
bankshinder <- readOGR("Data day 1/Mapping/Hinder banks/banks.shp")
netherlands_coast <- readOGR("Data day 1/Mapping/netherlands_coast/world_countries_coasts.shp")

eezfort <- fortify(eez)
banksfort <- fortify(banks)
bightfort <- fortify(bight)
bankszeelandfort <- fortify(bankszeeland)
bankscoastalfort <- fortify(bankscoastal)
bankshinderfort <- fortify(bankshinder)
netherlands_coastfort <- fortify(netherlands_coast)
netherlands_coastfort <- filter(netherlands_coastfort, lat >51.36)

plot_map <- function() {
  ggplot() + 
    coord_map(xlim = c(2,3.5), ylim = c(51,52)) +
    theme_bw() +
    theme(panel.background = element_rect(fill = "#0093b4"),
          panel.grid.major = element_line(linetype = "blank"),
          panel.grid.minor = element_line(linetype = "blank"),
          axis.title = element_blank(),
          axis.text = element_text(size = 16)) +
    geom_polygon(aes(x=long, y=lat, group=group), data = bightfort, fill = "white") +
    geom_polygon(aes(x=long, y=lat, group=group), data = netherlands_coastfort, fill = "#0093b4") +  
    geom_path(aes(x=long, y=lat, group=group), data = bightfort) +
    geom_polygon(aes(x=long, y=lat, group=group), data = banksfort, fill = "#016483") +
    geom_polygon(aes(x=long, y=lat, group=group), data = bankszeelandfort, fill = "#016483") +
    geom_polygon(aes(x=long, y=lat, group=group), data = bankscoastalfort, fill = "#016483") +
    geom_polygon(aes(x=long, y=lat, group=group), data = bankshinderfort, fill = "#016483") +
    geom_path(aes(x=long, y=lat), data = eezfort, colour = "black")
}

plot_map()

#### 2. Plot the station locations on the map ####
# First off, group our station data in a new data frame stationdf.
# summarise is used to have lat and long only once! Otherwise it would be plotted 1000den times on your plot..
stationdf <- group_by(poddata, Station) %>% summarise(Latitude = max(Latitude), Longitude = max(Longitude))
plot_map() + geom_point(data = stationdf, aes(Longitude,Latitude), size = 4, colour = "red")

# Now, we can add the station names.
plot_map() + geom_point(data = stationdf, aes(Longitude,Latitude), size = 4, colour = "red") + 
  geom_label(data = stationdf, aes(Longitude +0.01,Latitude,label = Station), hjust = 0)

# This is very messy... We can use the package ggrepel to clean things up a bit!
install.packages("ggrepel")
library(ggrepel)
plot_map() + geom_point(data = stationdf, aes(Longitude,Latitude), size = 4, colour = "red") + 
  geom_label_repel(data = stationdf, aes(Longitude +0.01,Latitude,label = Station))

# Repel does the job, but our plot is still a bit messy. We can try to plot zones instead.
zonedf <- group_by(poddata, Zone) %>% summarise(Latitude = mean(Latitude), Longitude = mean(Longitude))
plot_map() + geom_point(data = zonedf, aes(Longitude,Latitude), size = 4, colour = "red") + 
  geom_label_repel(data = zonedf, aes(Longitude +0.01,Latitude,label = Zone))

#### 3. Spatial data exploration ####
# In order to plot detection related variables spatially, we first summarise them for each zone.
zonedf <- group_by(poddata_day, Zone) %>% 
  summarise(Latitude = mean(Latitude), 
            Longitude = mean(Longitude),
            Milliseconds = mean(Milliseconds),
            Number_clicks_filtered = mean(Number_clicks_filtered),
            Number_clicks_total = mean(Number_clicks_total),
            Lost_minutes = mean(Lost_minutes),
            Dpm = sum(Dpm),
            Dp10m = sum(Dp10m),
            Dph = sum(Dph),
            Recorded = sum(Recorded))
zonedf$Click_frequency <- zonedf$Dpm/zonedf$Recorded
zonedf$Click_intensity <- zonedf$Number_clicks_filtered/zonedf$Click_frequency

#### 3.1 Mapping detections ####
plot_map() + geom_point(data = zonedf, aes(Longitude,Latitude, size = Milliseconds), colour = "red")
plot_map() + geom_point(data = zonedf, aes(Longitude,Latitude, size = Number_clicks_filtered), colour = "red")
plot_map() + geom_point(data = zonedf, aes(Longitude,Latitude, size = Dpm), colour = "red")
plot_map() + geom_point(data = zonedf, aes(Longitude,Latitude, size = Dp10m), colour = "red")
plot_map() + geom_point(data = zonedf, aes(Longitude,Latitude, size = Dph), colour = "red")

plot_map() + geom_point(data = zonedf, aes(Longitude,Latitude, size = Click_frequency), colour = "red")
plot_map() + geom_point(data = zonedf, aes(Longitude,Latitude, size = Click_intensity), colour = "red")

# An alternative to first grouping and summarizing the data, is to use the group argument in ggplot.
plot_map() + geom_point(data = poddata_day, aes(Longitude,Latitude, size = Click_frequency, group=Zone), colour = "red")

#### 3.2 Visualize spatiotemporal patterns ####
# In order to get an idea of the porpoise distribution each month, we first summarize the data per zone and month.
zonedf <- group_by(poddata_day, Zone, month(Time)) %>% 
  summarise(Latitude = mean(Latitude), 
            Longitude = mean(Longitude),
            Milliseconds = mean(Milliseconds),
            Number_clicks_filtered = mean(Number_clicks_filtered),
            Number_clicks_total = mean(Number_clicks_total),
            Lost_minutes = mean(Lost_minutes),
            Dpm = sum(Dpm),
            Dp10m = sum(Dp10m),
            Dph = sum(Dph),
            Recorded = sum(Recorded))
zonedf$Click_frequency <- zonedf$Dpm/zonedf$Recorded
zonedf$Click_intensity <- zonedf$Number_clicks_filtered/zonedf$Click_frequency
colnames(zonedf)[2] <- "Month"


# We can now visualize the distribution pattern per month.
plot_map() + geom_point(data = zonedf[zonedf$Month == 1,], aes(Longitude,Latitude, size = Click_frequency), colour = "red") + ggtitle(1)

# Instead of making 12 plots in 12 lines of code, we can make a list of plots.
#lapply = maak een lijst voor iedere unieke maand
lapply(unique(zonedf$Month), function(x){
  plot_map() + geom_point(data = zonedf[zonedf$Month == x,], aes(Longitude,Latitude, size = Click_frequency), colour = "red") + ggtitle(x)
})

# Alternative to plot in chronological order:
lapply(1:12, function(x){
  plot_map() + geom_point(data = zonedf[zonedf$Month == x,], aes(Longitude,Latitude, size = Click_frequency), colour = "red") + ggtitle(x)
})

# or use facet-wrap
plot_map()+ geom_point(data = zonedf, aes(Longitude,Latitude, size = Click_frequency), colour = "red") + facet_wrap(~Month)

#### 3.3 Extra: heat maps ####

#### 4. Extra: Spatial autocorrelation ####
# Or rather: an attempt to investigate spatial autocorrelation
library(sp)
pod_sp <- zonedf
pod_sp <- poddata_day
coordinates(pod_sp) <- ~Longitude+Latitude
proj4string(pod_sp) <- "+proj=longlat"

library(gstat)
?variogram
v <- variogram(Dpm~1, data=pod_sp)
plot(v)
plot(variogram(Dpm~1, data=pod_sp, cloud=TRUE))
plot(variogram(Dpm~1, data=pod_sp, alpha = c(0,90)))
plot(variogram(Dpm~1, data=pod_sp, alpha = c(0,90), cloud=T))

# Next step: do it for months seperately
