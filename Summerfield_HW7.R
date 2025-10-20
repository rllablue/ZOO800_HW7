##Created by Samantha Summerfield and Rebekkah Leigh Lablue

##for mapping several lake locations for my research
##10/16/25

#load the libraries, these may all not be needed 
#I just had them for various trials of making the map
library(sp)
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(ggplot2)
library(sf)
library(raster)
library(terra)
library(ggmap)
library(maps)
library(rnaturalearthdata)
library(rnaturalearth)
library(rnaturalearthhires)
library(RColorBrewer)

#bring in the wisconsin state outline
usa = ne_states(country = "united states of america", returnclass = "sf")
wisconsin = usa %>% filter(name == "Wisconsin")


#read the file into a variable
sites = read.csv("sample_sites_ss.csv")

#turn original data into the geometry of each dot, better for plotting later
sites_sf <- st_as_sf(sites, coords = c("Longitude", "Latitude"), crs = 4326)

##check to make sure they are the same file and same class (should be sf and dataframe)
st_crs(wisconsin)
st_crs(sites_sf)
class(wisconsin)
class(sites_sf)

#check that the data is within the boundary of wi
sf::st_within(sites_sf, wisconsin, sparse = FALSE)


#plot wi with data points, they are smaller dots all over wi but that is ok
#add the x and y labels to make it publish ready
#using colorblind friendly scheme
 ggplot() +
   geom_sf(data = wisconsin, fill = "lightgrey", color = "black") +  # base map
   geom_sf(data = sites_sf, aes(color = Sample), size = 1) +  # points by sample
   labs(
     title = NULL,
     x = "Longitude",
     y = "Latitude",
     color = "Sample Site"
   ) +
   scale_color_brewer(palette = "Paired") +
   theme_minimal()
 

