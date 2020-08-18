library(tidyverse)
library(lubridate)
library(sf)
library(leaflet)
library(shiny)

# Visualize the elements to identify the road IDs
city <- "bandung"
fl <- list.files(paste0("output/", city), full.names = T)
fl
bandung <- read_rds(fl[[1]])

bdg_map <- leaflet(bandung) %>%
  addTiles() %>%
  addPolylines(highlightOptions = highlightOptions(color = "black"),
               popup = ~htmltools::htmlEscape(paste0(DE, " ", PC)))
bdg_map

### Roads from OSM to be spatial-joined
road_osm <- st_read("D:/Spatial/OpenStreetMap/roads/bandung.geojson")
st_write(bandung, "D:/Spatial/OpenStreetMap/roads/bandung_here.geojson")


