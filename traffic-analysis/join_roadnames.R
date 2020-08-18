library(tidyverse)
library(lubridate)
library(sf)
library(leaflet)
library(shiny)

rm(list = ls())

# Fix the road names by using OSM as reference
city <- "bandung"
fl <- list.files(paste0("output/", city), full.names = T)
bandung <- read_rds(fl[[1]])

### Roads from OSM to be spatial-joined
road_osm <- st_read("D:/Spatial/OpenStreetMap/roads/bandung.geojson")

unique(road_osm$fclass)
imp_road_buffer <- road_osm %>%
  filter(fclass %in% c("primary", "secondary", "tertiary", "motorway", "trunk")) %>%
  st_buffer(0.0001)

joined <- st_join(bandung, imp_road_buffer, left = T, largest = T) %>%
  select(PC, DE, name)

bdg_map <- leaflet(joined) %>%
  addTiles() %>%
  addPolylines(highlightOptions = highlightOptions(color = "black"),
               popup = ~htmltools::htmlEscape(paste0(DE, " ", PC, "\n", name)))

bdg_map

write_rds(joined %>% st_set_geometry(NULL), "data/bandung_ref.rds")
