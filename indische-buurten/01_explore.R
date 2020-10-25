library(tidyverse)
library(ggsflabel)
library(hrbrthemes)
library(extrafont)
library(sf)
library(RColorBrewer)

rm(list = ls())
getwd()

## 01. Re-organize data

# Geonames
# geonamen <- geojsonsf::geojson_sf("data/geonamen.geojson")
geonamen_pt <- read_rds("data/raw/geonamen.rds") %>%
  filter(st_is(geometry, "POINT")) %>%
  select(name, geometry) %>%
  mutate(name = gsub("Latn", "", gsub("dutGazetteer", "", .$name))) %>%
  write_rds("data/geonamen_pt.rds")
geonamen_pt <- read_rds("data/geonamen_pt.rds")

# City boundary
grenzen <- geojsonsf::geojson_sf("data/raw/gemeentegrenzen.geojson") %>%
  st_set_crs(28992) %>%
  st_transform(4326) %>%
  write_rds("data/gemeentegrenzen.rds")
grenzen <- read_rds("data/gemeentegrenzen.rds")

# Landuse
bodemgebruik <- geojsonsf::geojson_sf("data/raw/bodemgebruik.geojson") %>%
  st_set_crs(28992) %>%
  st_transform(4326) %>%
  mutate(Hoofdgroep = recode(Hoofdgroep, 
                             `Bedrijfsterrein` = "Bebouwd", `Recreatie` = "Bebouwd", 
                             `Semi-bebouwd` = "Bebouwd", `Vliegveld` = "Bebouwd",
                             `Bos` = "Onbebouwd", `Droog natuurlijk terrein` = "Onbebouwd", `Glastuinbouw` = "Onbebouwd",
                             `Landbouw` = "Onbebouwd", `Nat natuurlijk terrein` = "Onbebouwd",
                             `Spoorweg` = "Bebouwd", `Hoofdweg` = "Bebouwd"
                             )) %>%
  st_make_valid() %>%
  write_rds("data/bodemgebruik.rds")
bodemgebruik <- read_rds("data/bodemgebruik.rds")

# Cities
steden <- c("Amsterdam", "Rotterdam", "'s-Gravenhage", "Utrecht", "Eindhoven", "Groningen", "Tilburg", "Almere", "Breda", "Nijmegen")

# Amsterdam
adam <- grenzen %>%
  filter(Gemeentenaam == "Amsterdam")

geonamen_adam <- geonamen_pt %>%
  filter(st_contains(adam, ., sparse = F)) %>%
  filter(str_detect(name, regex("java|sumatra|borneo|celebes|bali|banka|madura|deli
                                |lombok|padang|djambi|soenda|lampong|makassar|boeroe
                                |gorontalo|ternate|nias|kramat|ceram|karimata|boeton
                                |kramat|molukken|bali|atjeh|riouw|minahassa|ambon
                                |halmaheira|tidore|solo|batavia|preanger|semarang
                                |mataram|palembang|batjan|soerabaja|djakarta", ignore_case = T)))

bodemgebruik_adam <- st_intersection(bodemgebruik, adam)

unique(bodemgebruik_adam$Hoofdgroep)

ggplot() +
  geom_sf(data = adam, fill = "white") +
  geom_sf(data = bodemgebruik_adam, aes(fill = Hoofdgroep), size = 0.2) +
  geom_sf(data = geonamen_adam, alpha = 0.25) +
  scale_fill_manual(values=c("#fdcdac", "#b3e2cd", "#cbd5e8")) +
  theme_ipsum_tw() +
  labs(title = "Amsterdam") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(), legend.position = "none",
        plot.margin = grid::unit(c(0,0,0,0), "mm"))

ggsave("adam.png", dpi = 500, width = 20, height = 15, units = "cm")

unique(bodemgebruik_adam$Hoofdgroep)
geonamen_adam
