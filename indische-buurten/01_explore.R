library(tidyverse)
library(ggsflabel)
library(hrbrthemes)
library(extrafont)
library(sf)
library(RColorBrewer)

import_roboto_condensed()

rm(list = ls())
getwd()

## 01. Re-organize data

# Geonames
# geonamen <- geojsonsf::geojson_sf("data/geonamen.geojson")
# geonamen_pt <- read_rds("data/raw/geonamen.rds") %>%
#   filter(st_is(geometry, "POINT")) %>%
#   select(name, geometry) %>%
#   mutate(name = gsub("Latn", "", gsub("dutGazetteer", "", .$name))) %>%
#   write_rds("data/geonamen_pt.rds")
geonamen_pt <- read_rds("data/geonamen_pt.rds")

# City boundary
# grenzen <- geojsonsf::geojson_sf("data/raw/gemeentegrenzen.geojson") %>%
#   st_set_crs(28992) %>%
#   st_transform(4326) %>%
#   write_rds("data/gemeentegrenzen.rds")
grenzen <- read_rds("data/gemeentegrenzen.rds")

# Landuse
# bodemgebruik <- geojsonsf::geojson_sf("data/raw/bodemgebruik.geojson") %>%
#   st_set_crs(28992) %>%
#   st_transform(4326) %>%
#   mutate(Hoofdgroep = recode(Hoofdgroep, 
#                              `Bedrijfsterrein` = "Bebouwd", `Recreatie` = "Bebouwd", 
#                              `Semi-bebouwd` = "Bebouwd", `Vliegveld` = "Bebouwd",
#                              `Bos` = "Onbebouwd", `Droog natuurlijk terrein` = "Onbebouwd", `Glastuinbouw` = "Onbebouwd",
#                              `Landbouw` = "Onbebouwd", `Nat natuurlijk terrein` = "Onbebouwd",
#                              `Spoorweg` = "Bebouwd", `Hoofdweg` = "Bebouwd"
#                              )) %>%
#   st_make_valid() %>%
#   write_rds("data/bodemgebruik.rds")
bodemgebruik <- read_rds("data/bodemgebruik.rds")

# Cities
# steden <- c("Amsterdam", "Rotterdam", "'s-Gravenhage", "Utrecht", "Eindhoven", "Groningen", "Tilburg", "Almere", "Breda", "Nijmegen")

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

# Color brewer
# ggplot() +
#   geom_sf(data = adam, fill = "white") +
#   geom_sf(data = bodemgebruik_adam, aes(fill = Hoofdgroep), size = 0.2) +
#   geom_sf(data = geonamen_adam, alpha = 0.25) +
#   scale_fill_manual(values=c("#fdcdac", "#b3e2cd", "#cbd5e8")) +
#   theme_ipsum_tw() +
#   labs(title = "Amsterdam") +
#   theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
#         axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
#         panel.grid.major = element_blank(), legend.position = "none",
#         plot.margin = grid::unit(c(0,0,0,0), "mm"))
# 
# ggsave("adam.png", dpi = 500, width = 20, height = 15, units = "cm")

# Monochrome
ggplot() +
  geom_sf(data = adam, fill = "white") +
  geom_sf(data = bodemgebruik_adam, aes(fill = Hoofdgroep), 
          size = 0.2, color = "#b2b2b277") +
  geom_sf(data = geonamen_adam, color = "gold2", alpha = 0.25) +
  scale_fill_manual(values=c("#3B454A", "#3B454A", "#555f66")) +
  theme_ipsum_tw() +
  # theme_ft_rc(grid="", strip_text_face = "bold") +
  labs(title = "The Indonesian Neighborhood of Amsterdam",
       subtitle = "Places with Indonesian elements in its name",
       caption = "@tom5ive\n#30DayMapChallenge") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.title = element_text(color = "white"),
        plot.subtitle = element_text(color = "#74838c"),
        plot.caption = element_text(color = "white"),
        panel.grid.major = element_blank(), legend.position = "none",
        plot.margin = grid::unit(c(3,3,3,3), "mm"),
        plot.background = element_rect(fill = "#252a32"))
ggsave("adam.png", dpi = 500, width = 20, height = 15, units = "cm")
##

adam$geometry <- adam[[4]][[1]][[1]][[1]]
