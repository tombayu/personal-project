library(tidyverse)
library(edgebundle)
library(igraph)
library(sf)
library(igraph)
library(hrbrthemes)

rm(list = ls())

tj <- read_rds("data/tj/TJ.rds")
tj_rute <- read_rds("data/tj/sp_tj_rute.rds") %>%
  filter(isHidden == F)
# tj_halte <- read_rds("data/tj/sp_tj_halte.rds")
tj_halte <- read_rds("data/tj/tb_tj_halte.rds")

dki <- st_read("data/rbi/KotaJadetabek.geojson") %>%
  filter(str_detect(NAME_1, "Jakarta"))

ggplot() +
  geom_sf(data = dki, fill = NA, col = "grey", size = 0.1, linetype = "dashed") +
  # geom_sf(data = tj_rute) +
  geom_sf(data = tj_rute, col = "#9d0191", size = 0.1, show.legend = FALSE) +
  geom_sf(data = tj_rute, col = "white", size = 0.05, alpha = 0.5, show.legend = FALSE) +
  # geom_point(data = important_stops, aes(lon * 100, lat * 100, size = n_koridor), col = "steelblue2", alpha = 0.2, shape = 19) +
  # geom_point(data = important_stops, aes(lon * 100, lat * 100), col = "steelblue2", size = 0.25) +
  # geom_text(data = important_stops, aes(lon * 100, lat * 100, label = nama_halte), size = 1, color = "steelblue2") +
  labs(caption = c("\n@tom5ive", "Jaringan layanan TransJakarta\nData: Trafi")) +
  theme_ipsum_tw() +
  theme(plot.title = element_text(color = "white"),
        plot.caption = element_text(hjust = c(1, 0), color = "white", size = 8),
        plot.background = element_rect(fill = "black"),
        panel.grid.major = element_blank(),
        plot.margin = grid::unit(c(3, 3, 3, 3), "mm"),
        axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = "none")

ggsave("output/tj_routes.jpg", dpi = 500, width = 10, height = 10, units = "cm")

