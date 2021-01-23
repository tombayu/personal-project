library(tidyverse)
library(edgebundle)
library(igraph)
library(sf)
library(igraph)
library(hrbrthemes)

airports <- read_csv("data/flights/airports.dat.txt", col_names = c("airport_id", "name", "city", "country", "IATA", "ICAO",
                                                                    "lat", "lon", "alt", "tz", "dst", "tz_db", "type", "source"),
                    col_types = cols(airport_id = col_double()))
routes <- read_csv("data/flights/routes.dat.txt", col_names = c("airlines", "airlines_id", 
                                                                "from_airport", "from_airport_id", "to_airport", "to_airport_id",
                                                                "codeshare", "stops", "equipment"),
                   col_types = cols(from_airport_id = col_double(), to_airport_id = col_double()))

airports_id <- airports %>%
  filter(country == "Indonesia") %>%
  select(IATA, airport_id, name, city, lon, lat, alt)

routes_id <- routes %>%
  filter(from_airport_id %in% airports_id$airport_id) %>%
  filter(to_airport_id %in% airports_id$airport_id) %>%
  select(from_airport, to_airport, from_airport_id, to_airport_id, airlines)

airports_id <- airports_id %>%
  filter(IATA %in% unique(c(routes_id$from_airport, routes_id$to_airport)))

flights_id <- graph_from_data_frame(d = routes_id, vertices = airports_id, directed = F)
flights_id <- simplify(flights_id)
# Edge bundling

flights_id_se <- edge_bundle_stub(flights_id, as.matrix(airports_id[,5:6]))

# Edge bundling and visualization
id_prov <- st_read("data/rbi/Provinsi.geojson") %>%
  mutate(geometry = geometry)

# Force directed
flights_id_fd <- edge_bundle_force(flights_id, as.matrix(airports_id[,5:6]), compatibility_threshold = 0.9)
ggplot(flights_id_fd) +
  geom_sf(data = id_prov, fill = NA, col = "grey", size = 0.05) +
  geom_path(aes(x, y, group=group), col = "#9d0191", size = 0.05, show.legend = FALSE) +
  geom_path(aes(x, y, group=group), col = "white", size = 0.025, alpha = 0.5, show.legend = FALSE) +
  geom_point(data = airports_id, aes(lon, lat), col = "#9d0191", size = 0.02) +
  geom_point(data = airports_id, aes(lon, lat), col = "white", size = 0.02, alpha = 0.5) +
  labs(caption = "Jaringan penerbangan Indonesia\n@tom5ive - Data: openflights.org") +
  theme_ipsum_tw() +
  theme(plot.title = element_text(color = "white"),
        plot.caption = element_text(color = "white"),
        plot.background = element_rect(fill = "black"),
        panel.grid.major = element_blank(),
        plot.margin = grid::unit(c(3, 3, 3, 3), "mm"),
        axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())

ggsave("output/id_flights_fd.jpg", dpi = 500, width = 15, height = 8, units = "cm")

# Hammer edge
flights_id_he <- edge_bundle_hammer(flights_id, as.matrix(airports_id[,5:6]), bw = 0.3, decay = 0.2)
ggplot(flights_id_he) +
  geom_sf(data = id_prov, fill = NA, col = "grey", size = 0.05) +
  geom_path(aes(x, y, group=group), col = "#9d0191", size = 0.01, show.legend = FALSE) +
  geom_path(aes(x, y, group=group), col = "white", size = 0.005, alpha = 0.5, show.legend = FALSE) +
  geom_point(data = airports_id, aes(lon, lat), col = "#9d0191", size = 0.02) +
  geom_point(data = airports_id, aes(lon, lat), col = "white", size = 0.02, alpha = 0.5) +
  labs(caption = "Jaringan penerbangan Indonesia\n@tom5ive - Data: openflights.org") +
  theme_ipsum_tw() +
  theme(plot.title = element_text(color = "white"),
        plot.caption = element_text(color = "white"),
        plot.background = element_rect(fill = "black"),
        panel.grid.major = element_blank(),
        plot.margin = grid::unit(c(3, 3, 3, 3), "mm"),
        axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())

ggsave("output/id_flights_he.jpg", dpi = 500, width = 15, height = 8, units = "cm")


airports_id
length(unique(c(routes_id$from_airport, routes_id$to_airport)))
length(airports_id$IATA)
  # inner_join(airports_id, by = c("from_airport_id" = "airport_id", "to_airport_id" = "airport_id"))
  # filter(from_airport_id %in% airports_id$from_airport_id)
str(routes)
str(airports_id)
routes_id

routes %>%
  left_join(airports, by = from_airport_id)

routes
getwd()
?read_csv

routes
