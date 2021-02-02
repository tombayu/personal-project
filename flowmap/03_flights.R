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

# Edge bundling and visualization
id_prov <- st_read("data/rbi/id_prov.geojson") %>%
  mutate(geometry = geometry)

# Force directed
ct_seq <- seq(0, 1, 0.1)
for (i in seq_along(ct_seq)) {
  cat(paste0(i, " "))
  ct = ct_seq[i]
  ln_color = "white"
  flights_id_fd <- edge_bundle_force(flights_id, as.matrix(airports_id[,5:6]), compatibility_threshold = ct)
  fd_plot <- ggplot(flights_id_fd) +
    geom_sf(data = id_prov, fill = NA, col = "grey", size = 0.05) +
    geom_path(aes(x, y, group=group), col = "#9d0191", size = 0.1, show.legend = FALSE) +
    geom_path(aes(x, y, group=group), col = ln_color, size = 0.05, alpha = 0.3, show.legend = FALSE) +
    geom_point(data = airports_id, aes(lon, lat), col = "#9d0191", size = 0.1) +
    geom_point(data = airports_id, aes(lon, lat), col = "white", size = 0.09, alpha = 0.5) +
    labs(caption = c("@tom5ive\ndata: openflights.org", paste0("\nThreshold: ", ct))) +
    theme_ipsum_tw() +
    theme(plot.title = element_text(color = "white"),
          plot.caption = element_text(hjust = c(1, 0), color = "white"),
          plot.background = element_rect(fill = "black"),
          panel.grid.major = element_blank(),
          plot.margin = grid::unit(c(3, 3, 3, 3), "mm"),
          axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
  
  ggsave(paste0("output/fd/id_flights_fd", "_", ct, "_", ln_color, ".jpg"), plot = fd_plot, dpi = 200, width = 15, height = 8, units = "cm")
  
}

## Hammer edge
## Loop to see different parameter effect?
seq_comb <- expand.grid(seq(0, 1, 0.1), seq(0.1, 1, 0.1))
for (i in 1:nrow(seq_comb)) {
  cat(paste0(i, " "))
  bw = as.double(seq_comb[i,][1])
  decay = as.double(seq_comb[i,][2])
  ln_color = "white"
  flights_id_he <- edge_bundle_hammer(flights_id, as.matrix(airports_id[,5:6]), bw = bw, decay = decay)
  he_plot <- ggplot(flights_id_he) +
    geom_sf(data = id_prov, fill = NA, col = "grey", size = 0.05) +
    geom_path(aes(x, y, group=group), col = "#9d0191", size = 0.1, show.legend = FALSE) +
    geom_path(aes(x, y, group=group), col = ln_color, size = 0.05, alpha = 0.3, show.legend = FALSE) +
    geom_point(data = airports_id, aes(lon, lat), col = "#9d0191", size = 0.1) +
    geom_point(data = airports_id, aes(lon, lat), col = "white", size = 0.09, alpha = 0.5) +
    labs(caption = c("@tom5ive\ndata: openflights.org", paste0("bw = ", bw, "\ndecay =", decay))) +
    theme_ipsum_tw() +
    theme(plot.title = element_text(color = "white"),
          plot.caption = element_text(hjust = c(1, 0), color = "white"),
          plot.background = element_rect(fill = "black"),
          panel.grid.major = element_blank(),
          plot.margin = grid::unit(c(3, 3, 3, 3), "mm"),
          axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
  
  ggsave(paste0("output/he/id_flights_he", "_bw", bw, "_decay", decay, "_", ln_color, ".jpg"), plot = he_plot, dpi = 200, width = 15, height = 8, units = "cm")
}

## BEST OF..
# Best of hammer edge
bw = 1
decay = 0.2
flights_id_he <- edge_bundle_hammer(flights_id, as.matrix(airports_id[,5:6]), bw = bw, decay = decay)
he_plot <- ggplot(flights_id_he) +
  geom_sf(data = id_prov, fill = NA, col = "grey", size = 0.05) +
  geom_path(aes(x, y, group=group), col = "#9d0191", size = 0.1, show.legend = FALSE) +
  geom_path(aes(x, y, group=group), col = ln_color, size = 0.05, alpha = 0.3, show.legend = FALSE) +
  geom_point(data = airports_id, aes(lon, lat), col = "#9d0191", size = 0.1) +
  geom_point(data = airports_id, aes(lon, lat), col = "white", size = 0.09, alpha = 0.5) +
  labs(caption = c("\n@tom5ive", "Jaringan penerbangan domestik di Indonesia\nData: openflights.org")) +
  theme_ipsum_tw() +
  theme(plot.title = element_text(color = "white"),
        plot.caption = element_text(hjust = c(1, 0), color = "white"),
        plot.background = element_rect(fill = "black"),
        panel.grid.major = element_blank(),
        plot.margin = grid::unit(c(3, 3, 3, 3), "mm"),
        axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
he_plot
ggsave(paste0("output/post/id_flights_he", "_bw", bw, "_decay", decay, "_", ln_color, ".jpg"), plot = he_plot, dpi = 500, width = 15, height = 8, units = "cm")

# Best of force directed
ct = 0.25
ln_color = "white"
flights_id_fd <- edge_bundle_force(flights_id, as.matrix(airports_id[,5:6]), compatibility_threshold = ct)
fd_plot <- ggplot(flights_id_fd) +
  geom_sf(data = id_prov, fill = NA, col = "grey", size = 0.05) +
  geom_path(aes(x, y, group=group), col = "#9d0191", size = 0.1, show.legend = FALSE) +
  geom_path(aes(x, y, group=group), col = ln_color, size = 0.05, alpha = 0.3, show.legend = FALSE) +
  geom_point(data = airports_id, aes(lon, lat), col = "#9d0191", size = 0.1) +
  geom_point(data = airports_id, aes(lon, lat), col = "white", size = 0.09, alpha = 0.5) +
  labs(caption = c("\n@tom5ive", "Jaringan penerbangan domestik di Indonesia\nData: openflights.org")) +
  theme_ipsum_tw() +
  theme(plot.title = element_text(color = "white"),
        plot.caption = element_text(hjust = c(1, 0), color = "white"),
        plot.background = element_rect(fill = "black"),
        panel.grid.major = element_blank(),
        plot.margin = grid::unit(c(3, 3, 3, 3), "mm"),
        axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())

ggsave(paste0("output/post/id_flights_fd", "_", ct, "_", ln_color, ".jpg"), plot = fd_plot, dpi = 500, width = 15, height = 8, units = "cm")
