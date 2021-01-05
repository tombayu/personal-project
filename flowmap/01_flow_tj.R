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

get_nodes_links <- function(koridor_data) {
  nodes <- koridor_data %>%
    select(-halte) %>%
    unnest(cols = c(rute)) %>%
    filter(isHidden == F & direction == 1) %>%
    select(-shape) %>%
    unnest(cols = c(stops)) %>%
    select(stopId) %>%
    inner_join(tj_halte, by = c("stopId" = "halte_id"))
  
  links <- nodes %>%
    select("from" = stopId) %>%
    mutate(to = lead(from)) %>%
    slice(1:(n() - 1))
  
  return(list(nodes, links))
}

## Koridor 1 dan anak-anaknya
links_list <- list()
nodes_list <- list()

for (i in 1:nrow(tj)) {
  nl <- get_nodes_links(tj[i,])
  
  nodes_list[[i]] <- nl[[1]]
  links_list[[i]] <- nl[[2]]
  
}

nodes <- bind_rows(nodes_list) %>%
  distinct()
links <- bind_rows(links_list)

nw_tj <- graph_from_data_frame(d = links, vertices = nodes,
                                                directed = F)
nw_tj <- simplify(nw_tj)

### Experiment with visualization
dki <- st_read("data/rbi/KotaJadetabek.geojson") %>%
  mutate(geometry = geometry * 100) %>%
  filter(str_detect(NAME_1, "Jakarta"))

## Force bundle
tj_fbundle <- edge_bundle_force(nw_tj, as.matrix(nodes[,3:4]) * 100, compatibility_threshold = 0.05)

# Basic
ggplot(tj_fbundle) +
  geom_sf(data = dki, fill = NA, col = "grey", size = 0.1, linetype = "dashed") +
  geom_path(aes(x, y, group=group), col = "#9d0191", size = 0.2, show.legend = FALSE) +
  geom_path(aes(x, y, group=group), col = "white", size = 0.1, alpha = 0.5, show.legend = FALSE) +
  # geom_point(data = nodes, aes(lon * 100, lat * 100), col = "#9d0191", size = 0.02) +
  # geom_point(data = nodes, aes(lon * 100, lat * 100), col = "white", size = 0.02, alpha = 0.5) +
  labs(title = "Jakarta's transportation flow", caption = "@tom5ive\nData: Trafi") +
  theme_ipsum_tw() +
  theme(plot.title = element_text(color = "white"),
        plot.caption = element_text(color = "white"),
        plot.background = element_rect(fill = "black"),
        panel.grid.major = element_blank(),
        plot.margin = grid::unit(c(3, 3, 3, 3), "mm"),
        axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())

ggsave("output/tj_fbundle_all_005.jpg", dpi = 500, width = 10, height = 10, units = "cm")

## Bundle stub
tj_stub <- edge_bundle_stub(nw_tj, as.matrix(nodes[,3:4]) * 100)

ggplot(tj_stub)+
  geom_bezier(aes(x,y,group=group),size=2,col="grey66")+
  geom_point(data=as.data.frame(xy),aes(V1,V2),size=5)+
  theme_void()

## Hammer
# v1: default
# v2: bw = 0.7, decay = 0.5
# v3: bw = 0.1, decay = 0.1
tj_hammer <- edge_bundle_hammer(nw_tj, as.matrix(nodes[,3:4]) * 100)
?edge_bundle_hammer

important_stops <- nodes %>%
  filter(n_koridor > 10)

ggplot(tj_hammer) +
  geom_sf(data = dki, fill = NA, col = "grey", size = 0.1, linetype = "dashed") +
  geom_path(aes(x, y, group=group), col = "#9d0191", size = 0.1, show.legend = FALSE) +
  geom_path(aes(x, y, group=group), col = "white", size = 0.05, alpha = 0.5, show.legend = FALSE) +
  geom_point(data = important_stops, aes(lon * 100, lat * 100, size = n_koridor), col = "steelblue2", alpha = 0.2, shape = 19) +
  geom_point(data = important_stops, aes(lon * 100, lat * 100), col = "steelblue2", size = 0.25) +
  # geom_text(data = important_stops, aes(lon * 100, lat * 100, label = nama_halte), size = 1, color = "steelblue2") +
  labs(caption = "Alur transportasi Jakarta\n@tom5ive - Data: Trafi") +
  theme_ipsum_tw() +
  theme(plot.title = element_text(color = "white"),
        plot.caption = element_text(color = "white"),
        plot.background = element_rect(fill = "black"),
        panel.grid.major = element_blank(),
        plot.margin = grid::unit(c(3, 3, 3, 3), "mm"),
        axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = "none")

ggsave("output/tj_hammer_all_wpoint_v1.2.jpg", dpi = 500, width = 10, height = 10, units = "cm")

## 

