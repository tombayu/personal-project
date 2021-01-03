library(tidyverse)
library(edgebundle)
library(igraph)
library(sf)
library(igraph)
library(gggraph)

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

for (i in 1:160) {
  nl <- get_nodes_links(tj[i,])
  
  nodes_list[[i]] <- nl[[1]]
  links_list[[i]] <- nl[[2]]
  
}

nodes <- bind_rows(nodes_list) %>%
  distinct()
links <- bind_rows(links_list)

network_k1 <- graph_from_data_frame(d = links, vertices = nodes,
                                                directed = F)
network_k1 <- simplify(network_k1)

### Experiment with visualization
## Force bundle
k1_fbundle <- edge_bundle_force(network_k1, as.matrix(nodes[,3:4]) * 100, compatibility_threshold = 0.1)

# Basic
ggplot(k1_fbundle)+
  geom_path(aes(x,y,group=group,col=as.factor(group)),size = 1,show.legend = FALSE)+
  geom_point(data = nodes, aes(lon * 100, lat * 100), size = 1)+
  ggraph::theme_graph(background = "black")

# 

p1 <- ggplot()+
  geom_polygon(data=states,aes(long,lat,group=group),col="white",size=0.1,fill=NA)+
  geom_path(data = fbundle,aes(x,y,group=group),col="#9d0191",size=0.05)+
  geom_path(data = fbundle,aes(x,y,group=group),col="white",size=0.005)+
  geom_point(data = verts,aes(x,y),col="#9d0191",size=0.25)+
  geom_point(data = verts,aes(x,y),col="white",size=0.25,alpha=0.5)+
  geom_point(data=verts[verts$name!="",],aes(x,y), col="white", size=3,alpha=1)+
  labs(title="Force Directed Edge Bundling")+
  ggraph::theme_graph(background = "black")+
  theme(plot.title = element_text(color="white"))
## 

g <- graph_from_edgelist(
  matrix(c(1,12,2,11,3,10,4,9,5,8,6,7),ncol=2,byrow = T),F)
xy <- cbind(c(rep(0,6),rep(1,6)),c(1:6,1:6))

fbundle <- edge_bundle_force(g,xy,compatibility_threshold = 0.1)
head(fbundle)

network_k1

V(network_k1)
nodes
g
xy
##
nodes
k1[[1]]
class(tj[1,])

r1 <- tj[1,]

koridor_1 <- tj %>%
  filter(kode_koridor == "1") %>%
  select(-halte) %>%
  unnest(cols = c(rute)) %>%
  filter(isHidden == F & direction == 1) %>%
  select(-shape) %>%
  unnest(cols = c(stops)) %>%
  select(stopId) %>%
  inner_join(tj_halte, by = c("stopId" = "halte_id"))

from_to <- koridor_1 %>%
  select("from" = stopId) %>%
  mutate(to = lead(from)) %>%
  slice(1:(n() - 1))
from_to

network_k1 <- graph_from_data_frame(d = from_to, vertices = koridor_1,
                      directed = F)
network_k1
plot(network_k1)

# function to batch 

# get 3 different corridors

# make edge bundle
# 

edge_bundle_force()

##
g <- graph_from_edgelist(
  matrix(c(1,12,2,11,3,10,4,9,5,8,6,7),ncol=2,byrow = T),F)
xy <- cbind(c(rep(0,6),rep(1,6)),c(1:6,1:6))
V(g)
g
xy

?edge_bundle_force
fbundle <- edge_bundle_force(g,xy,compatibility_threshold = 0.1)
head(fbundle)

##


str(nodes)
str(links)
head(nodes) # basically the halte
head(links) # basically the rute

net <- graph_from_data_frame(d = links, vertices = nodes,
                             directed = T)
1:5
lead(1:5)
?lag

names(koridor_1)

koridor_1$geometry

coord <- st_coordinates(koridor_1$geometry)
class(as.vector(coord[,1]))
length(coord[,1])
class(coord)
koridor_1$geometry
?st_coordinates

str(koridor_1)
koridor_1$stops
  
unnest_koridor_1 <- tj %>%
  filter(kode_koridor == "1") %>%
  unnest(c(rute))

  koridor_1 <- tj %>%
    filter(kode_koridor == "1") #%>%
  unnest(c(rute))
  
  
rute <- koridor_1$rute

class(koridor_1$rute)
koridor_1$rute %>%
  filter(isHidden == F)

head(koridor_1$rute)

?graph


####




xy <- cbind(state.center$x,state.center$y)[!state.name%in%c("Alaska","Hawaii"),]
xy_dummy <- tnss_dummies(xy,4)
gtree <- tnss_tree(cali2010,xy,xy_dummy,4,gamma = 0.9)

ggraph(gtree,"manual",x=V(gtree)$x,y=V(gtree)$y)+
  geom_polygon(data=us,aes(long,lat,group=group),fill="#FDF8C7",col="black")+
  geom_edge_link(aes(width=flow,col=sqrt((xy[root,1]-..x..)^2 + (xy[root,2]-..y..)^2)),
                 lineend = "round",show.legend = FALSE)+
  scale_edge_width(range=c(0.5,4),trans="sqrt")+
  scale_edge_color_gradient(low="#cc0000",high = "#0000cc")+
  geom_node_point(aes(filter=tnss=="real"),size=1)+
  geom_node_point(aes(filter=(name=="California")),size=5,shape=22,fill="#cc0000")+
  theme_graph()+
  labs(title="Migration from California (2010) - Flow map")


matrix(c(1,12,2,11,3,10,4,9,5,8,6,7),ncol=2,byrow = T)
g <- graph_from_edgelist(
  matrix(c(1,12,2,11,3,10,4,9,5,8,6,7),ncol=2,byrow = T),F)
xy <- cbind(c(rep(0,6),rep(1,6)),c(1:6,1:6))
fbundle <- edge_bundle_force(g,xy,compatibility_threshold = 0.1)
head(fbundle)


g
xy

?edge_bundle_force
g
xy
?graph_from_data_frame

cali2010
f <- us_flights
metro_berlin
?graph_from_edgelist

actors <- data.frame(name=c("Alice", "Bob", "Cecil", "David",
                            "Esmeralda"),
                     age=c(48,33,45,34,21),
                     gender=c("F","M","F","M","F"))
relations <- data.frame(from=c("Bob", "Cecil", "Cecil", "David",
                               "David", "Esmeralda"),
                        to=c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),
                        same.dept=c(FALSE,FALSE,TRUE,FALSE,FALSE,TRUE),
                        friendship=c(4,5,5,2,1,1), advice=c(4,5,5,4,2,3))
g <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)
print(g, e=TRUE, v=TRUE)

actors
relations
?graph_from_data_frame


##

TJ <- read_rds("data/tj/TJ.rds")

routes <- TJ %>%
  select(-halte) %>%
  unnest(cols = c(rute)) #%>%
  mutate(coords = map(shape, decode_pl), warna = paste0("#", warna)) %>%
  unnest(coords) %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  group_by_at(vars(-stops, -geometry)) %>%
  summarise(do_union = F) %>%
  st_cast("LINESTRING")
routes
