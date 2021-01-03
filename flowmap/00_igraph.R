library(igraph)
library(ggplot2)

plot(graph_from_literal("Blok M"--"Kota", "Harmoni"--"Juanda"))

g4 <- graph(c("John", "Jim", "Jim", "Jack", "Jim", "Jack", "John", "John"), 
            isolates = c("Jesse", "Janis", "Jennifer", "Justin"))  

plot(g4)
E(g4)
V(g4)

## network from dataframe

nodes <- read.csv("data_tutorial/Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
links <- read.csv("data_tutorial/Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)

# head(nodes)
# head(links)
# nrow(nodes); length(unique(nodes$id))
# nrow(links); nrow(unique(links[,c("from", "to")]))

links <- aggregate(links[,3], links[,-3], sum)
links <- links[order(links$from, links$to),]
colnames(links)[4] <- "weight"
rownames(links) <- NULL

str(nodes)
str(links)
head(nodes) # basically the halte
head(links) # basically the rute

net <- graph_from_data_frame(d = links, vertices = nodes,
                             directed = T)
class(net)
E(net)
V(net)
E(net)$type
V(net)$media

plot(net, edge.arrow.size = .05)

net <- simplify(net, remove.multiple = F, remove.loops = T)
as_edgelist(net, names = T)
as_adjacency_matrix(net, attr = "weight")
as_data_frame(net, what="edges")
as_data_frame(net, what="vertices")

plot(net, edge.arrow.size = .05, 
     vertex.label = V(net)$media)

colrs <- c("gray50", "tomato", "gold")
V(net)$color <- colrs[V(net)$media.type]
V(net)$size <- V(net)$audience.size*0.7
V(net)$label.color <- "black"
V(net)$label <- NA
E(net)$width <- E(net)$weight/6

E(net)$arrow.size <- .2
E(net)$edge.color <- "gray80"
E(net)$width <- 1+E(net)$weight/12

plot(net, edge.color="orange", vertex.color="gray50",
     edge.arrow.size = 0.05) 

## exploring network coordinates
net.bg <- sample_pa(80)
V(net.bg)$size <- 8
V(net.bg)$frame.color <- "white"
V(net.bg)$color <- "orange"
V(net.bg)$label <- "" 
E(net.bg)$arrow.mode <- 0
plot(net.bg)

l <- layout_in_circle(net.bg)
plot(net.bg, layout=l)
l
