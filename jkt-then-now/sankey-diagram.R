library(tidyverse)
library(networkD3)

df_luas <- read_csv("luas_then_now.csv")

tebet <- df_luas %>%
  filter(kecamatan == "Tebet") %>%
  select(kecamatan, kelas_then_2, kelas_now_2, luas)

tebet

p <- sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = "source",
                   Target = "target", Value = "value", NodeID = "name",
                   units = "TWh", fontSize = 12, nodeWidth = 30)



unique(df_luas$kelas_then_1)
unique(df_luas$kelas_now_1)


URL <- "https://cdn.rawgit.com/christophergandrud/networkD3/master/JSONdata/energy.json"
Energy <- jsonlite::fromJSON(URL)


# Now we have 2 data frames: a 'links' data frame with 3 columns (from, to, value), and a 'nodes' data frame that gives the name of each node.
head( Energy$links )
head( Energy$nodes )
Energy

src <- c("A", "A", "A", "A",
         "B", "B", "C", "C", "D")
target <- c("B", "C", "D", "J",
            "E", "F", "G", "H", "I")
networkData <- data.frame(src, target)
networkData

Energy$nodes
Energy$links
