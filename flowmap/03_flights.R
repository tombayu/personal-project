library(tidyverse)
library(edgebundle)
library(igraph)
library(sf)
library(igraph)
library(hrbrthemes)

airports <- read_csv("data/flights/airports.dat.txt", col_names = c("airport_id", "name", "city", "country", "IATA", "ICAO",
                                                                    "lat", "lon", "alt", "tz", "dst", "tz_db", "type", "source"))
routes <- read_csv("data/flights/routes.dat.txt", col_names = c("airlines", "airlines_id", 
                                                                "from_airport", "from_airport_id", "to_airport", "to_airport_id",
                                                                "codeshare", "stops", "equipment"))

routes %>%
  left_join(airports, by = )

routes
getwd()
?read_csv

routes
