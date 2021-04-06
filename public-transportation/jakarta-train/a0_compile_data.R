library(tidyverse)
library(sf)
library(googleway)

# Load the self-compiled data
lines <- readxl::read_xlsx("data/rekap.xlsx")

# Load Trafi-scraped data
dt_trafi <- read_rds("data/trains_jkt.rds")

rute <- dt_trafi %>%
  select(-info_rute, -stasiun) %>%
  unnest(cols = c(rute)) %>%
  mutate(koordinat = map(shape, decode_pl)) %>%
  unnest(koordinat) %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  group_by_at(vars(-stops, -geometry)) %>%
  summarise(do_union = F) %>%
  st_cast("LINESTRING") %>%
  select(-shape, -geometry)
st_crs(rute) <- 4326

st_write(rute, "data/raw/rute_trafi.geojson")


