library(hereR)
library(ggplot2)
library(taskscheduleR)
library(sf)

getwd()
set_key("HaJ__TyYimIndF2qaAVdbdNBKSgATVYM1A6_y6aPRD8")

# Gather
sby <- st_read("D:/Spatial/Boundary/Surabaya.shp")
traffic <- flow(sby)

bjm <- st_read("D:/Spatial/Boundary/Banjarmasin.shp")
traffic <- flow(bjm)

bdg <- st_read("D:/Spatial/Boundary/Bandung.shp")
traffic <- flow(bdg)

saveRDS(traffic, "Nama_kota.rds")

# Visualize
ggplot(traffic) +
  geom_sf(aes(color = SP), size = 1.5) +
  scale_color_gradient(low = 'blue', high = 'yellow') +
  ggtitle("Banjarmasin")
