# Toponimi
tp <- st_read("D:/Spatial/ID/ToponimiJabodetabek.shp")

st_write(tp, "data/ToponimiJadetabek.geojson")

# Kelurahan
desa <- st_read("D:/Spatial/ID/DesaJadetabek.shp") 
desa <- desa %>%
  select(NAME_1, NAME_2, NAME_3, NAME_4)

st_write(desa, "data/KelurahanJadetabek.geojson")

# Kota
kota <- st_read("D:/Spatial/ID/KotaJadetabek.shp")
kota <- kota %>%
  select(NAME_1, NAME_2)
st_write(kota, "data/KotaJadetabek.geojson")
