pacman::p_load(magrittr, dplyr, sf, stringr)

toponimi <- st_read("data/ToponimiJadetabek.geojson")
desa <- st_read("data/KelurahanJadetabek.geojson")
kota <- st_read("data/KotaJadetabek.geojson")

unique(toponimi$REMARK)

### TOPONIMI

# Look at the summary!
toponimi %>%
  group_by(REMARK) %>%
  summarise(n = n()) %>%
  View()

# Kebon
kebon <- toponimi %>%
  #filter(REMARK == "Wilayah Administrasi Kelurahan/Desa" | REMARK == "Permukiman dan Tempat Kegiatan") %>%
  filter(str_detect(NAMOBJ, "Kebon")) #%>%
  filter(!str_detect(NAMOBJ, " "))
kebon
unique(kebon$NAMOBJ)

k <- toponimi %>%
  filter(REMARK == "Permukiman dan Tempat Kegiatan") %>%
  filter(str_detect(NAMOBJ, "Kebon"))
unique(k$NAMOBJ)

# Kampung
toponimi %>%
  filter()

###

### KELURAHAN
desakebon <- desa %>%
  select(NAME_4) %>%
  filter(str_detect(NAME_4, "Kebon"))
unique(desakebon$NAME_4)
