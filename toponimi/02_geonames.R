pacman::p_load(magrittr, dplyr, sf, stringr, readr, ggplot2, ggthemes, purrr)
library(ggsflabel)
library(hrbrthemes)
library(extrafont)
library(ggsci)
library(ggpubr)

rm(list = ls())

# Load the geo-data
desa <- st_read("data/rbi/KelurahanJadetabek.geojson")
kota <- st_read("data/rbi/KotaJadetabek.geojson")

# Load the gazetteers, convert to geo-data
cols <- read_file("data/geonames/cols.txt") %>%
  str_split("\r\n") %>%
  unlist() %>%
  str_split(":") %>%
  map_chr(1) %>%
  str_trim()

gaz_jkt <- read_tsv("data/geonames/ID.txt", col_names = cols) %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(4326) %>%
  st_intersection(kota)

# Filter the names..
# The function
filter_toponimi <- function(pattern, toponimi = toponimi) {
  indeks <- toponimi %>%
    select(name, asciiname, alternatenames, feature.class, feature.code) %>%
    filter(str_detect(name, regex(pattern, ignore_case = T)) | str_detect(alternatenames, regex(pattern, ignore_case = T))) %>%
    # filter(!str_detect(name, regex("kelurahan|satu|dua|tiga|empat|timur|barat|utara|selatan|indah|tengah", ignore_case = T))) %>%
    filter(!str_detect(name, regex("kelurahan|satu|dua|tiga|empat|timur|barat|indah|tengah", ignore_case = T))) %>%
    mutate(shortname = gsub(paste0(".*", pattern), "", .$name, ignore.case = T)) %>%
    mutate(shortname = tolower(gsub(" ", "", .$shortname)), id = 1:nrow(.)) %>%
    st_drop_geometry() %>%
    distinct(shortname, .keep_all = T) %>%
    select(shortname, id)
  
  label <- toponimi %>%
    select(name, asciiname, alternatenames, feature.class, feature.code) %>%
    filter(str_detect(name, regex(pattern, ignore_case = T)) | str_detect(alternatenames, regex(pattern, ignore_case = T))) %>%
    # filter(!str_detect(name, regex("kelurahan|satu|dua|tiga|empat|timur|barat|utara|selatan|indah|tengah", ignore_case = T))) %>%
    filter(!str_detect(name, regex("kelurahan|satu|dua|tiga|empat|timur|barat|indah|tengah", ignore_case = T))) %>%
    mutate(shortname = gsub(paste0(".*", pattern), "", .$name, ignore.case = T)) %>%
    mutate(shortname = tolower(gsub(" ", "", .$shortname)), id = 1:nrow(.)) %>%
    filter(id %in% indeks$id)
  
  titik <- toponimi %>%
    select(name, asciiname, alternatenames, feature.class, feature.code) %>%
    filter(str_detect(name, regex(pattern, ignore_case = T)) | str_detect(alternatenames, regex(pattern, ignore_case = T))) %>%
    filter(!str_detect(name, regex("kelurahan", ignore_case = T))) %>%
    # filter(!str_detect(name, regex("satu", ignore_case = T))) %>%
    # filter(!str_detect(name, regex("dua", ignore_case = T))) %>%
    mutate(shortname = gsub(paste0(".*", pattern), "", .$name, ignore.case = T)) %>%
    mutate(shortname = tolower(gsub(" ", "", .$shortname)), id = 1:nrow(.))
  
  return(list(label = label, titik = titik))
}

buat_peta <- function(lab, titik, title, subtitle, size = 2.5) {
  peta <- ggplot(lab) +
    geom_sf(data = kota, fill = "lightgrey", color = "white") +
    geom_sf(data = titik, color = "steelblue2", alpha = 0.5) +
    geom_sf_text_repel(aes(label = shortname), family = "Titillium Web", size = size) +
    theme_ipsum_tw() +
    labs(title = title,
         subtitle = subtitle,
         caption = "© 2020 Tombayu Amadeo Hidayat\nSumber data: geonames.org, RBI25K") +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          panel.grid.major = element_blank(), legend.position = "none")
  return(peta)
}

buat_peta_mod <- function(lab, titik, title, subtitle, size = 2.5) {
  peta <- ggplot(lab) +
    geom_sf(data = kota, fill = "lightgrey", color = "white") +
    geom_sf(data = titik, color = "steelblue2", alpha = 0.5) +
    geom_sf_text_repel(aes(label = name), family = "Titillium Web", size = size) +
    theme_ipsum_tw() +
    labs(title = title,
         subtitle = subtitle,
         caption = "© 2020 Tombayu Amadeo Hidayat\nSumber data: geonames.org, RBI25K") +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          panel.grid.major = element_blank(), legend.position = "none")
  return(peta)
}

# LET'S ROLL!

## Kebon
kebon <- filter_toponimi("kebon", gaz_jkt)
buat_peta(kebon[[1]], kebon[[2]],
          title = "Macam-macam 'Kebon' di Jadetabek", 
          subtitle = "Toponimi dengan awalan 'kebon' di Jadetabek ")
ggsave("output/geonames/01_Kebon.png", height = 13, width = 15, units = "cm", dpi = 400, bg = "transparent")

## Kampung
kampung <- filter_toponimi("kampung", gaz_jkt)
buat_peta(kampung[[1]], kampung[[2]],
          title = "Jakarta: Sebuah Kampung Besar",
          subtitle = "Toponimi dengan awalan 'kampung-' di Jadetabek", size = 2)
ggsave("output/geonames/02_Kampung.png", height = 13, width = 15, units = "cm", dpi = 400, bg = "transparent")

## Pondok
pondok <- filter_toponimi("pondok", gaz_jkt)
pondok[[1]] <- pondok[[1]] %>%
  filter(!str_detect(shortname, regex("pemakaman|-hilir|parhentian|-udik", ignore_case = T)))
buat_peta(pondok[[1]], pondok[[2]],
          title = "Ada 'Pondok' Apa Saja di Jadetabek?",
          subtitle = "Toponimi dengan awalan 'pondok-' di Jadetabek", size = 2)
ggsave("output/geonames/03_Pondok.png", height = 13, width = 15, units = "cm", dpi = 400, bg = "transparent")

## Jati
jati <- filter_toponimi("jati", gaz_jkt)
buat_peta(jati[[1]], jati[[2]],
          title = "Jati..?",
          subtitle = "Nama wilayah dengan awalan 'Jati-' di Jadetabek")
ggsave("output/geonames/04_Jati.png", height = 13, width = 15, units = "cm", dpi = 400, bg = "transparent")

## Hari
nama_hari <- c("senen", "senin", "selasa", "rabu", "rebo", "kamis", "kemis", "jumat", "sabtu", "minggu")
possible_filter <- possibly(filter_toponimi, otherwise = NA_real_)

# hari <- nama_hari %>%
#   map(possible_filter, toponimi = gaz_jkt) %>%
#   map("label") %>%
#   compact() %>%
#   reduce(rbind) %>%
#   filter(!str_detect(name, "Amaris")) %>%
#   mutate(shortname = R.utils::capitalize(tolower(gsub(" ", "", .$name))))

hari <- st_read("data/geonames/pasar_hari.geojson") %>%
  select(-shortnm) %>%
  mutate(shortname = name)

semua_hari <- nama_hari %>%
  map(possible_filter, toponimi = gaz_jkt) %>%
  map("titik") %>%
  compact() %>%
  reduce(rbind)

buat_peta(hari, semua_hari,
          title = "Jakarta Hanya Mengenal 5 Hari?",
          subtitle = "Toponimi dengan nama hari")
ggsave("output/geonames/05_Hari.png", height = 13, width = 15, units = "cm", dpi = 400, bg = "transparent")

## Tanah
tanah <- filter_toponimi("Tanah", gaz_jkt)
buat_peta(tanah[[1]], tanah[[2]],
          title = "Tanah-tanah di Jakarta", subtitle = "Toponimi dengan awalan tanah-")
ggsave("output/geonames/10_Tanah.png", height = 13, width = 15, units = "cm", dpi = 400, bg = "transparent")

## Fitur geografi!
# Rawa
rawa <- filter_toponimi("Rawa", gaz_jkt)
buat_peta(rawa[[1]], rawa[[2]],
          title = "Rawa", subtitle = "", size = 1.9) + theme(plot.subtitle = element_blank())
ggsave("output/geonames/06_Rawa.png", height = 13, width = 15, units = "cm", dpi = 400, bg = "transparent")

## Lebak
lebak <- filter_toponimi("Lebak", gaz_jkt)
buat_peta(lebak[[1]], lebak[[2]],
          title = "Lebak", subtitle = "Wilayah dengan awalan lebak-") + theme(plot.subtitle = element_blank())
ggsave("output/geonames/07_Lebak.png", height = 13, width = 15, units = "cm", dpi = 400, bg = "transparent")

## Bukit
bukit <- filter_toponimi("Bukit", gaz_jkt)
buat_peta(bukit[[1]], bukit[[2]],
          title = "Bukit", subtitle = "Wilayah dengan awalan bukit-") + theme(plot.subtitle = element_blank())
ggsave("output/geonames/08_Bukit.png", height = 13, width = 15, units = "cm", dpi = 400, bg = "transparent")

## Gunung
gunung <- filter_toponimi("Gunung", gaz_jkt)
add <- gunung[[2]] %>% filter(name == "Gunung") %>% filter(feature.code == "PPLX")
gunung[[1]] <- rbind(gunung[[1]], add)
gunung[[1]] <- gunung[[1]] %>%
  mutate(shortname = R.utils::capitalize(tolower(gsub(" ", "", .$name))))

buat_peta(gunung[[1]], gunung[[2]],
          title = "Gunung", subtitle = "Wilayah dengan awalan gunung-") + theme(plot.subtitle = element_blank())
ggsave("output/geonames/09_Gunung.png", height = 13, width = 15, units = "cm", dpi = 400)

## Ci
ci <- filter_toponimi("Ci", gaz_jkt)
buat_peta_mod(ci[[1]], ci[[2]],
          title = "Ci- ci- an di Ja kar ta", subtitle = " :-)", size = 1.5)
ggsave("output/geonames/11_Ci.png", height = 13, width = 15, units = "cm", dpi = 400, bg = "transparent")
ci[[2]]
###########
frawa <- buat_peta_fgeo(rawa[[1]], rawa[[2]], "Rawa")
flebak <- buat_peta_fgeo(lebak[[1]], lebak[[2]], "Lebak")
fgunung <- buat_peta_fgeo(gunung[[1]], gunung[[2]], "Gunung")
fbukit <- buat_peta_fgeo(bukit[[1]], bukit[[2]], "Bukit")

geof <- ggarrange(frawa, flebak, fgunung, fbukit,
          nrow = 2, ncol = 2)
annotate_figure(geof, bottom = "© 2020 Tombayu Amadeo Hidayat\nSumber data: geonames.org, RBI25K")
ggsave("output/geonames/11_FiturGeografis.png", height = 13, width = 15, units = "cm", dpi = 400, bg = "transparent")

##############


id <- gaz_jkt %>%
  select(name, asciiname, alternatenames, feature.class, feature.code) %>%
  filter(str_detect(name, regex("kampung", ignore_case = T)) | str_detect(alternatenames, regex("kampung", ignore_case = T))) %>%
  filter(!str_detect(name, "kelurahan")) %>%
  mutate(shortname = gsub(".*kampung", "", .$name, ignore.case = T)) %>%
  mutate(shortname = tolower(gsub(" ", "", .$shortname)), id = 1:nrow(.)) %>%
  st_drop_geometry() %>%
  distinct(shortname, .keep_all = T) %>%
  select(shortname, id)

label <- gaz_jkt %>%
  select(name, asciiname, alternatenames, feature.class, feature.code) %>%
  filter(str_detect(name, regex("kampung", ignore_case = T)) | str_detect(alternatenames, regex("kampung", ignore_case = T))) %>%
  filter(!str_detect(name, "kelurahan")) %>%
  mutate(shortname = gsub(".*kampung", "", .$name, ignore.case = T)) %>%
  mutate(shortname = tolower(gsub(" ", "", .$shortname)), id = 1:nrow(.))

titik <- gaz_jkt %>%
  select(name, asciiname, alternatenames, feature.class, feature.code) %>%
  filter(str_detect(name, regex("kampung", ignore_case = T)) | str_detect(alternatenames, regex("kampung", ignore_case = T))) %>%
  filter(!str_detect(name, "kelurahan"))
id  
label

?str_detect
kampung