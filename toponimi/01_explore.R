pacman::p_load(magrittr, dplyr, sf, stringr, readr, ggplot2, ggthemes)
library(ggsflabel)
library(hrbrthemes)
library(extrafont)

extrafont::loadfonts()

rm(list = ls())

toponimi <- st_read("data/ToponimiJadetabek.geojson")
desa <- st_read("data/KelurahanJadetabek.geojson")
kota <- st_read("data/KotaJadetabek.geojson")
# provinsi <- st_read("data/Provinsi.geojson") %>%
#   filter(NAME_1 %in% c("Jawa Barat", "Jakarta Raya", "Banten"))

unique(toponimi$REMARK)

### FUNCTION
filter_toponimi <- function(pattern, toponimi = toponimi) {
  indeks <- toponimi %>%
    filter(str_detect(NAMOBJ, pattern)) %>%
    filter(!str_detect(NAMOBJ, " ")) %>%
    mutate(ID = 1:nrow(.)) %>%
    st_drop_geometry() %>%
    distinct(NAMOBJ, .keep_all = T) %>%
    select(NAMOBJ, ID)
  
  titik <- toponimi %>%
    filter(str_detect(NAMOBJ, pattern)) %>%
    filter(!str_detect(NAMOBJ, " "))
  
  label <- toponimi %>%
    filter(str_detect(NAMOBJ, pattern)) %>%
    filter(!str_detect(NAMOBJ, " ")) %>%
    mutate(ID = 1:nrow(.)) %>%
    filter(ID %in% indeks$ID)
  
  return(list(titik, label))
}

buat_peta <- function(lab, titik, title, subtitle, size = 2.5) {
  peta <- ggplot(lab) +
    geom_sf(data = kota, fill = "lightgrey", color = "white") +
    geom_sf(data = titik, aes(color = NAMOBJ), alpha = 0.5) +
    geom_sf_text_repel(aes(label = NAMOBJ), family = "Titillium Web", size = size) +
    theme_ipsum_tw() +
    labs(title = title,
         subtitle = subtitle,
         caption = "© 2020 Tombayu Amadeo Hidayat\nSumber data: RBI 25K - BIG") +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          panel.grid.major = element_blank(), legend.position = "none")
  return(peta)
}

### TOPONIMI

# Look at the summary!
# toponimi %>%
#   group_by(REMARK) %>%
#   summarise(n = n()) %>%
#   View()

## Kebon
kebon <- filter_toponimi("Kebon", toponimi)
buat_peta(kebon[[2]], kebon[[1]],
          title = "Macam-macam 'Kebon' di Jadetabek", subtitle = "Nama tempat di Jadetabek dengan awalan 'kebon'")
ggsave("output/01_Kebon.png", height = 13, width = 15, units = "cm", dpi = 400, bg = "transparent")

## Kampung
indeks_kampung <- toponimi %>%
  # filter(REMARK == "Wilayah Administrasi Kelurahan/Desa" | REMARK == "Permukiman dan Tempat Kegiatan") %>%
  filter(str_detect(NAMOBJ, "Kampung")) %>%
  filter(!str_detect(NAMOBJ, " ")) %>%
  mutate(ID = 1:nrow(.)) %>%
  st_drop_geometry() %>%
  distinct(NAMOBJ, .keep_all = T) %>%
  select(NAMOBJ, ID)

kampung_all <- toponimi %>%
  filter(str_detect(NAMOBJ, "Kampung")) %>%
  filter(!str_detect(NAMOBJ, " "))

kampung <- toponimi %>%
  filter(str_detect(NAMOBJ, "Kampung")) %>%
  filter(!str_detect(NAMOBJ, " ")) %>%
  mutate(ID = 1:nrow(.)) %>%
  filter(ID %in% indeks_kampung$ID) %>%
  mutate(NAMOBJ = gsub("Kampung", "", .$NAMOBJ))

ggplot(kampung) +
  geom_sf(data = kota, fill = "lightgrey", color = "white") +
  geom_sf(data = kampung_all,aes(color = NAMOBJ), alpha = 0.5) +
  geom_sf_text_repel(aes(label = NAMOBJ), family = "Titillium Web", size = 2.2) +
  theme_ipsum_tw() +
  labs(title = "Jakarta dan Kampungnya",
       subtitle = "Nama wilayah di Jadetabek dengan awalan 'kampung-'",
       caption = "© 2020 Tombayu Amadeo Hidayat\nSumber data: RBI 25K - BIG") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(), legend.position = "none")
ggsave("output/02_Kampung.png", height = 13, width = 15, units = "cm", dpi = 400, bg = "transparent")

## Pondok
indeks_pondok <- toponimi %>%
  # filter(REMARK == "Wilayah Administrasi Kelurahan/Desa" | REMARK == "Permukiman dan Tempat Kegiatan") %>%
  filter(str_detect(NAMOBJ, "Pondok")) %>%
  filter(!str_detect(NAMOBJ, " ")) %>%
  mutate(ID = 1:nrow(.)) %>%
  st_drop_geometry() %>%
  distinct(NAMOBJ, .keep_all = T) %>%
  select(NAMOBJ, ID)

pondok_all <- toponimi %>%
  filter(str_detect(NAMOBJ, "Pondok")) %>%
  filter(!str_detect(NAMOBJ, " "))

pondok <- toponimi %>%
  filter(str_detect(NAMOBJ, "Pondok")) %>%
  filter(!str_detect(NAMOBJ, " ")) %>%
  mutate(ID = 1:nrow(.)) %>%
  filter(ID %in% indeks_pondok$ID) %>%
  mutate(NAMOBJ = gsub("Pondok", "", .$NAMOBJ))

ggplot(pondok) +
  geom_sf(data = kota, fill = "lightgrey", color = "white") +
  geom_sf(data = pondok_all, aes(color = NAMOBJ), alpha = 0.5) +
  geom_sf_text_repel(aes(label = NAMOBJ), family = "Titillium Web", size = 2.5) +
  theme_ipsum_tw() +
  labs(title = "Ada 'Pondok' Apa Saja di Jadetabek?",
       subtitle = "Nama wilayah dengan awalan 'pondok-' di Jadetabek",
       caption = "© 2020 Tombayu Amadeo Hidayat\nSumber data: RBI 25K - BIG") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(), legend.position = "none")
ggsave("output/03_Pondok.png", height = 13, width = 15, units = "cm", dpi = 400, bg = "transparent")

## Jati
jati <- filter_toponimi("Jati", toponimi)
buat_peta(jati[[2]], jati[[1]],
          title = "Jati..?",
          subtitle = "Nama wilayah dengan awalan 'Jati-' di Jadetabek")
ggsave("output/04_Jati.png", height = 13, width = 15, units = "cm", dpi = 400, bg = "transparent")

## Hari
nama_hari <- c("Senin", "Selasa", "Rabu", "Rebo", "Kamis", "Jumat", "Sabtu", "Minggu") %>%
  str_c(collapse = "|")

indeks_hari <- toponimi %>%
  filter(grepl(nama_hari, NAMOBJ, ignore.case = T)) %>%
  mutate(ID = 1:nrow(.)) %>%
  st_drop_geometry() %>%
  distinct(NAMOBJ, .keep_all = T) %>%
  select(NAMOBJ, ID)

hari_all <- toponimi %>%
  filter(grepl(nama_hari, NAMOBJ, ignore.case = T)) %>%
  mutate(ID = 1:nrow(.))

hari <- toponimi %>%
  filter(grepl(nama_hari, NAMOBJ, ignore.case = T)) %>%
  mutate(ID = 1:nrow(.)) %>%
  filter(ID %in% indeks_hari$ID)

ggplot(hari) +
  geom_sf(data = kota, fill = "lightgrey", color = "white") +
  geom_sf(data = hari_all, aes(color = NAMOBJ), alpha = 0.5) +
  geom_sf_text_repel(aes(label = NAMOBJ), family = "Titillium Web", size = 2.5) +
  theme_ipsum_tw() +
  labs(title = "Jakarta Hanya Mengenal 3 Hari..",
       subtitle = "Wilayah dengan nama hari pada namanya",
       caption = "© 2020 Tombayu Amadeo Hidayat\nSumber data: RBI 25K - BIG") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(), legend.position = "none")
ggsave("output/05_Hari.png", height = 13, width = 15, units = "cm", dpi = 400, bg = "transparent")

#### Fitur geografi!

## Rawa
rawa <- filter_toponimi("Rawa", toponimi)
buat_peta(rawa[[2]], rawa[[1]],
          title = "Rawa", subtitle = "") + theme(plot.subtitle = element_blank())
ggsave("output/06_Rawa.png", height = 13, width = 15, units = "cm", dpi = 400, bg = "transparent")

## Lebak
lebak <- filter_toponimi("Lebak", toponimi)
buat_peta(lebak[[2]], lebak[[1]],
          title = "Lebak", subtitle = "Wilayah dengan awalan lebak-") + theme(plot.subtitle = element_blank())
ggsave("output/07_Lebak.png", height = 13, width = 15, units = "cm", dpi = 400, bg = "transparent")

## Bukit
bukit <- filter_toponimi("Bukit", toponimi)
buat_peta(bukit[[2]], bukit[[1]],
          title = "Bukit", subtitle = "Wilayah dengan awalan bukit-") + theme(plot.subtitle = element_blank())
ggsave("output/08_Bukit.png", height = 13, width = 15, units = "cm", dpi = 400, bg = "transparent")

## Gunung
gunung <- filter_toponimi("Gunung", toponimi)
buat_peta(gunung[[2]], gunung[[1]],
          title = "Gunung", subtitle = "Wilayah dengan awalan gunung-") + theme(plot.subtitle = element_blank())
ggsave("output/09_Gunung.png", height = 13, width = 15, units = "cm", dpi = 400, bg = "transparent")

## Ci-
ci <- filter_toponimi("Ci", toponimi)
buat_peta(ci[[2]], ci[[1]],
          title = "Ci", subtitle = "Wilayah dengan awalan ci-", size = 2) + theme(plot.subtitle = element_blank())
ggsave("output/10_Ci.png", height = 13, width = 15, units = "cm", dpi = 400, bg = "transparent")

## Tanah
tanah <- filter_toponimi("Tanah", toponimi)
buat_peta(tanah[[2]], tanah[[1]],
          title = "Tanah", subtitle = "Wilayah dengan awalan tanah-") + theme(plot.subtitle = element_blank())
ggsave("output/11_Tanah.png", height = 13, width = 15, units = "cm", dpi = 400, bg = "transparent")

## Try out
read_tsv()
ID <- read_tsv("D:/Spatial/ID/ID.txt", col_names = F)