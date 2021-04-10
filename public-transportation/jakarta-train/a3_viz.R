library(tidyverse)
library(sf)
library(gganimate)
library(extrafont)
library(ggtext)
library(glue)
library(pdftools)

# font_import(paths = c("C:/Users/Tombayu Hidayat/Documents/Designs/Fonts/montserrat/", prompt = F)) # to install font
loadfonts()

rm(list = ls())

theme_set(theme_minimal())
theme_update(panel.grid.major = element_blank(),
             axis.title.x = element_blank(), axis.text.x = element_blank(),
             axis.title.y = element_blank(), axis.text.y = element_blank(),
             legend.position = "none",
             plot.margin = margin(r = 1, l = 1, unit = "cm"),
             plot.title = element_text(family = "Montserrat ExtraBold", 
                                       color = "black",
                                       size = 30, 
                                       face = "bold",
                                       hjust = 0.5,
                                       margin = margin(t = 18, b = 0)),
             plot.subtitle = element_text(family = "Montserrat", 
                                          color = "grey40",
                                          size = 18, 
                                          face = "plain",
                                          hjust = 0.5,
                                          lineheight = 1.2,
                                          margin = margin(t = 6, b = 0)),
             plot.caption = element_textbox_simple(family = "Montserrat",
                                                   color = "grey40",
                                                   halign = 0.5,
                                                   margin = margin(t = 22, b = 16)))

# Basemaps
kota <- st_read("C:/Users/Tombayu Hidayat/Documents/Coding/personal-project/z_data-source/rbi/jadetabek_kota.geojson")
prov <- st_read("C:/Users/Tombayu Hidayat/Documents/Coding/personal-project/z_data-source/rbi/id_prov.geojson") %>%
  filter(NAME_1 %in% c("Jakarta Raya", "Jawa Barat", "Banten"))

# Trains
trains <- st_read("data/jakarta-train-network.geojson") %>%
  arrange(tahun_operasi)

# Indices to loop trough the year (beacuse gganimate curiously doesn't work..)
indices <- c(6, 7, 8, 11, 13, 16, 17, 19, 20, 23, 25, 27, 28, 29)
annotations <- tibble("desc" = c("Modernisasi angkutan KRL di Jabodetabek menjadi 5 rute utama",
                                 "Ekstensi layanan Green Line menuju Maja",
                                 "Layanan Nambo-Angke dibuka sebagai bagian dari Loop Line",
                                 "KA Bandara layanan BNI City - Bandara Soekarno-Hatta beroperasi, ekstensi Bekasi Line menuju Cikarang, ekstensi Green Line menuju Rangkasbitung",
                                 "Pengoperasian layanan MRT pertama di Indonesia pada Maret 2019, disusul LRT Jakarta pada Desember 2019",
                                 "LRT Jabodetabek beroperasi, menghubungkan Cibubur, Bekasi dengan Dukuh Atas",
                                 "Ekstensi jalur Utara-Selatan MRT Jakarta menuju Kota",
                                 "Ekstensi jalur Utara-Selatan MRT Jakarta menuju Ancol Barat, Loop Line LRT Jakarta menuju JIS",
                                 "Pengoperasian jalur Timur-Barat MRT Jakarta layanan Kalideres-Ujung Menteng",
                                 "Pengoperasian Loop Line MRT Jakarta layanan Fatmawati-TMII dan LRT Jakarta layanan Pulogebang, ekstensi Loop Line LRT Jakarta menuju Manggarai",
                                 "Ekstensi Loop Line MRT Jakarta menuju PIK dan LRT Jakarta layanan Pulogebang-Joglo",
                                 "Ekstensi Loop Line LRT Jakarta menuju Pesing/Karet dan layanan Pulogebang",
                                 "Ekstensi Loop Line LRT Jakarta JIS - Kedoya",
                                 "Pengoperasian LRT Jakarta layanan PIK - Kamal Muara")) 

# Color palletes according to the official color
col <- as.character(trains$warna)
names(col) <- as.character(trains$kode_rute)

for (i in seq_along(indices)) {
  
  trains_sl <- trains %>% slice_head(n = indices[i])
  year <- trains_sl %>% slice_tail(n = 1) %>% pull(tahun_operasi) %>% as.character()
  
  plot <- ggplot() +
    # geom_sf(data = prov, fill = "grey95", color = "white") +
    geom_sf(data = kota, fill = "grey93", color = "white") +
    geom_sf(data = trains, size = 1, color = "grey85") +
    geom_sf(data = trains_sl, size = 1, aes(color = kode_rute)) +
    coord_sf(xlim = c(106.5515, 107.0425), ylim = c(-6.461283, -6.089036)) +
    scale_color_manual(values = col) +
    labs(title = "Jaringan Kereta Api Jabodetabek",
         subtitle = "Perkembangan dan rencana 2011 - 2040",
         caption = glue("<span style='font-size:26pt; color:black;'>{year}</span><br><br>
                      <span style='font-size:14pt;'>Visualisasi oleh @tom5ive   •   Data: Trafi, jakarta.go.id</span>"))
  # plot
  ggsave(plot, filename = glue("output/pdf/trains_{year}.pdf"), width = 10, height = 10, device = cairo_pdf)
}

# Animate
pdfs <- list.files("output/pdf/", patter = "*.pdf", full.names = T)
for (pdf in pdfs) {
  pdf_convert(pdf, format = "png", dpi = 300)
}

here::here()
system("magick.exe -delay 70 *.png -delay 700 *latest_1.png -loop 0 corona_series_sum.gif")

?system
# If all viewers are open..
# for (i in dev.list()[1]:dev.list()[length(dev.list())]) {
#   dev.off()
# }

# # Play with layouts..
# trains_sl <- trains %>% slice_head(n = indices[3])
# year <- trains_sl %>% slice_tail(n = 1) %>% pull(tahun_operasi) %>% as.character()
# 
# plot <- ggplot() +
#   # geom_sf(data = prov, fill = "grey95", color = "white") +
#   geom_sf(data = kota, fill = "grey93", color = "white") +
#   geom_sf(data = trains, size = 1, color = "grey85") +
#   geom_sf(data = trains_sl, size = 1, aes(color = kode_rute)) +
#   coord_sf(xlim = c(106.5515, 107.0425), ylim = c(-6.461283, -6.089036)) +
#   scale_color_manual(values = col) +
#   labs(title = "Jaringan Kereta Api Jabodetabek",
#        subtitle = "Perkembangan dan rencana 2011 - 2040",
#        caption = glue("<span style='font-size:26pt; color:black;'>{year}</span><br>
#                       <span style='font-size:14pt;'>Visualisasi oleh @tom5ive   •   Data: Trafi, jakarta.go.id</span>"))
# # plot
# ggsave(plot, filename = glue("output/trains_{year}.pdf"), width = 10, height = 10, device = cairo_pdf)