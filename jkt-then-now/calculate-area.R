library(tidyverse)
library(sf)

rm(list = ls())

# Data then ---------------------------------------------------------------

fl_then <- list.files("data/vector/data_now/dump/then_per_kecamatan", full.names = T)
kecamatan_then <- fl_then %>%
  gsub("data/vector/data_now/dump/then_per_kecamatan/then_WADMKC_", "", .) %>%
  gsub(".gpkg", "", .) %>%
  stringr::str_to_title()
pl_kecamatan_then <- fl_then %>%
  lapply(st_read)


# Data now ----------------------------------------------------------------

fl_now <- list.files("data/vector/data_now/dump/now_per_kecamatan", full.names = T)
kecamatan_now <- fl_now %>%
  gsub("data/vector/data_now/dump/now_per_kecamatan/now_WADMKC_", "", .) %>%
  gsub(".gpkg", "", .) %>%
  stringr::str_to_title()
pl_kecamatan_now <- fl_now %>%
  lapply(st_read)


# Union, luas, save -------------------------------------------------------------------

df_luas <- data.frame(kecamatan = character(), kelas_then_1 = character(), kelas_then_2 = character(), 
                      kelas_now_1 = character(), kelas_now_2 = character(), luas = double())

#seq_along(fl_then)
for (i in seq_along(fl_then)) {
  cat(paste0("[", i, "/42] "))
  # union, hitung luas
  cat("Union... ")
  pl_union <- st_union(pl_kecamatan_then[[i]], pl_kecamatan_now[[i]]) %>% 
    mutate(luas = units::set_units(st_area(.), km^2), kecamatan = kecamatan_then[[i]]) %>%
    rename(kelas_then_1 = Kelas, kelas_then_2 = Kelas_2, kelas_now_1 = Kelas.1, kelas_now_2 = Kelas_2.1) %>%
    select(kecamatan, kelas_then_1, kelas_then_2, kelas_now_1, kelas_now_2, luas)
  # save polygon
  cat("Save polygon... ")
  pl_union %>%
    st_write(paste0("data/vector/data_now/dump/union/", kecamatan_then[[i]], ".gpkg"))
  # drop geometry, ambil df, tambahkan ke df_luas
  cat("Getting dataframe...\n")
  df_luas <- pl_union %>% 
    st_drop_geometry() %>%
    rbind(df_luas)
}

write_csv(df_luas, "luas_then_now.csv")


