library(tidyverse)
library(sf)
library(googleway)
library(jsonlite)

get_url <- function(id) {
  id <- gsub(" ", "%20", id)
  return(paste0("https://web.trafi.com/api/schedules/jakarta/schedule?scheduleId=", id, "&transportType=train"))
}

trains <- fromJSON("https://web.trafi.com/api/schedules/jakarta/all?transportType=train")[[1]] %>% unnest()
trains_jkt <- trains %>%
  mutate(api_url = map_chr(scheduleId, get_url),
         tanggal = Sys.Date(),
         info_rute = map(api_url, fromJSON),
         color = paste0("#", color)) %>%
  select(-transportNamePlural) %>%
  rename(moda = transportName, layanan_id = scheduleId, moda_id = transportId,
         nama_singkat = name, layanan = longName, warna = color, moda_icon = icon) %>%
  mutate(stasiun = map(info_rute, "stops"),
         rute = map(info_rute, "tracks"))

write_rds(trains_jkt, "jakarta-train/trains_jkt.rds")
