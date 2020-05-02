(time <- Sys.time())

library(sf)
library(hereR)
library(taskscheduleR)
library(lubridate)

set_key("HaJ__TyYimIndF2qaAVdbdNBKSgATVYM1A6_y6aPRD8")

# Bandung
bdg <- st_read("C:/Users/Tombayu Hidayat/Documents/Coding/weekly-proj/traffic-analysis/data/Bandung.shp")
traffic_bdg <- flow(bdg)

saveRDS(traffic_bdg, paste0("C:/Users/Tombayu Hidayat/Documents/Coding/weekly-proj/traffic-analysis/output/bandung/Traffic_Bandung_", Sys.Date(), "_", hour(time), "h", minute(time), "m", ".rds"))

# Makassar
mks <- st_read("C:/Users/Tombayu Hidayat/Documents/Coding/weekly-proj/traffic-analysis/data/Makassar.shp")
traffic_mks <- flow(mks)

saveRDS(traffic_mks, paste0("C:/Users/Tombayu Hidayat/Documents/Coding/weekly-proj/traffic-analysis/output/makassar/Traffic_Makassar_", Sys.Date(), "_", hour(time), "h", minute(time), "m", ".rds"))

# Surabaya
sby <- st_read("C:/Users/Tombayu Hidayat/Documents/Coding/weekly-proj/traffic-analysis/data/Surabaya.shp")
traffic_sby <- flow(sby)

saveRDS(traffic_sby, paste0("C:/Users/Tombayu Hidayat/Documents/Coding/weekly-proj/traffic-analysis/output/surabaya/Traffic_Surabaya_", Sys.Date(), "_", hour(time), "h", minute(time), "m", ".rds"))

# DKI
dki <- st_read("C:/Users/Tombayu Hidayat/Documents/Coding/weekly-proj/traffic-analysis/data/DKI_Jakarta.shp")
traffic_dki <- flow(dki)

saveRDS(traffic_dki, paste0("C:/Users/Tombayu Hidayat/Documents/Coding/weekly-proj/traffic-analysis/output/dki/Traffic_DKI_", Sys.Date(), "_", hour(time), "h", minute(time), "m", ".rds"))
