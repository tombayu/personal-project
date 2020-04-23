(time <- Sys.time())

library(sf)
library(hereR)
library(taskscheduleR)
library(lubridate)

set_key("HaJ__TyYimIndF2qaAVdbdNBKSgATVYM1A6_y6aPRD8")

(time <- Sys.time())

# Bandung
bdg <- st_read("C:/Users/Administrator/Documents/traffic/data/Bandung.shp")
traffic_bdg <- flow(bdg)

saveRDS(traffic_bdg, paste0("C:/Users/Administrator/Documents/traffic/output/bandung/Traffic_Bandung_", Sys.Date(), "_", hour(time), "h", minute(time), "m", ".rds"))

# Makassar
mks <- st_read("C:/Users/Administrator/Documents/traffic/data/Makassar.shp")
traffic_mks <- flow(mks)

saveRDS(traffic_mks, paste0("C:/Users/Administrator/Documents/traffic/output/makassar/Traffic_Makassar_", Sys.Date(), "_", hour(time), "h", minute(time), "m", ".rds"))

# Surabaya
sby <- st_read("C:/Users/Administrator/Documents/traffic/data/Surabaya.shp")
traffic_sby <- flow(sby)

saveRDS(traffic_sby, paste0("C:/Users/Administrator/Documents/traffic/output/surabaya/Traffic_Surabaya_", Sys.Date(), "_", hour(time), "h", minute(time), "m", ".rds"))

# Banjarmasin
bjm <- st_read("C:/Users/Administrator/Documents/traffic/data/Banjarmasin.shp")
traffic_bjm <- flow(bjm)

saveRDS(traffic_bjm, paste0("C:/Users/Administrator/Documents/traffic/output/banjarmasin/Traffic_Banjarmasin_", Sys.Date(), "_", hour(time), "h", minute(time), "m", ".rds"))
