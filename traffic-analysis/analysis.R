library(tidyverse)
library(lubridate)
library(sf)
library(leaflet)

rm(list = ls())

ref <- read_rds("data/bandung_ref.rds")

city <- "bandung"
roadid <- 9560
hr <- 9
min <- 0

fl <- list.files(paste0("output/", city))

df_traffic <- tibble("filename" = fl) %>%
  separate(filename, c("type", "region", "date", "time"), sep = "_", remove = F) %>%
  select(filename, region, date, time) %>%
  mutate(time = gsub(".rds", "", time)) %>%
  separate(time, c("hour", "minute"), sep = "h") %>%
  mutate(minute = gsub("m", "", minute)) %>%
  mutate(minute = if_else(str_length(minute) == 1, paste0("0", minute), minute), 
         hour = if_else(str_length(hour) == 1, paste0("0", hour), hour),
         "datetime_nl" = ymd_hm(paste0(date, " ", hour, ":", minute), tz = "Europe/Amsterdam"),
         "datetime_id" = with_tz(datetime_nl, tzone = "Asia/Jakarta")) %>%
  arrange(datetime_id) %>%
  select(filename, region, datetime_id)

df_9am <- df_traffic %>%
  filter(hour(datetime_id) == hr) %>%
  filter(minute(datetime_id) == min)

df_cipaganti <- tibble(datetime = POSIXct(), SP = double(), FF = double(), JF = double(), CN = double())

for (i in seq_along(df_9am$filename)) {
  # Messages
  if (i == 1) {
    cat(paste0("[", length(df_9am$filename), "] "))
  }
  cat(paste0(i, " "))
  
  tf <- read_rds(paste0("output/bandung/", df_9am$filename[[i]])) %>%
    left_join(ref) %>% 
    distinct(PC, .keep_all = T) %>%
    filter(PC == 9560, QD == "-")
  df_cipaganti <- df_cipaganti %>%
    add_row(datetime = df_9am$datetime_id[[i]], SP = tf$SP, FF = tf$FF, JF = tf$JF, CN = tf$CN)
}

### The magic piping to assign the correct road names..
s <- read_rds(paste0("output/bandung/", df_traffic[1,]$filename))
joined <- s %>% left_join(ref) %>% distinct(PC, .keep_all = T)

