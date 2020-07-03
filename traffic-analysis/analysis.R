library(tidyverse)
library(lubridate)
library(sf)

rm(list = ls())

fl <- list.files("output/bandung/")

df_traffic <- data.frame("filename" = fl) %>%
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

view(df_traffic)

df_cipaganti <- tibble(datetime = POSIXct(), JF = double())

for (i in 1101:length(fl)) {
  cat(paste0(i, " "))
  tf <- read_rds(paste0("output/bandung/", df_traffic$filename[[i]])) %>%
    filter(PC == 9560, QD == "+")
  df_cipaganti <- df_cipaganti %>%
    add_row(datetime = df_traffic$datetime_id[[i]], JF = tf$JF)
  #cipaganti <- c(cipaganti, tf$JF)
}
Sys.time()
df_cipaganti

