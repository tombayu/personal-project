library(tidyverse)
library(sf)

trafi_data <- read_rds("jakarta-train/sp_kr_rute.rds") %>%
  filter(isHidden == F, direction == 1)
trafi_data
