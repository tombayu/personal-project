library(tidyverse)
library(sf)

trafi_data <- read_rds("jakarta-train/trains_jkt.rds") #%>%
  filter(isHidden == F, direction == 1)
trafi_data
