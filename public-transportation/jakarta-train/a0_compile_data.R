library(tidyverse)
library(sf)

# Load the self-compiled data
lines <- readxl::read_xlsx("data/rekap.xlsx")

# Load Trafi-scraped data
dt_trafi <- read_rds("data/trains_jkt.rds")

# Load SSC data
"data/future-network.kml"
"data/current-network.kml"