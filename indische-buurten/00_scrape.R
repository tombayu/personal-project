library(tidyverse)
library(rvest)

rm(list = ls())

url <- "https://indischebuurten.nl/lexicon/oost-indische-buurten?start=0"

read_html(url)
//*[@id="glossarylist"]/tbody/tr[3]/td[2]/div[1]/p[3]/text()
