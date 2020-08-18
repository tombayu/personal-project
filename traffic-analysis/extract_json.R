library(jsonlite)
library(tidyverse)

rm(list = ls())

flow <- fromJSON("output/logic-apps/flow.json")$RWS$RW

extract <- function(ls, param) {
  ls %>%
    map(c(1,1,1)) %>%
    map(param)
}

###
s <- flow %>% map(1) %>% map(`[`, c(1:2)) #%>% 
  tibble(
    # PC = map(., ~ ext(x = .x, param = "PC")) %>% unlist()#,
    # DE = map(., ~ ext(x = .x, param = "DE")) %>% unlist()#,
    # QD = map(., ~ ext(x = .x, param = "QD")) %>% unlist(),
    LE = map(., ~ ext(x = .x, param = "LE")) %>% unlist()
  )

  
  
####
  sample <- flow %>% map(1) %>% map(`[`, c(1:5)) %>% 
    map(~ ext(x = .x, param = "DE")) %>%
    unlist()
  sample
  toJSON(s)
  write_rds(s, "here_traffic.rds")
  
    rename(c("data", "DE"))
  rename_all()

flow[[1]][["FIS"]][[1]][["FI"]][[1]][["TMC"]][["DE"]]
flow[[1]][["FIS"]][[2]][["FI"]][[1]][["TMC"]][["DE"]]

### repurrrsive

gh <- repurrrsive::gh_users
gh



###

library(jsonlite)
library(tidyverse)

flow <- fromJSON("output/logic-apps/flow.json")

extract <- function(ls, param) {
  ls %>%
    map(c(1,1,1)) %>%
    map(param)
}

###
traffic <- flow %>% map(1) %>% map(`[`, c(1:2)) %>%
  tibble(
  # PC = map(., ~ ext(x = .x, param = "PC")) %>% unlist()#,
  # DE = map(., ~ ext(x = .x, param = "DE")) %>% unlist()#,
  # QD = map(., ~ ext(x = .x, param = "QD")) %>% unlist(),
  LE = map(., ~ ext(x = .x, param = "LE")) %>% unlist()
)