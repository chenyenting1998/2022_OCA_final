library(dplyr)
library(writexl)
library(readxl)
library(GRSPRSThesisData)

# locate file
path_station <- dir("xlsx/station", ".xlsx", full.names = TRUE) 
  # grep("station",
  #      ,
  #      invert = TRUE,
  #      value = TRUE)

# load station ----
st <- 
  read_xlsx(path_station[1]) %>% 
  filter(macrofauna == 1) %>% 
  select(Location, Station_zh, Station, Lat, Lon) %>% 
  # mutate(Location = paste0("2021.", Location)) %>% 
  mutate(Lon = as.numeric(Lon))

st_liuqiu <- 
  read_xlsx(path_station[2]) %>% 
  mutate(Location = "Liuqiu") %>% 
  select(Location, Station_zh, Station, Lat, Lon)

st_taoyuan <-
  read_xlsx(path_station[3]) %>% 
  mutate(Location = "TY") %>% 
  select(Location, Station_zh, Station, Lat, Lon)

st <- 
  full_join(st, st_liuqiu) %>% 
  full_join(st_taoyuan)


write_xlsx(st, path = "data/station.xlsx")