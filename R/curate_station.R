library(dplyr)
library(writexl)
library(readxl)
library(GRSPRSThesisData)

# locate file
# path_station <- dir("xlsx/station", ".xlsx", full.names = TRUE) 

# load station ----
st <- 
  read_xlsx("xlsx/station/station.xlsx") %>% 
  filter(macrofauna == 1) %>% 
  select(Location, Station_zh, Station, Lat, Lon) %>% 
  # mutate(Location = paste0("2021.", Location)) %>% 
  mutate(Lon = as.numeric(Lon))

st_liuqiu <- 
  read_xlsx("xlsx/station/station_liuqiu.xlsx") %>% 
  mutate(Location = "Liuqiu") %>% 
  select(Location, Station_zh, Station, Lat, Lon)

st_taoyuan <-
  read_xlsx("xlsx/station/station_taoyuan.xlsx") %>% 
  mutate(Location = "Taoyuan") %>% 
  select(Location, Station_zh, Station, Lat, Lon)

st_kenting <- 
  read_xlsx("xlsx/station/station_kenting.xlsx") %>% 
  mutate(Location = "Kenting") %>% 
  select(Location, Station_zh, Station, Lat, Lon)

st <- 
  full_join(st, st_liuqiu) %>% 
  full_join(st_taoyuan) %>% 
  full_join(st_kenting)

loc <-
  c("North" = "北台灣",
    "East" = "東台灣",
    "Penghu" = "澎湖",
    "Liuqiu" = "小琉球",
    "Kenting" = "墾丁",
    "Taoyuan" = "桃園")


st$Location_zh <- loc[match(st$Location, names(loc))]
st <- st %>% select(Location_zh, Location, Station_zh, Station, Lat, Lon)


write_xlsx(st, path = "data/station.xlsx")
