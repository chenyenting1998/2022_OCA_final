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
  

st[st$Station_zh == "龜山島I", 2] <- "龜山島(I)"
st[st$Station_zh == "龜山島II", 2] <- "龜山島(II)"

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

st[st$Station == "加母子", "Station"] <- "加母子灣"
st[st$Station == "加母子", "Station_zh"] <- "加母子灣"

loc <-
  c("North" = "北台灣",
    "East" = "東台灣",
    "Penghu" = "澎湖",
    "Liuqiu" = "小琉球",
    "Kenting" = "墾丁",
    "Taoyuan" = "桃園")


st$Location_zh <- loc[match(st$Location, names(loc))]
st <- 
  st %>% 
  select(Location_zh, Location, Station_zh, Station, Lat, Lon) %>%
  mutate(Station = Station_zh)

# add progress
mea_2021 <- read_xlsx("data/2021_OCA_macrofauna_measurement.xlsx")
mea_2022 <- read_xlsx("data/2022_OCA_macrofauna_measurement.xlsx")

st$Progress <- "已採樣"
st$Progress[st$Station %in% unique(mea_2021$Station)] <- "2021"
st$Progress[st$Station %in% unique(mea_2022$Station)] <- "2022"

write_xlsx(st, path = "data/station.xlsx")
