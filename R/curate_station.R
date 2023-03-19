library(dplyr)
library(writexl)
library(readxl)
library(GRSPRSThesisData)

# load station ----
st <- 
  read_xlsx("xlsx/station/station.xlsx") %>% 
  filter(macrofauna == 1) %>% 
  select(Location, Station_zh, Station, Lat, Lon) %>% 
  # mutate(Location = paste0("2021.", Location)) %>% 
  mutate(Lon = as.numeric(Lon))
  
# rename stations
st[st$Station_zh == "龜山島I", 2] <- "龜山島(I)"
st[st$Station_zh == "龜山島II", 2] <- "龜山島(II)"

# reading station files
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

# rename stations
st[28,2] <- "加母子灣" # this line of code is specific to this analysis; remove if necessary 

# attach the chinese location name
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
st$Station_zh <- NULL

# add progress
mid2021 <- c("花嶼", "貓嶼", "澎澎灘", "西吉嶼", "東吉嶼", "小門嶼", "鯨魚洞", "北鐵砧", "青灣內灣", "姑婆嶼")
fin2021 <- c("頭巾嶼", "懷恩堂", "七美", "四角嶼", "東嶼坪","彭佳嶼","目斗嶼", "西嶼坪", "桶盤嶼", "雞籠嶼", "鼻頭","82.5","觀新", "蝙蝠洞", "龍洞", "小香蘭", "基隆嶼","協和","深澳", "大潭", "卯澳", "外木山", "和平島","花瓶嶼","虎井嶼", "桂安漁港, 馬崗", "潮境", "南鐵砧")
mid2022 <- c("烏石鼻", "磯崎", "新社", "石雨傘", "基翬", "加母子灣", "美人洞", "大福", "厚石", "杉福", "烏鬼洞", "中澳沙灘", "桂安漁港", "馬崗","青灣")
# fin2022 <- c(insert station names here)

# read size data
size <- read_xlsx("data/2022_OCA_final_macrofauna_size.xlsx")

fin2022 <- unique(size$Station)[!unique(size$Station) %in% c(mid2021, fin2021, mid2022)]

# add labels for mapping
st$Progress <- "已採樣，未納入分析"
st$Progress[st$Station %in% mid2021] <- "2021期中"
st$Progress[st$Station %in% fin2021] <- "2021期末"
st$Progress[st$Station %in% mid2022] <- "2022期中"
st$Progress[st$Station %in% fin2022] <- "2022期末"

write_xlsx(st, path = "data/2022_OCA_final_station.xlsx")
