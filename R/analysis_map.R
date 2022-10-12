library(readxl) 
library(writexl) 
library(ggplot2) 
library(dplyr) 
library(patchwork) 
library(ncdf4)

# load files
st <- read_xlsx("data/station.xlsx")
load("data/loc_and_prog_color_code.RData")
prog_color <- c("已採樣，未納入分析"="green", 
                "2021期中" = "red",
                "2021期末" = "blue",
                "2022期中" = "purple",
                "2022期末" = "orange")

# read source
source("source/map_func.R")
source("source/msjh.R")

# load GEBCO 2020 ----
map_nc_path <- dir(path = "GEBCO_2020", pattern = "2020.nc", full.names = T)
map_nc <- nc_open(map_nc_path)

# extract lon. and lat.
Lon <- ncvar_get(map_nc, "lon")
Lat <- ncvar_get(map_nc, "lat")
Ele <- ncvar_get(map_nc, "elevation")

# Extract tw map
map <- 
  expand.grid(Lon = Lon,
              Lat = Lat) %>%
  cbind(Ele = as.vector(Ele)) %>% 
  as.data.frame() %>% 
  filter(Lon > 119 & Lon < 122.5 & Lat > 21.5 & Lat < 25.8)

# remove 
rm(list = c("map_nc", "Lon", "Lat", "Ele"))

# plot tw ----
tw <- 
  plot_map(map) + 
  add_tw_st(st, unique(st$Location_zh))+
  scale_color_manual(values = loc_color) +
  theme(legend.position = c(0.15,0.20)) +
  guides(color = guide_legend(title = "Location"))

# plot location ----
# `location`_corner
e_c <- data.frame(Lon = c(121.2, 121.9), Lat = c(23, 24.51))
l_c <- data.frame(Lon = c(120.325, 120.415), Lat = c(22.3, 22.38))
n_c <- data.frame(Lon = c(121.45, 122.3), Lat = c(24.7, 25.7))
p_c <- data.frame(Lon = c(119.2, 119.8), Lat = c(23.1, 23.9))
t_c <- data.frame(Lon = c(121.1, 121.3), Lat = c(25.06, 25.21))
k_c <- data.frame(Lon = c(120.6, 121), Lat = c(21.75, 22.05))

# subset map
e_map <- map %>% filter(Lon > min(e_c$Lon) & Lon < max(e_c$Lon) & Lat > min(e_c$Lat) & Lat < max(e_c$Lat))
l_map <- map %>% filter(Lon > min(l_c$Lon) & Lon < max(l_c$Lon) & Lat > min(l_c$Lat) & Lat < max(l_c$Lat))
n_map <- map %>% filter(Lon > min(n_c$Lon) & Lon < max(n_c$Lon) & Lat > min(n_c$Lat) & Lat < max(n_c$Lat))
p_map <- map %>% filter(Lon > min(p_c$Lon) & Lon < max(p_c$Lon) & Lat > min(p_c$Lat) & Lat < max(p_c$Lat))
t_map <- map %>% filter(Lon > min(t_c$Lon) & Lon < max(t_c$Lon) & Lat > min(t_c$Lat) & Lat < max(t_c$Lat))
k_map <- map %>% filter(Lon > min(k_c$Lon) & Lon < max(k_c$Lon) & Lat > min(k_c$Lat) & Lat < max(k_c$Lat))

# plot progress

theme(legend.text = element_text(family = msjh))
  
e <- 
  plot_map(e_map) + add_st(st, loc = "東台灣") + 
  add_st_lab(st, loc = "東台灣", s = 3) +
  scale_color_manual(values = prog_color) +
  scale_fill_manual(values = prog_color) +
  theme(legend.text = element_text(family = msjh),
        legend.title = element_text(family = msjh)) +
  guides(color = guide_legend(title = "分析進度",
                              override.aes = list(size = 8)),
         fill = "none")+
  theme(legend.position = c(0.7,0.20))
  

n <- plot_map(n_map) + 
  add_st(st, loc = "北台灣") + 
  add_st_lab(st, loc = "北台灣", s = 100) + 
  ht +
  scale_color_manual(values = prog_color) +
  scale_fill_manual(values = prog_color) +
  ht

l <- plot_map(l_map) + 
  add_st(st, loc = "小琉球") + 
  add_st_lab(st, loc = "小琉球") + 
  scale_color_manual(values = prog_color) +
  scale_fill_manual(values = prog_color) +
  ht

p <- plot_map(p_map) + 
  add_st(st, loc = "澎湖") + 
  add_st_lab(st, loc = "澎湖", s = 6) + 
  scale_color_manual(values = prog_color) +
  scale_fill_manual(values = prog_color) +
  ht

t <- plot_map(t_map) + 
  add_st(st, loc = "桃園") + 
  add_st_lab(st, loc = "桃園") + 
  scale_color_manual(values = prog_color) +
  scale_fill_manual(values = prog_color) +
  ht

k <- plot_map(k_map) + 
  add_st(st, loc = "墾丁") + 
  add_st_lab(st, loc = "墾丁") + 
  scale_color_manual(values = prog_color) +
  scale_fill_manual(values = prog_color) +
  ht

plot_save(p + n + plot_annotation(tag_levels = "a",
                                  tag_prefix = "(",
                                  tag_suffix = ")"), 
          "map_Penghu_North", scale = 1.7)
sp2 <- wrap_plots(t, l, k, ncol = 1)
plot_save(sp2 + plot_annotation(tag_levels = "a",
                                tag_prefix = "(",
                                tag_suffix = ")")
          , "map_TY_Liuqiu_East", scale = 1.7)

plot_save((p / n) - (t / l / k) - e + 
            plot_annotation(tag_levels = "a",
                            tag_prefix = "(",
                            tag_suffix = ")"),
          "map_whole", scale = 2.2)

plot_save(tw + theme(legend.text = element_text(family = msjh)), "map_tw", s = 1.6)

e_map_ggplot <- plot_map(e_map)
n_map_ggplot <- plot_map(n_map)
l_map_ggplot <- plot_map(l_map)
p_map_ggplot <- plot_map(p_map)
t_map_ggplot <- plot_map(t_map)
k_map_ggplot <- plot_map(k_map)

save(e_map_ggplot, n_map_ggplot, l_map_ggplot, p_map_ggplot, t_map_ggplot, k_map_ggplot, 
     file = "data/map_ggplot.RData")

