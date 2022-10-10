# library
library(dplyr)
library(readxl)
library(writexl)
library(tidyr)
library(vegan)
library(ggplot2)
library(ggrepel)
library(ggdendro)
library(patchwork)
library(pairwiseAdonis)
library(cluster)
library(Polychrome)
library(extrafont)

# source
source("source/msjh.R")
source("source/plot_save.R")

# load
load("data/loc_and_prog_color_code.RData")

# constants
gc_area <- (6.6 / 2 * 0.01) ^2 * pi # gravity core area
sw <- 1.13 # specific weight

# load data
size <- read_xlsx("data/2022_OCA_final_macrofauna_size.xlsx")

# density wide ----
dw <-
  size %>% 
  filter(Condition %in% c("C", "FH")) %>% 
  filter(!Taxon %in% c("Unknown")) %>% 
  group_by(Location_zh, Station, Taxon) %>% 
  summarize(Density = n()) %>%  
  pivot_wider(names_from = Taxon, values_from = Density, values_fill = 0)

#den wide dist
dwd <- dw[-(1:2)] %>% decostand(method = "hellinger") %>% dist()

# hierarchical clustering ----
dwc <- hclust(dwd, method = "ward.D2")
dwc$labels <- dw$Station

# silhouette plot for k----
asw <- numeric(nrow(dw))

for(k in 2:(nrow(dw) - 1)){
  sil <- silhouette(cutree(dwc, k = k), dwd)
  asw[k] <- summary(sil)$avg.width
}
which.max(asw)
order(asw, decreasing = T)
plot(x = c(1:nrow(dw)), y = asw)

# plot dendro----
# extract seg
all_den_seg <- dendro_data(dwc) %>% segment()

# extract lab
all_den_lab <- dendro_data(dwc) %>% label() 
colnames(all_den_lab)[3] <- "Station"
all_den_lab <- all_den_lab %>% left_join(dw[1:2], by = "Station")

# plot
den_dendro_ggplot <-
  ggplot() +
  geom_segment(data = all_den_seg, 
               aes(x = x, y = y, xend = xend, yend = yend))+
  geom_point(data = all_den_lab, 
             aes(x = x, y = y, color = Location_zh), size = 2) +
  geom_text(data = all_den_lab, 
            aes(x = x, y = y + 0.12, label = Station),
            size = 3,
            hjust = 0.01,
            family = msjh,
            nudge_y = 0.15,
            show.legend = F) +
  scale_color_manual(values = loc_color) +
  coord_flip() + # flipping coordinates (1)
  scale_y_reverse(expand=c(0.2, 0)) + # (2)
  ylab("Height") +
  guides(color = guide_legend(title = "Location"))+
  theme(axis.title.y = element_blank(), # x axis
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(), # back panel
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(), # remove border
        axis.line.x.bottom = element_line(colour = "black")) # add back y border

# biomass wide ----
bw <-
  size %>% 
  filter(Condition %in% c("C", "FH")) %>% 
  filter(!Taxon %in% c("Unknown")) %>% 
  # mutate(Location = gsub("2021.", "", Location)) %>% 
  group_by(Location_zh, Station, Taxon) %>% 
  summarize(Biomass = sum(WM)*0.001) %>%  
  pivot_wider(names_from = Taxon, values_from = Biomass, values_fill = 0)

## bio wide dist----
bwd <- bw[-(1:2)] %>% decostand(method = "hellinger") %>% dist()

## hierarchical clustering ----
bwc <- hclust(bwd, method = "ward.D2")
bwc$labels <- bw$Station

## silhouette plot for k----
asw <- numeric(nrow(bw))

for(k in 2:(nrow(bw) - 1)){
  sil <- silhouette(cutree(bwc, k = k), bwd)
  asw[k] <- summary(sil)$avg.width
}
which.max(asw)
order(asw, decreasing = T)
plot(x = c(1:nrow(bw)), y = asw)


## plot dendro----
# extract seg
all_bio_seg <- dendro_data(bwc) %>% segment()

# extract lab
all_bio_lab <- dendro_data(bwc) %>% label() 
colnames(all_bio_lab)[3] <- "Station"
all_bio_lab <- all_bio_lab %>% left_join(dw[1:2], by = "Station")

# plot
bio_dendro_ggplot <-
  ggplot() +
  geom_segment(data = all_bio_seg, 
               aes(x = x, y = y, xend = xend, yend = yend))+
  geom_point(data = all_bio_lab, 
             aes(x = x, y = y, color = Location_zh), size = 2) +
  geom_text(data = all_bio_lab, 
            aes(x = x, y = y + 0.12, label = Station),
            size = 3,
            hjust = 0.01,
            family = msjh,
            nudge_y = 0.15,
            show.legend = F) +
  scale_color_manual(values = loc_color) +
  coord_flip() + # flipping coordinates (1)
  scale_y_reverse(expand=c(0.2, 0)) + # (2)
  ylab("Height") +
  guides(color = guide_legend(title = "Location"))+
  theme(axis.title.y = element_blank(), # x axis
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(), # back panel
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(), # remove border
        axis.line.x.bottom = element_line(colour = "black")) # add back y border

cluster_plot <- 
  den_dendro_ggplot + theme(legend.position = "none")+ 
  bio_dendro_ggplot +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")")

plot_save(cluster_plot, "hel_cluster", h = 6, w = 8, scale = 1.7)


save(all_den_seg, all_den_lab, den_dendro_ggplot,
     all_bio_seg, all_bio_lab, bio_dendro_ggplot,
     file = "data/hel_cluster.RData")

#
# save(bw, dw, file = "data/den_bio_wide.RData")
