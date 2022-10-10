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
load("data/den_bio_wide.RData")

# constants
gc_area <- (6.6 / 2 * 0.01) ^2 * pi # gravity core area
sw <- 1.13 # specific weight

# load data
size <- read_xlsx("data/2022_OCA_final_macrofauna_size.xlsx")

# density pca ----
all_den_rda <-
  dw[-(1:2)] %>%
  decostand(method = "hellinger") %>% 
  rda()

den_rda_pr <- round((all_den_rda$CA$eig / all_den_rda$tot.chi) * 100, 2)

barplot(all_den_rda$CA$eig)

all_den_sites <- scores(all_den_rda, scales = 1)$sites %>% as.data.frame %>% cbind(dw[1:2])
all_den_species <- scores(all_den_rda, scales = 1)$species %>% as.data.frame()
all_den_species$RI <- (all_den_species$PC1^2 + all_den_species$PC2^2)^0.5

den_rda_ggplot <-
  ggplot()+
  geom_point(data = all_den_sites, 
             aes(x = PC1, y = PC2, color = Location_zh), size = 2) +
  geom_text_repel(data = all_den_sites, 
                  aes(x = PC1, 
                      y = PC2, 
                      family = msjh,
                      color = Location_zh, 
                      label = Station), 
                  seed = 1, size = 2.5,
                  max.overlaps = 22,
                  force = 20)+
  guides(color = guide_legend(title = "Location"))+
  xlab(paste0("PC1 (", den_rda_pr[1], "% explained)"))+
  ylab(paste0("PC2 (", den_rda_pr[2], "% explained"))+
  scale_color_manual(values = loc_color)+
  scale_x_continuous(limits = c(-0.85, 0.7)) +
  theme_bw() +
  coord_fixed()

den_rda_ggplot_species <-
  ggplot()+
  geom_point(data = all_den_sites, 
             aes(x = PC1, y = PC2, color = Location_zh), size = 2) +
  geom_text(data = all_den_species,
            aes(x = PC1,
                y = PC2,
                family = msjh,
                label = rownames(all_den_species),
                alpha = RI))+
  guides(color = guide_legend(title = "Location"))+
  xlab(paste0("PC1 (", den_rda_pr[1], "% explained)"))+
  ylab(paste0("PC2 (", den_rda_pr[2], "% explained"))+
  scale_color_manual(values = loc_color)+
  scale_x_continuous(limits = c(-0.85, 0.7)) +
  theme_bw() +
  coord_fixed()

# biomass pca ----

# density pca ----
all_bio_rda <-
  bw[-(1:2)] %>%
  decostand(method = "hellinger") %>% 
  rda()

bio_rda_pr <- round((all_bio_rda$CA$eig / all_bio_rda$tot.chi) * 100, 2)

barplot(all_bio_rda$CA$eig)

all_bio_sites <- scores(all_bio_rda, scales = 1)$sites %>% as.data.frame %>% cbind(bw[1:2])
all_bio_species <- scores(all_bio_rda, scales = 1)$species %>% as.data.frame()
all_bio_species$RI <- (all_bio_species$PC1^2 + all_bio_species$PC2^2)^0.5

bio_rda_ggplot <-
  ggplot(all_bio_sites, (aes(x = PC1, y = PC2, color = Location_zh)))+
  geom_point(size = 2) +
  geom_text_repel(aes(family = msjh,
                      label = Station), 
                  seed = 1, size = 2.5, force = 20, max.overlaps = 50)+
  guides(color = guide_legend(title = "Location"))+
  xlab(paste0("PC1 (", bio_rda_pr[1], "% explained)"))+
  ylab(paste0("PC2 (", bio_rda_pr[2], "% explained"))+
  scale_color_manual(values = loc_color)+
  scale_x_continuous(limits = c(-1, 0.75)) +
  theme_bw() +
  coord_fixed()

bio_rda_ggplot_species <-
  ggplot(all_bio_sites, (aes(x = PC1, y = PC2, color = Location_zh)))+
  geom_point(size = 2) +
  geom_text(data = all_bio_species,
            aes(x = PC1, 
                y = PC2, 
                family = msjh,
                label = rownames(all_bio_species),
                alpha = RI),
            color = "black")+
  guides(color = guide_legend(title = "Location"))+
  xlab(paste0("PC1 (", bio_rda_pr[1], "% explained)"))+
  ylab(paste0("PC2 (", bio_rda_pr[2], "% explained"))+
  scale_x_continuous(limits = c(-1, 0.75)) +
  scale_color_manual(values = loc_color)+
  theme_bw() +
  coord_fixed()

# save
all_rda <- 
  den_rda_ggplot + theme(legend.position = "none") +
  bio_rda_ggplot + 
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")")
plot_save(all_rda, "hel_pca", w = 8, h =5)

save(all_den_sites, all_den_species, den_rda_ggplot, den_rda_ggplot_species,
     all_bio_sites, all_bio_species, bio_rda_ggplot, bio_rda_ggplot_species,
     file = "data/hel_pca.RData")
