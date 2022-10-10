# library
library(dplyr)
library(tidyr)
library(ggplot2)
library(Polychrome)
library(ggrepel)
library(extrafont)
library(readxl)
library(writexl)
library(FSA)
library(GRSPRSThesisData)

# source
source("source/msjh.R")
source("source/plot_save.R")

# 
load("data/taxa_color.Rdata")
load("data/taxa_rank.Rdata")
load("data/loc_and_prog_color_code.Rdata")

# constants
gc_area <- (6.6 / 2 * 0.01) ^2 * pi # gravity core area
sw <- 1.13 # specific weight
ss_name <- 
  c("Density" = "Density~(ind.~m^-2)",
    "Biomass" = "Biomass~(g~wet~weight~m^-2)")
fct_loc <- c("澎湖", "北台灣", "桃園", "小琉球", "墾丁", "東台灣") 

size <- read_xlsx("data/2022_OCA_final_macrofauna_size.xlsx")

# standing stock
ss <- 
  size %>% 
  filter(Condition %in% c("C", "FH")) %>% 
  filter(!Taxon %in% c("Unknown")) %>% 
  group_by(Location_zh, Station) %>% 
  summarize(Density = n()/gc_area, 
            Biomass = sum(Size * sw) / gc_area / 1000) %>% 
  pivot_longer(cols = c("Density", "Biomass"),
               names_to = "Variable",
               values_to = "Value")

set.seed(1)
boxplot_ggplot <- 
  ss %>% 
  mutate(Location = factor(Location_zh, fct_loc)) %>% 
  ggplot(aes(x = Location_zh, y = Value, color = Location_zh))+
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = "jitter")+
  facet_wrap(~Variable, 
             scales = "free_y",
             labeller = as_labeller(ss_name, label_parsed))+
  guides(color = guide_legend(title = "Location"))+
  scale_color_manual(values = loc_color)+
  theme_bw()+
  theme(axis.text.x = element_text(family = msjh, 
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 0.5))

plot_save(boxplot_ggplot, "standing_stock_boxplot", scale = 1)

save(ss, file = "data/standing_stock.Rdata")

# kruskal wallis 
ss_pnles <- ss[!ss$Location_zh %in% c("桃園", "墾丁"),]

kw_density <- 
  kruskal.test(Value~Location_zh, 
               data = ss_pnles[ss_pnles$Variable == "Density",])

kw_density <- 
  data.frame("chi-squared" = kw_density$statistic,
             "df" = kw_density$parameter,
             "p.value" = kw_density$p.value,
             row.names = "Density")
d_density <-
  dunnTest(Value~Location_zh, 
           data = ss_pnles[ss_pnles$Variable == "Density",],
           method = "bonferroni")

kw_biomass <- 
  kruskal.test(Value~Location_zh, 
               data = ss_pnles[ss_pnles$Variable == "Biomass",])

kw_biomass <- 
  data.frame("chi-squared" = kw_biomass$statistic,
             "df" = kw_biomass$parameter,
             "p.value" = kw_biomass$p.value,
             row.names = "Biomass")
d_biomass <-
  dunnTest(Value~Location_zh,
           data = ss_pnles[ss_pnles$Variable == "Biomass",],
           method = "bonferroni")

write_xlsx(list(KW_density = kw_density,
                Dunn_density = as.data.frame(d_density$res),
                KW_biomass = kw_biomass,
                Dunn_biomass = as.data.frame(d_biomass$res)),
           path = "tab/nonparm_test_standing_stock.xlsx")