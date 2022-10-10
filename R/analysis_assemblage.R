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
load("data/standing_stock.Rdata")

# constants
gc_area <- (6.6 / 2 * 0.01) ^2 * pi # gravity core area
sw <- 1.13 # specific weight
ss_name <- 
  c("Density" = "Density~(ind.~m^-2)",
    "Biomass" = "Biomass~(g~wet~weight~m^-2)")
fct_loc <- c("澎湖", "北台灣", "桃園", "小琉球","東台灣","墾丁") 

# read
size <- read_xlsx("data/2022_OCA_final_macrofauna_size.xlsx")

# Density composition ------
comp <- # standing stock
  size %>% 
  filter(Condition %in% c("C", "FH")) %>% 
  filter(!Taxon %in% c("Unknown")) %>% 
  group_by(Location_zh, Station, Taxon) %>% 
  summarize(Density = n()/gc_area, 
            Biomass = sum(Size * sw) / gc_area / 1000) %>% 
  pivot_longer(cols = c("Density", "Biomass"),
               names_to = "Variable",
               values_to = "Value") 

comp$Taxa_den <- rank_den$Taxa[match(comp$Taxon, rank_den$Taxon)]
comp$Taxa_bio <- rank_bio$Taxa[match(comp$Taxon, rank_bio$Taxon)]

den_comp_ind_ggplot <- 
  comp[comp$Variable == "Density",] %>% 
  mutate(Location = factor(Location_zh, fct_loc)) %>% 
  ggplot(aes(x = Station, y = Value, fill = Taxa_den))+
  geom_bar(stat = "identity")+
  facet_grid(~factor(Location_zh, fct_loc), scales = "free_x", space = "free")+
  scale_fill_manual(values = taxa_den_color)+
  scale_y_continuous(expand = c(0,0.1), 
                     limits = c(0,max(ss[ss$Variable == "Density",]$Value) * 1.05))+
  xlab("Station")+
  ylab(Density~(ind.~m^-2))+
  guides(fill = guide_legend(title = "Taxa"))+
  theme_bw()+
  theme(axis.text.x = element_text(family = msjh, 
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 0.5),
        strip.text = element_text(family = msjh))

plot_save(den_comp_ind_ggplot, "composition_density_ind.")

den_comp_ggplot_percent <- 
  comp[comp$Variable == "Density",] %>% 
  mutate(Location = factor(Location_zh, fct_loc)) %>% 
  ggplot(aes(x = Station, y = Value, fill = Taxa_den))+
  geom_bar(stat = "identity", position = "fill")+
  facet_grid(~factor(Location_zh, fct_loc), scales = "free_x", space = "free")+
  scale_fill_manual(values = taxa_den_color)+
  scale_y_continuous(expand = c(0,0),
                     label = scales::percent) +
  xlab("Station")+
  ylab("Density (%)")+
  guides(fill = guide_legend(title = "Taxa"))+
  theme_bw()+
  theme(axis.text.x = element_text(family = msjh, 
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 0.5),
        strip.text = element_text(family = msjh))

plot_save(den_comp_ggplot_percent, "composition_density_percent")

# Biomass composition ------
bio_comp_ggplot <- 
  comp[comp$Variable == "Biomass",] %>% 
  mutate(Location = factor(Location_zh, fct_loc)) %>% 
  ggplot(aes(x = Station, y = Value, fill = Taxa_bio))+
  geom_bar(stat = "identity")+
  facet_grid(~factor(Location_zh, fct_loc), scales = "free_x", space = "free")+
  scale_fill_manual(values = taxa_bio_color)+
  scale_y_continuous(expand = c(0,0.1), 
                     limits = c(0,max(ss[ss$Variable == "Biomass",]$Value) * 1.05))+
  xlab("Station")+
  ylab(Biomass~(g~wet~weight~m^-2))+
  theme_bw()+
  guides(fill = guide_legend(title = "Taxa"))+
  theme(axis.text.x = element_text(family = msjh, 
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 0.5),
        strip.text = element_text(family = msjh))

plot_save(bio_comp_ggplot, "composition_biomass_g")


bio_comp_percent_ggplot <- 
  comp[comp$Variable == "Biomass",] %>% 
  mutate(Location = factor(Location_zh, fct_loc)) %>% 
  ggplot(aes(x = Station, y = Value, fill = Taxa_bio))+
  geom_bar(stat = "identity", position = "fill")+
  facet_grid(~factor(Location_zh, fct_loc), scales = "free_x", space = "free")+
  scale_fill_manual(values = taxa_bio_color)+
  scale_y_continuous(expand = c(0, 0),
                     label = scales::percent) +
  xlab("Station")+
  ylab("Biomass (%)")+
  theme_bw()+
  guides(fill = guide_legend(title = "Taxa"))+
  theme(axis.text.x = element_text(family = msjh, 
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 0.5),
        strip.text = element_text(family = msjh))

plot_save(bio_comp_percent_ggplot, "composition_biomass_percent")

# Phylum rank ----
phylum_rank <- 
  c("Annelida", "Arthopoda", "Echinodermata", "Mollusca", "Cnidaria",
    "Nematoda", "Nemertea", "Entoprocta", "Bryozoa", "Phoronida",
    "Chaetognatha", "Hemichordata", "Chordata", "Platyhelminthes",
    "Tardigrada","Unknown")

add._taxa_label <- 
  data.frame(Taxon = c("Sipuncula", "Polyplacophora", "Unknown", "Ascidiacea", "Cirripedia", "Hirudinea", "Nudibranchia"),
             Phylum = c("Annelida", "Mollusca", "Unknown", "Chordata", "Arthopoda", "Annelida", "Mollusca"),
             Group = c("Annelida", "Mollusca", "Unknown", "Other", "Crustacea", "Annelida", "Mollusca"))
coarse_taxa_add <- 
  rbind(coarse_taxa[coarse_taxa$Taxon != "Sipuncula",],
        add._taxa_label)

table <- function(v = "Density", location = "Penghu"){
  x <-
    comp %>%
    add_coarse_taxon(coarse_taxa_add, output = "Phylum") %>%
    ungroup %>%
    filter(Location_zh %in% location) %>%
    filter(Variable %in% v) %>%
    select(Phylum, Station, Taxon, Value) %>%
    pivot_wider(names_from = "Station",
                values_from = "Value",
                values_fill = NA_integer_) %>%
    group_by(Phylum, Taxon) %>%
    slice(match(phylum_rank, Phylum))
  
  s <- strsplit(v, "")[[1]]
  s[1] <- tolower(s[1])
  V <- paste0(s, collapse = "")
  
  a <- rbind(x[1:2], 
             data.frame(Phylum = paste0("Total ", V), Taxon = ""))
  
  sum_narm <- function(x) sum(x, na.rm = TRUE)
  b <- rbind(x[-(1:2)], 
             apply(x[-(1:2)], 2, sum_narm))
  cbind(a,b)
}

#density table ----
den_table_ph <- table("Density", "澎湖")
den_table_n <- table("Density", "北台灣")

den_table_tle <- 
  full_join(table("Density", "桃園"), 
            table("Density", "小琉球")) %>%
  full_join(table("Density", "東台灣")) %>% 
  full_join(table("Density", "墾丁")) %>% 
  slice(match(c(phylum_rank, "Total density"), c(Phylum, "Total density")))

# biomass talbe ----
bio_table_ph <- table("Biomass", "澎湖")
bio_table_n <- table("Biomass", "北台灣")
bio_table_tle <- 
  full_join(table("Biomass", "桃園"), 
            table("Biomass", "小琉球")) %>%
  full_join(table("Biomass", "東台灣")) %>% 
  full_join(table("Biomass", "墾丁")) %>% 
  slice(match(c(phylum_rank, "Total biomass"), c(Phylum, "Total biomass")))

write_xlsx(list(Density_Penghu = den_table_ph, 
                Density_North = den_table_n, 
                Density_Taoyuan_Liuqiu_East = den_table_tle,
                Biomass_Penghu = bio_table_ph,
                Biomass_North = bio_table_n,
                Biomass_Taoyuan_Liuqiu_East = bio_table_tle), 
           "tab/2022_OCA_midterm_density_biomass_table.xlsx")
