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

# density ----

omit<- c("桃園", "墾丁")

## permdisp ----
dwd_p <- 
  dw[!dw$Location_zh %in% omit, -(1:2)] %>%
  decostand(method = "hellinger") %>% 
  dist()

den_disp <- 
  betadisper(dwd_p,
             dw[!dw$Location_zh %in% omit,]$Location_zh,
             type = 'median',
             sqrt.dist = FALSE,
             bias.adjust = TRUE)  
set.seed(1)
den_disp_perm <-
  permutest(den_disp, permutations = 9999)

## permanova ----
set.seed(7)
den_perm <-
  adonis2(dwd_p~Location_zh, dw[!dw$Location_zh %in% omit,], permutations = 9999)

## pairwise ----
set.seed(8)
pairwise_den_perm <- 
  pairwise.adonis2(dwd_p~Location_zh, 
                   dw[!dw$Location_zh %in% omit,], 
                   nperm = 9999)

# biomass ----
## permdisp ----
bwd_p <- 
  bw[!bw$Location_zh %in% omit, -(1:2)] %>%
  decostand(method = "hellinger")%>% 
  dist()

bio_disp <- 
  betadisper(bwd_p,
             bw[!bw$Location_zh %in% omit,]$Location_zh,
             type = 'median',
             bias.adjust = TRUE,
             sqrt.dist = FALSE)

set.seed(1)
bio_disp_perm <-
  permutest(bio_disp, permutations = 9999)

## permanova ----
set.seed(7)
bio_perm <-
  adonis2(bwd_p~Location_zh, bw[!bw$Location_zh %in% omit,], permutations = 9999)

## pairwise ----
set.seed(8)
pairwise_bio_perm <- 
  pairwise.adonis2(bwd_p~Location_zh, 
                   bw[!bw$Location_zh %in% omit,], 
                   nperm = 9999)


permdisp_table <- function(permdisp_object) cbind(rownames(as.data.frame(permdisp_object$tab)),as.data.frame(permdisp_object$tab))
permanova_table <- function(permanova_object) cbind(rownames(as.data.frame(permanova_object)), as.data.frame(permanova_object))


write_xlsx(list(density_permdisp = permdisp_table(den_disp_perm),
                density_permanova = permanova_table(den_perm),
                biomass_permdisp = permdisp_table(bio_disp_perm),
                biomass_permanova = permanova_table(bio_perm) ),
           path = "tab/hel_permutation_test.xlsx")

pairwise_bio_perm[[1]] <- NULL
pairwise_den_perm[[1]] <- NULL

pairwise_bio_table <- 
  lapply(pairwise_bio_perm, function(x) { 
    x$p.adjust <- p.adjust(x$`Pr(>F)`, method = "bonferroni", n = 6)
    name <- rownames(x)
    x <- cbind(name, x)
    return(x)})

pairwise_den_table <- 
  lapply(pairwise_den_perm, function(x) { 
    x$p.adjust <- p.adjust(x$`Pr(>F)`, method = "bonferroni", n = 6)
    name <- rownames(x)
    x <- cbind(name, x)
    return(x)})

write_xlsx(pairwise_den_table,
           path = "tab/hel_pairwise_permutation_den.xlsx")
write_xlsx(pairwise_bio_table,
           path = "tab/hel_pairwise_permutation_bio.xlsx")
