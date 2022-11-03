# plot combine
library(patchwork)
load("data/hel_cluster.RData")
load("data/hel_pca.RData")

# density
den1 <- (den_rda_ggplot+ theme(legend.position = "none")) / (den_rda_ggplot_species + theme(legend.position = "none"))
den2 <- den_dendro_ggplot


plot_save(den1 - den2+ 
            plot_annotation(tag_levels = "a",
                            tag_prefix = "(",
                            tag_suffix = ")"),
          "den_cluster_pca", h = 4, w = 5, scale = 2)

# biomass ----
bio1 <- (bio_rda_ggplot+ theme(legend.position = "none")) / (bio_rda_ggplot_species + theme(legend.position = "none"))
bio2 <- bio_dendro_ggplot


plot_save(bio1 - bio2 + 
            plot_annotation(tag_levels = "a",
                            tag_prefix = "(",
                            tag_suffix = ")"),
          "bio_cluster_pca", h = 4, w = 4.5, scale = 2)

# ppt ----
# bio1 <- bio_dendro_ggplot + ylim(c(3.1, -1)) + theme(legend.position = "none")
# bio2 <- bio_rda_ggplot + xlim(c(-0.75, 0.8)) + theme(legend.text = element_text(family = msjh, size = 12))
# plot_save(bio1 - bio2 + plot_layout(widths = c(1, 2)),
#           "bio_cluster_pca_ppt", h = 6, w = 10, scale = 1)
# 
# 
# p6 <- ggplot(mtcars) + geom_point(aes(mpg, disp, color=cyl))
# p7 <- ggplot(mtcars) + geom_point(aes(mpg, hp, color=cyl))
# p6 + p7 + plot_layout(guides='collect')
# 
# p6 + p7 + plot_layout(guides='collect') &
#   theme(legend.position='bottom')
