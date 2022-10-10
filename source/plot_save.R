plot_save <- function(object, name, scale = 1.5, h = 3.5, w = 8){
  L <- paste0("fig/", name, ".png")
  ggsave(filename = L,
         plot = object,
         scale = scale,
         height = h,
         width = w)
}