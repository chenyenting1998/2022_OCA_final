check_taxa <- function(data){ 
  unique(data$Taxon)[order(unique(data$Taxon))]
}
