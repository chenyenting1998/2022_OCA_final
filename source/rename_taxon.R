rename_taxon <- function(data, old_name, revised_name) {
  data[data$Taxon == old_name, "Taxon"] <- revised_name
  data
}

