library(dplyr)
library(writexl)
library(readxl)
library(GRSPRSThesisData)

# read data
mea <- 
  rbind(read_xlsx("data/2021_OCA_macrofauna_measurement.xlsx"),
        read_xlsx("data/2022_OCA_macrofauna_measurement.xlsx")) %>% 
  select(-Habitat, -Deployment, -Family, -Genus, -a, -b, -C)

# checking typo
mea[mea$Taxon  %in% c("Gastrapoda", "Oligochaetea"),]

# remove year
mea$Cruise <- gsub(".*\\.", "", mea$Cruise)

# add kenting
mea$Cruise[mea$Cruise == "South"] <- "Kenting"

# change column name "cruise" to "Location"
mea <- rename(mea, "Location" = "Cruise")

# attach the chinese location name
loc <-
  c("North" = "北台灣",
    "East" = "東台灣",
    "Penghu" = "澎湖",
    "Liuqiu" = "小琉球",
    "Kenting" = "墾丁",
    "Taoyuan" = "桃園")
mea$Location_zh <- loc[match(mea$Location, names(loc))]
mea <- relocate(mea, Location_zh, .after = "Location")

# check typo again
taxlist <- mea["Taxon"] %>% distinct

# get biovolume method
oca_2022_biovolume <- 
  read_xlsx("xlsx/biovolume_add_on.xlsx") %>% 
  full_join(biovolume_method)

# calculating biovolume and wet mass (using self-defined functions)
grouping_variables <- colnames(mea)[1:5]
  
size <- 
  mea %>% 
  assign_method(method_file = oca_2022_biovolume) %>% 
  calculate_biovolume() %>% 
  define_ophiuroid_size(protocol_ophiuroid = "all_arms",
                        grouping_variables = grouping_variables) %>% 
  mutate(WM = Size * 1.13,
         Type = Method) %>% 
  select(-Method)

# add description
descr <- 
  data.frame(
    Variables = c("Tube","Section", "Condition", "Type", "L", "W", "C", "Size", "WM"),
    Description = c("tube number" ,"Core section (cm)", "Body condition (C = Complete; FH = fragmented with head intact; FT = fragmented with tail intact; F = fragmented)", "Biovolume calculation method (LWR = Length-width relationship')", "length (mm)", "Width (mm)", "Conversion factor", "mm^3", "mg wet weight"))

write_xlsx(list(Size = size, Description = descr), 
           path = "data/2022_OCA_final_macrofauna_size.xlsx")
