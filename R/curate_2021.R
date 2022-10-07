library(dplyr)
library(writexl)
library(readxl)
library(GRSPRSThesisData)

# read and curate new files ----

## Tung, Chueh-Chen----
tcc1 <- 
  read_xlsx("xlsx/2021_final/2021.Penghu_macro_size_Chueh_20210701.xlsx")

ttc2 <- 
  read_xlsx("xlsx/2021_final/1104_chueh.xlsx",
            col_types = c(rep("guess", 11), rep("numeric", 2), rep("guess", 5)))

## Chen, Yen-Ting----
cyt1 <- 
  read_xlsx("xlsx/2021_final/OCA_final_macro_size_CYT.xlsx") %>% 
  mutate(Condition = toupper(Condition)) %>% 
  mutate(L = L*0.1) %>% 
  mutate(W = W*0.1)

cyt2 <- 
  read_xlsx("xlsx/2021_final/2021.Penghu_macro_size_CYT.xlsx")

## Chen, Hsin----
ch1 <- 
  read_xlsx("xlsx/2021_final/2021.North_macro_size_Hsin.xlsx")

ch2 <- 
  read_xlsx("xlsx/2021_final/2021.Penghu_macro_size_Hsin.xlsx")

## Tang, Jing----
tj <- 
  read_xlsx("xlsx/2021_final/Sorting_湯淨.xlsx", sheet = 3)

tj[tj$Taxon == "Nemertina", "Taxon"] <- "Nemertea"
tj[tj$Taxon == "Acarina", "Taxon"] <- "Acari"
tj[tj$Taxon == "Turbellaria", "Taxon"] <- "Platyhelminthes"

# combine data
mea <- rbind(tcc1, 
             ttc2, 
             cyt1, 
             cyt2, 
             ch1, 
             ch2, 
             tj)

# capitalize the first letter in colony
sessile <- c("Hydrozoa", "Bryozoa") # subset data
mea[mea$Taxon %in% sessile, "Note"] <-  
  Hmisc::capitalize(mea[mea$Taxon %in% sessile,]$Note)

# set all hydrozoans and bryozoans to "C"
mea[mea$Taxon %in% sessile, "Condition"] <- "C"

# rename megalopa to decapoda
mea[mea$Taxon == "Megalopa", "Note"] <- "Megalopa" 
mea[mea$Taxon == "Megalopa", "Taxon"] <- "Decapoda"

# rename stations
mea[mea$Station =="東吉", "Station"] <- "東吉嶼"
mea[mea$Station =="西吉", "Station"] <- "西吉嶼"
mea[mea$Station =="鐵砧(北)", "Station"] <- "北鐵砧"
mea[mea$Station =="鐵砧嶼", "Station"] <- "南鐵砧"
mea[mea$Station == "青灣", "Station"] <- "青灣內灣"

write_xlsx(mea, "data/2021_OCA_macrofauna_measurement.xlsx")
