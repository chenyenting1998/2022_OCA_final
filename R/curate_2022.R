library(dplyr)
library(writexl)
library(readxl)
library(GRSPRSThesisData)

# read and curate new files ----

## Tung, Chueh-Chen----
tcc1 <- 
  read_xlsx("xlsx/2022/2022_0608_chueh.xlsx",
            col_types = c(rep("guess", 11), rep("numeric", 2), rep("guess", 5))) %>% 
  mutate(Cruise = if_else(Cruise == "2021.小琉球",
                          "2021.Liuqiu",
                          Cruise)) 
tcc2 <- 
  read_xlsx("xlsx/2022/2022_1003_chueh.xlsx") %>% 
  mutate(Cruise = if_else(Cruise == "2021.小琉球",
                          "2021.Liuqiu",
                          Cruise)) 

tcc2[tcc2$Taxon == "Malacostraca",]$Note <- "Paguroidea"
tcc2[tcc2$Taxon == "Malacostraca",]$Taxon <- "Decapoda"

# Malacostraca at 萬里桐

## Chen, Yen-Ting----
cyt1 <- 
  read_xlsx("xlsx/2022/2022.06.16.CYT.xlsx") %>% 
  mutate(Condition = toupper(Condition)) 

cyt2 <- 
  read_xlsx("xlsx/2022/2022.08.24.CYT.xlsx") %>% 
  mutate(Condition = toupper(Condition)) %>% 
  mutate(L = 0.1 * L,
         W = 0.1 * W)

## Chen, Hsin----
ch1 <- 
  read_xlsx("xlsx/2022/2021.East___Liuqiu_macro_size_Hsin.xlsx")
unique(tj$Taxon)
# ch2 <- 

## Tang, Jing----
tj <- 
  read_xlsx("xlsx/2022/Sorting_湯淨_20221003.xlsx", sheet = 3) %>% 
  mutate(Cruise = if_else(Cruise == "2021.小琉球",
                          "2021.Liuqiu",
                          Cruise)) 


tj[tj$Taxon == "Nemetina", "Taxon"] <- "Nemertea"
tj[tj$Taxon == "Acarina", "Taxon"] <- "Acari"
tj[tj$Taxon == "Leptocardii", "Taxon"] <- "Cephalochordata"
tj[tj$Taxon == "Bivalve", "Taxon"] <- "Bivalvia"

mea <- 
  rbind(tcc1, 
      tcc2,
      cyt1,
      cyt2,
      ch1,
      tj)

# mea[,c("Taxon", "Condition", "Note")] %>% distinct() %>% arrange(Taxon)%>% View

# capitalize the first letter in colony
sessile <- c("Hydrozoa", "Bryozoa") # subset data
mea[mea$Taxon %in% sessile, "Note"] <-  
  Hmisc::capitalize(mea[mea$Taxon %in% sessile,]$Note)

# set all hydrozoans and bryozoans to "C"
mea[mea$Taxon %in% sessile, "Condition"] <- "C"

#
write_xlsx(mea, "data/2022_OCA_macrofauna_measurement.xlsx")
