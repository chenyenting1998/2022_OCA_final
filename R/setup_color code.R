# location color code
loc_color <-
  c("澎湖" = "black",
    "北台灣" = "purple", 
    "桃園" = "darkgrey", 
    "小琉球" = "orange",
    "墾丁" = "red",
    "東台灣" = "green") 

# Progress color code
prog_color <-
  c("Analyzed" = "cyan", "Sampled" = "pink")

save(prog_color, loc_color,
     file = "data/loc_and_prog_color_code.Rdata")