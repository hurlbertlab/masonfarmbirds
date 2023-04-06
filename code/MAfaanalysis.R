#############################################
###### MA Forest Acoustics Analsysis   ######
###### Distance & Frequency            ######
###### Last update: 4/6/2023           ######
#############################################

library(dplyr)
library(tidyverse)

analysis_table <- read.csv("data/analysis_table.csv")

analysis_table <- analysis_table %>%
  mutate(vol = case_when((species == "CW5" & vol == 75) ~ 100,
                         TRUE ~ as.numeric(as.character(vol))
                         ))

analysis <- analysis_table %>%
  filter(relative_dir == "N", vol == "100")

colors_spec = c("#9FD573","#79A356", "#4C6F2F", "#2B4416", "#0B2308")

# Create DFs for each species

CW_analysis <- analysis %>% 
  filter(species == "CW5")
CWdistMod = lm(log(relative.amp) ~ distance_m, data = CW_analysis)

AF_analysis <- analysis %>% 
  filter(species == "AF")
AFdistMod = lm(log(relative.amp) ~ distance_m, data = AF_analysis)

BG_analysis <- analysis %>% 
  filter(species == "BG")
BGdistMod = lm(log(relative.amp) ~ distance_m, data = BG_analysis)

EWP_analysis <- analysis %>% 
  filter(species == "EWP")
EWPdistMod = lm(log(relative.amp) ~ distance_m, data = EWP_analysis)

YBC_analysis <- analysis %>% 
  filter(species == "YBC")
YBCdistMod = lm(log(relative.amp) ~ distance_m, data = YBC_analysis)



plot(analysis$distance_m, log(analysis$relative.amp), col=analysis_table$col_spec, pch = 16, cex = 2,
     ylab = "log Relative amplitude", xlab = "Distance (m)")
abline(BGdistMod, lwd = 5, col = analysis_table$col_spec[1])
abline(AFdistMod, lwd = 5, col = analysis_table$col_spec[2])
abline(EWPdistMod, lwd = 5, col = analysis_table$col_spec[3])
abline(CWdistMod, lwd = 5, col = analysis_table$col_spec[4])
abline(YBCdistMod, lwd = 5, col = analysis_table$col_spec[5])
legend("topright", legend = c("BG", "AF", "EWP", "CW", "YBC"), pch = 16, cex = 2, col = colors_spec[1:5])

