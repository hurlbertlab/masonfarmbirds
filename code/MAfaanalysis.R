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

colors_spec = c("#BC4749","#79A356", "#FFB703", "#2A9D8F", "#283618")

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



plot(analysis$distance_m, log(analysis$relative.amp), col="white",lty="dotted", pch = 16, cex = 1.7,
     ylab = "log Relative Amplitude", xlab = "Distance (m)", xaxp = c(0, 100, 4) )
abline(BGdistMod, lwd = 4, col = colors_spec[1])
abline(AFdistMod, lwd = 4, col = colors_spec[2])
abline(EWPdistMod, lwd = 4, col = colors_spec[3])
abline(CWdistMod, lwd = 4, col = colors_spec[4])
abline(YBCdistMod, lwd = 4, col = colors_spec[5])
legend("topright", legend = c("BG: 6.88 kHz", "AF: 5.27 kHz", "EWP: 4.21 kHz", "CW: 3.00 kHz", "YBC: 1.70 kHz"), pch = 16, cex = 2, col = colors_spec[1:5])

species <- c("BG", "AF", "EWP", "CW", "YBC")
frequencies <- c(6.88, 5.27, 4.21, 3.00, 1.70)
slope <- c(-0.035, -0.029, -0.028, -0.028, -0.023)

freqvslope <- data.frame(species, frequencies, slope)

freqvslopegraph = lm(slope ~ frequencies, data=freqvslope)
plot(freqvslope$frequencies, freqvslope$slope, col=c("#BC4749", "#79A356", "#FFB703","#2A9D8F", "#283618"), lty="dotted", pch = 16, cex = 2.5,
     ylab = "Slope of Line on log Relative Amp vs Dist Graph", xlab = "Frequency (kHz)" )
abline(freqvslopegraph, lwd = 4, col = "black") 
legend("topright", legend = c("BG", "AF", "EWP", "CW", "YBC"), pch = 16, cex = 2.5, col = colors_spec[1:5])
