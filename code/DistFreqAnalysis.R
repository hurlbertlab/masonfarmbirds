#############################################
###### Distance Frequency Analysis     ######
###### Last update: 2/13/2024          ######
#############################################

library(dplyr)
library(tidyverse)
library(ggplot2)

analysis_table <- read.csv("data/analysis_table_03012023_noaway.csv")

analysis_table <- analysis_table %>%
  mutate(vol = case_when((species == "CW5" & vol == 75) ~ 100,
                         TRUE ~ as.numeric(as.character(vol))
                         ))

analysis <- analysis_table %>%
  filter(relative_dir == "N", vol == "100")

analysis$relative.amp <- as.numeric(analysis$relative.amp)
analysis$distance_m <- as.numeric(analysis$distance_m)

# BG AF EWP CW YBC
colors_spec = c("#4488D2","#FF7700", "#FD569C", "#0ACE79", "#F0B924")

# Create DFs for each species

CW_analysis <- analysis %>% 
  filter(species == "CW5")

CWdistMod = lm(log10(relative.amp) ~ distance_m, data = CW_analysis)

AF_analysis <- analysis %>% 
  filter(species == "AF")
AFdistMod = lm(log10(relative.amp) ~ distance_m, data = AF_analysis)

BG_analysis <- analysis %>% 
  filter(species == "BG")
BGdistMod = lm(log10(relative.amp) ~ distance_m, data = BG_analysis)

EWP_analysis <- analysis %>% 
  filter(species == "EWP")
EWPdistMod = lm(log10(relative.amp) ~ distance_m, data = EWP_analysis)

YBC_analysis <- analysis %>% 
  filter(species == "YBC")
YBCdistMod = lm(log10(relative.amp) ~ distance_m, data = YBC_analysis)

plot(analysis$distance_m, log10(analysis$relative.amp), col="white",lty="dotted", pch = 16, cex = 1.7,
     ylab = "log10 Relative Amplitude", xlab = "Distance (m)", xaxp = c(0, 100, 4), ylim = c(-2.3, -0.7), cex.lab = 1.5, main = "Relative Amplitude vs Distance for Different Frequency Bird Vocalizations", cex.main = 1.8, cex.axis = 1.3)
abline(BGdistMod, lwd = 4, col = colors_spec[1])
abline(AFdistMod, lwd = 4, col = colors_spec[2])
abline(EWPdistMod, lwd = 4, col = colors_spec[3])
abline(CWdistMod, lwd = 4, col = colors_spec[4])
abline(YBCdistMod, lwd = 4, col = colors_spec[5])
legend("topright", legend = c("BG: 6.88 kHz, p-val = 0.000106, R2 = 0.9309", "AF: 5.27 kHz, p-val = 0.0052, R2 = 0.8173", "EWP: 4.21 kHz, p-val = 0.00168, R2 = 0.829", "CW: 3.00 kHz, p-val = 0.002, R2 = 0.8189", "YBC: 1.70 kHz, p-val = 0.000303, R2 = 0.9022"), pch = 16, cex = 1.5, col = colors_spec[1:5])

species <- c("BG", "AF", "EWP", "CW", "YBC")
frequencies <- c(6.88, 5.27, 4.21, 3.00, 1.70)
slope <- c(-0.035, -0.029, -0.028, -0.028, -0.023)

freqvslope <- data.frame(species, frequencies, slope)

par(mfrow=c(1,1), mar = c(5, 7, 5, 1))

freqvslopegraph = lm(slope ~ frequencies, data=freqvslope)
plot(freqvslope$frequencies, freqvslope$slope, col=c("#4488D2","#FF7700", "#FD569C", "#0ACE79", "#F0B924"), lty="dotted", pch = 16, cex = 2.5,
      xlab = "", ylab = "", cex.lab = 1.3, cex.axis = 1.2, main = "Slope of Regression for Relative Amplitude vs Distance against Frequency of Bird Vocalization", cex.main = 1.7)
abline(freqvslopegraph, lwd = 4, col = "black") 
title(ylab = "Slope of Relative Amplitude vs Distance Regression", line = 4.3, cex.lab = 1.3)
title(xlab = "Frequency (kHz)", line = 3, cex.lab = 1.3)
legend(6.421, -0.02254, legend = c("BG", "AF", "EWP", "CW", "YBC"), pch = 16, cex = 2, col = colors_spec[1:5])
legend(5.485, -0.02254, legend = c("p-val = 0.1613", "R2 = 0.8895"), cex= 1.5)

plot(log10(analysis$relative.amp) ~ analysis$distance_m, data = analysis)


plot(analysis$distance_m, log10(analysis$relative.amp), col=analysis_table$col_spec, pch = 16, cex = 2,
     ylab = "log Relative amplitude", xlab = "Distance (m)")
abline(CWdistMod, lwd = 5, col = analysis_table$col_spec[4])
legend("topright", legend = c("BG", "AF", "EWP", "CW5", "YBC"), pch = 16, cex = 2, col = colors_spec[1:5])

#### Raw data points graphs
par(mfrow=c(3,2))

#BG
plot(BG_analysis$distance_m, log10(BG_analysis$relative.amp), col= colors_spec[1],lty="dotted", pch = 16, cex = 1.7,
     ylab = "log Relative Amplitude", xlab = "Distance (m)", xaxp = c(0, 100, 4) )
abline(BGdistMod, lwd = 4, col = colors_spec[1])
legend("topright", legend = c("BG: 6.88 kHz"), pch = 16, cex = 1, col = colors_spec[1])

#AF

plot(AF_analysis$distance_m, log10(AF_analysis$relative.amp), col= colors_spec[2],lty="dotted", pch = 16, cex = 1.7,
     ylab = "log Relative Amplitude", xlab = "Distance (m)", xaxp = c(0, 100, 4) )
abline(AFdistMod, lwd = 4, col = colors_spec[2])
legend("topright", legend = c("AF: 5.27 kHz"), pch = 16, cex = 1, col = colors_spec[2])

#EWP

plot(EWP_analysis$distance_m, log10(EWP_analysis$relative.amp), col= colors_spec[3],lty="dotted", pch = 16, cex = 1.7,
     ylab = "log Relative Amplitude", xlab = "Distance (m)", xaxp = c(0, 100, 4) )
abline(EWPdistMod, lwd = 4, col = colors_spec[3])
legend("topright", legend = c("EWP: 4.21 kHz"), pch = 16, cex = 1, col = colors_spec[3])


#CW5

plot(CW_analysis$distance_m, log10(CW_analysis$relative.amp), col= colors_spec[4],lty="dotted", pch = 16, cex = 1.7,
     ylab = "log Relative Amplitude", xlab = "Distance (m)", xaxp = c(0, 100, 4) )
abline(CWdistMod, lwd = 4, col = colors_spec[4])
legend("topright", legend = c("CW: 3.00 kHz"), pch = 16, cex = 1, col = colors_spec[4])

#YBC
plot(YBC_analysis$distance_m, log10(YBC_analysis$relative.amp), col= colors_spec[5],lty="dotted", pch = 16, cex = 1.7,
     ylab = "log Relative Amplitude", xlab = "Distance (m)", xaxp = c(0, 100, 4) )
abline(YBCdistMod, lwd = 4, col = colors_spec[5])
legend("topright", legend = c("YBC: 1.70 kHz"), pch = 16, cex = 1, col = colors_spec[5])
