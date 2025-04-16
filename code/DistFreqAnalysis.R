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

# BG AF EWP CW YBC
colors_spec = c("#4488D2","#FF7700", "#FD569C", "#0ACE79", "#F0B924")

analysis <- analysis_table %>%
  filter(relative_dir == "N", vol == "100")
speciesColors = data.frame(species = c('BG', 'AF', 'EWP', 'CW5', 'YBC'), 
                           color = colors_spec,
                           pch = c(15, 16, 17, 18, 9))
analysis$relative.amp <- as.numeric(analysis$relative.amp)
analysis$distance_m <- as.numeric(analysis$distance_m)

analysis2 = left_join(analysis, speciesColors, by = 'species')


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

##############Combine to make figure 4############
png("plots/Figure4AmpvDis.png", height = 1200, width = 900)
par(mfrow=c(2,1), mar = c(7, 8, 4, 1), mgp = c(5, 1, 0))
  #can also make it pdf, specify in inches


plot(analysis2$distance_m, log10(analysis2$relative.amp), col = analysis2$color ,lty="dotted", pch = analysis2$pch, cex = 2.5,
     ylab = "log10 Relative Amplitude", xlab = "Distance (m)", xaxp = c(0, 100, 4), ylim = c(-2.3, -.3), cex.lab = 2.5, cex.axis = 2, yaxt = 'n')
axis(2, at = log10(c(0.01, 0.03, 0.1, 0.3, 1)), labels = c("1%", "3%", "10%", "30%", "100%"), las = 1, cex.axis = 2)
abline(BGdistMod, lwd = 4, col = colors_spec[1])
abline(AFdistMod, lwd = 4, col = colors_spec[2])
abline(EWPdistMod, lwd = 4, col = colors_spec[3])
abline(CWdistMod, lwd = 4, col = colors_spec[4])
abline(YBCdistMod, lwd = 4, col = colors_spec[5])
legend("topright", legend = c("BG: p = 0.0001, R2 = 0.93", "AF: p = 0.005, R2 = 0.82", "EWP: p = 0.002, R2 = 0.83", "CW: p = 0.002, R2 = 0.82", "YBC: p = 0.0003, R2 = 0.90"), pch = speciesColors$pch, cex = 2, col = colors_spec[1:5])

mtext("A", side = 3, adj = 0, line = 1, cex = 3)

species <- c("BG", "AF", "EWP", "CW", "YBC")
frequencies <- c(6.88, 5.27, 4.21, 3.00, 1.70)
slope <- c(-0.035, -0.029, -0.028, -0.028, -0.023)

freqvslope <- data.frame(species, frequencies, slope)


freqvslopegraph = lm(slope ~ frequencies, data=freqvslope)
plot(freqvslope$frequencies, freqvslope$slope, col=c("#4488D2","#FF7700", "#FD569C", "#0ACE79", "#F0B924"), lty="dotted", pch = 16, cex = 3,
      xlab = "", ylab = "", cex.lab = 2.5, cex.axis = 2)
abline(freqvslopegraph, lwd = 4, col = "black") 
title(ylab = "Distance Decay Rate", line = 5, cex.lab = 2.5)
title(xlab = "Frequency (kHz)", line = 4, cex.lab = 2.5)
legend("topright", legend = c("BG", "AF", "EWP", "CW", "YBC"), pch = 16, cex = 2, col = colors_spec[1:5])
text(4, -0.025,"p = 0.16\nR2 = 0.89", cex= 2)
    ##can also add letter with text

mtext("B", side = 3, adj = 0, line = 1, cex = 3)


dev.off()

##################################################

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

