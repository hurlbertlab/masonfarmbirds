#########################################
###### Forest Acoustics Analysis   ######
###### Amelia Milano               ######
###### Last update: 4/20/2023      ######
#########################################

library(dplyr)
library(tidyverse)

# read in table
analysis_table <- read.csv("data/analysis_table.csv")

# subset AF data
AF <- analysis_table %>%
  filter(species == "AF", vol == "100")

# set relative amp as numeric
AF$relative.amp <- as.numeric(AF$relative.amp)
AF$distance_m <- as.numeric(AF$distance_m)

# Directions (0 degrees, 90 degrees, 180 degrees, )

AF_0 <- AF %>%
  filter(relative_dir == "N")

AF_0T <- AF %>%
  filter(relative_dir == "N", TA == "T")

AF_0A <- AF %>%
  filter(relative_dir == "N", TA == "A")

AF_90W <- AF %>%
  filter(relative_dir == "W", TA == "T")

AF_90E <- AF %>%
  filter(relative_dir == "E", TA == "T")

AF_180T <- AF %>%
  filter(relative_dir == "S", TA == "T")

AF_180A <- AF %>%
  filter(relative_dir == "S", TA == "A")

# Directional & dist lm
AFdistMod = lm(log(relative.amp) ~ distance_m, data = AF)
AF0distMod = lm(log(relative.amp) ~ distance_m, data = AF_0T)
AF270distMod = lm(log(relative.amp) ~ distance_m, data = AF_90W)
AF90EdistMod = lm(log(relative.amp) ~ distance_m, data = AF_90E)
AF180distMod = lm(log(relative.amp) ~ distance_m, data = AF_180T)

# Plot

colors_dir <- c("#D81B60", "#1E88E5", "#FFC107", "#00C16C")

plot(AF$distance_m, log(AF$relative.amp), col="white",lty="dotted", pch = 16, cex = 1.7,
     ylab = "log Relative Amplitude", xlab = "Distance (m)", xaxp = c(0, 100, 4), )
abline(AF0distMod, lwd = 5, col = colors_dir[1])
abline(AF90EdistMod, lwd = 5, col = colors_dir[2])
abline(AF180distMod, lwd = 5, col = colors_dir[3])
abline(AF270distMod, lwd = 5, col = colors_dir[4])
legend("topright", legend = c("0°", "90°", "180°", "270°"), pch = 16, cex = 2, col = colors_dir[1:4], title = "Bearing Relative to ARU")



#### Toward and Away

colorsTA = c("#DE67C2", "#984284", "#00C16C", "#006538")

AF0AdistMod = lm(log(relative.amp) ~ distance_m, data = AF_0A)
AF0TdistMod = lm(log(relative.amp) ~ distance_m, data = AF_0T)
AF180AdistMod = lm(log(relative.amp) ~ distance_m, data = AF_180A)
AF180TdistMod = lm(log(relative.amp) ~ distance_m, data = AF_180T)

plot(AF$distance_m, log(AF$relative.amp), col="white",lty="dotted", pch = 16, cex = 1.7,
     ylab = "log Relative Amplitude", xlab = "Distance (m)", xaxp = c(0, 100, 4))
abline(AF0AdistMod, lwd = 5, col = colorsTA[2])
abline(AF0TdistMod, lwd = 5, col = colorsTA[1])
abline(AF180AdistMod, lwd = 5, col = colorsTA[4])
abline(AF180TdistMod, lwd = 5, col = colorsTA[3])
legend("topright", legend = c("0° Toward", "0° Away", "180° Toward", "180° Away"), pch = 16, cex = 2, col = colorsTA[1:4], title = "Location & Orientation of Speaker")
