#########################################
###### Forest Acoustics Analysis   ######
###### Amelia Milano               ######
###### Last update: 4/13/2023      ######
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

# plot
plot(AF$distance_m, log(AF$relative.amp), col=AF$col_dir, pch = 16, cex = 2, 
     ylab = "log Relative amplitude", xlab = "Distance (m)")
legend("topright", legend = c("N", "S", "E", "W"), pch = 16, cex = 2, col = colors[1:4])

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

AF_180 <- AF %>%
  filter(relative_dir == "S")

# Directional & dist lm
AFdistMod = lm(log(relative.amp) ~ distance_m, data = AF)
AF0distMod = lm(log(relative.amp) ~ distance_m, data = AF_0)
AF90WdistMod = lm(log(relative.amp) ~ distance_m, data = AF_90W)
AF90EdistMod = lm(log(relative.amp) ~ distance_m, data = AF_90E)
AF180distMod = lm(log(relative.amp) ~ distance_m, data = AF_180)

# Plot

colors_dir <- c("yellow", "red", "black", "blue")

plot(AF$distance_m, log(AF$relative.amp), col="white",lty="dotted", pch = 16, cex = 1.7,
     ylab = "log Relative Amplitude", xlab = "Distance (m)", xaxp = c(0, 100, 4))
abline(AFdistMod, lwd = 4)

abline(AF0distMod, lwd = 4, col = colors_dir[1])
abline(AF90WdistMod, lwd = 4, col = colors_dir[2])
abline(AF90EdistMod, lwd = 4, col = colors_dir[3])
abline(AF180distMod, lwd = 4, col = colors_dir[4])
