#########################################
###### Forest Acoustics Analysis   ######
###### Amelia Milano               ######
###### Last update: 9/19/2023      ######
#########################################

library(dplyr)
library(tidyverse)

# read in table
analysis_table <- read.csv("data/analysis_table_03012023.csv")

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
legend("topright", legend = c("0°, R2 = 0.7807, p = 0.0052**", 
                              "90°, R2 = 0.4741, p = 0.1941", 
                              "180°, R2 = 0.9181, p = 0.0001***", 
                              "270°, R2 = 0.9741, p = 0.00868**"), pch = 16, cex = 1.7, col = colors_dir[1:4], title = "Bearing Relative to ARU")



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
legend("topright", legend = c("0° Toward, R2 = 0.7807, p = 0.0052**", 
                              "0° Away, R2 = 0.5821, p = 0.3022", 
                              "180° Toward, R2 = 0.9181, p = 0.0001***", 
                              "180° Away, R2 = 0.9349, p = 0.1155"),pch = 16, cex = 1.7, col = colorsTA[1:4], title = "Location & Orientation of Speaker")


##########################

### Raw graphs 9/19/2023

### Direction and Dist

par(mfrow=c(2,2))

# 0 Degree

plot(AF_0T$distance_m, log(AF_0T$relative.amp), col= colors_dir[1] ,lty="dotted", pch = 16, cex = 1.7,
     ylab = "log Relative Amplitude", xlab = "Distance (m)", xaxp = c(0, 100, 4), main = "0°, R2 = 0.7807, p = 0.0052**")
abline(AF0distMod, lwd = 5, col = colors_dir[1])

# 90 Degree


plot(AF_90E$distance_m, log(AF_90E$relative.amp), col= colors_dir[2] ,lty="dotted", pch = 16, cex = 1.7,
     ylab = "log Relative Amplitude", xlab = "Distance (m)", xaxp = c(0, 100, 4), main = "90°, R2 = 0.4741, p = 0.194")
abline(AF90EdistMod, lwd = 5, col = colors_dir[2])

# 180 Degree

plot(AF_180T$distance_m, log(AF_180T$relative.amp), col= colors_dir[3] ,lty="dotted", pch = 16, cex = 1.7,
     ylab = "log Relative Amplitude", xlab = "Distance (m)", xaxp = c(0, 100, 4), main = "180°, R2 = 0.9181, p = 0.0001***")
abline(AF180distMod, lwd = 5, col = colors_dir[3])

# 270 Degree

plot(AF_90W$distance_m, log(AF_90W$relative.amp), col= colors_dir[4] ,lty="dotted", pch = 16, cex = 1.7,
     ylab = "log Relative Amplitude", xlab = "Distance (m)", xaxp = c(0, 100, 4), main= "270° Bearing, R2 = 0.9741, p = 0.00868**")
abline(AF270distMod, lwd = 5, col = colors_dir[4])

### Toward and Away

par(mfrow=c(2,2))

# 0 Degree Toward

plot(AF_0T$distance_m, log(AF_0T$relative.amp), col= colorsTA[1] ,lty="dotted", pch = 16, cex = 1.7,
     ylab = "log Relative Amplitude", xlab = "Distance (m)", xaxp = c(0, 100, 4), main = "0° Toward, R2 = 0.7807, p = 0.0052**")
abline(AF0TdistMod, lwd = 5, col = colorsTA[1])

# 0 Degree Away

plot(AF_0A$distance_m, log(AF_0A$relative.amp), col= colorsTA[2] ,lty="dotted", pch = 16, cex = 1.7, ylim = c(-3.6, -1.9),
     ylab = "log Relative Amplitude", xlab = "Distance (m)", xaxp = c(0, 100, 4), main = "0° Away, R2 = 0.5821, p = 0.3022")
abline(AF0AdistMod, lwd = 5, col = colorsTA[2])

# 180 Degree Toward

plot(AF_180T$distance_m, log(AF_180T$relative.amp), col= colorsTA[3] ,lty="dotted", pch = 16, cex = 1.7,
     ylab = "log Relative Amplitude", xlab = "Distance (m)", xaxp = c(0, 100, 4), main = "180° Toward, R2 = 0.9181, p = 0.0001***")
abline(AF180TdistMod, lwd = 5, col = colorsTA[3])

# 180 Degree Away

plot(AF_180A$distance_m, log(AF_180A$relative.amp), col= colorsTA[4] ,lty="dotted", pch = 16, cex = 1.7, ylim = c(-5.1, -3.5),
     ylab = "log Relative Amplitude", xlab = "Distance (m)", xaxp = c(0, 100, 4), main= "180° Away, R2 = 0.9349, p = 0.1155")
abline(AF180AdistMod, lwd = 5, col = colorsTA[4])
