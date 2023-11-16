######################################
#### Foliage Analysis             ####
#### Last updated: 11/16/2023     ####
######################################

# Load libraries

library(dplyr)
library(tidyverse)
library(ggplot2)
library(tuneR)
library(seewave)

############### Function to find mean amps

meanAmpOfCall = function(wavfile, frequency, threshold, cut_from, cut_to) {
  
  # read in file
  tmpwav = readWave(wavfile)
  
  # create matrix of values
  tmpspec = seewave::spectro(tmpwav)
  
  # find freq of max amplitude
  locationOfMax = which(tmpspec$freq == frequency, arr.ind = TRUE)
  #Row (first) value is the frequency of maximum amplitude
  
  # take all amplitudes at frequency of max amplitude
  ampsAtMaxFreq = tmpspec$amp[locationOfMax[1], tmpspec$time >= cut_from & tmpspec$time <= cut_to]
  
  # filter all amps to threshold
  ampsOfCall = ampsAtMaxFreq[ampsAtMaxFreq >= threshold]
  
  # take mean of filtered amps
  meanAmplitude = mean(ampsOfCall)
  
  
  return(meanAmplitude)
}

############### Find Relative Amp

relativeAmp = function(path, frequency, threshold) {
  # Pull all file names
  # Create DF with all amp values
  sp <- word(word(paste(path), 8, sep="/"), 1, sep = "_")
  output <- data.frame(Point = NULL, Species = NULL, Distance = NULL, Amp = NULL)
      mean_amp_1_1_25 <- meanAmpOfCall(paste(path), frequency, threshold, 0, 5)
      mean_amp_1_2_25 <- meanAmpOfCall(paste(path), frequency, threshold, 5, 10)
      mean_amp_1_3_25 <- meanAmpOfCall(paste(path), frequency, threshold, 10, 15)
      mean_amp_1_1_50 <- meanAmpOfCall(paste(path), frequency, threshold, 15, 20)
      mean_amp_1_2_50 <- meanAmpOfCall(paste(path), frequency, threshold, 20, 25)
      mean_amp_1_3_50 <- meanAmpOfCall(paste(path), frequency, threshold, 25, 30)
      
      mean_amp_2_1_25 <- meanAmpOfCall(paste(path), frequency, threshold, 30, 35)
      mean_amp_2_2_25 <- meanAmpOfCall(paste(path), frequency, threshold, 35, 40)
      mean_amp_2_3_25 <- meanAmpOfCall(paste(path), frequency, threshold, 40, 45)
      mean_amp_2_1_50 <- meanAmpOfCall(paste(path), frequency, threshold, 45, 50)
      mean_amp_2_2_50 <- meanAmpOfCall(paste(path), frequency, threshold, 50, 55)
      mean_amp_2_3_50 <- meanAmpOfCall(paste(path), frequency, threshold, 55, 60)
      
      mean_amp_3_1_25 <- meanAmpOfCall(paste(path), frequency, threshold, 60, 65)
      mean_amp_3_2_25 <- meanAmpOfCall(paste(path), frequency, threshold, 65, 70)
      mean_amp_3_3_25 <- meanAmpOfCall(paste(path), frequency, threshold, 70, 75)
      mean_amp_3_1_50 <- meanAmpOfCall(paste(path), frequency, threshold, 75, 80)
      mean_amp_3_2_50 <- meanAmpOfCall(paste(path), frequency, threshold, 80, 85)
      mean_amp_3_3_50 <- meanAmpOfCall(paste(path), frequency, threshold, 85, 90)
      
      df <- data.frame(Species = sp, Foliage = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3), Rep = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3), Distance = c(25, 25, 25, 50, 50, 50, 25, 25, 25, 50, 50, 50, 25, 25, 25, 50, 50, 50), Amp = c(mean_amp_1_1_25, mean_amp_1_2_25, mean_amp_1_3_25, mean_amp_1_1_50, mean_amp_1_2_50, mean_amp_1_3_50,mean_amp_2_1_25, mean_amp_2_2_25, mean_amp_2_3_25,mean_amp_2_1_50, mean_amp_2_2_50, mean_amp_2_3_50,mean_amp_3_1_25, mean_amp_3_2_25, mean_amp_3_3_25,mean_amp_3_1_50, mean_amp_3_2_50, mean_amp_3_3_50))
      output <- rbind(output, df)
  
  return(output)
}

############### Find mean of each foliage and distance combo

meanAmp = function(output_df) {
  output <- data.frame(Point = NULL, Species = NULL, Distance = NULL, Amp = NULL)
  
  output_df$RelativeAmpPercent <- (max(output_df$Amp, na.rm = TRUE) / output_df$Amp) * 100
  mean_fol1_25m <- mean(output_df$RelativeAmpPercent[1:3], na.rm = TRUE)
  mean_fol1_50m <- mean(output_df$RelativeAmpPercent[4:6], na.rm = TRUE)
  mean_fol2_25m <- mean(output_df$RelativeAmpPercent[7:9], na.rm = TRUE)
  mean_fol2_50m <- mean(output_df$RelativeAmpPercent[10:12], na.rm = TRUE)
  mean_fol3_25m <- mean(output_df$RelativeAmpPercent[13:15], na.rm = TRUE)
  mean_fol3_50m <- mean(output_df$RelativeAmpPercent[16:18], na.rm = TRUE)
  
  df <- data.frame(Species = output_df$Species[1:6], Foliage = c(1, 1, 2, 2, 3, 3), Distance = c(25, 50, 25, 50, 25, 50), MeanRelAmpPercent = c(mean_fol1_25m, mean_fol1_50m, mean_fol2_25m, mean_fol2_50m, mean_fol3_25m, mean_fol3_50m))
  output <- rbind(output, df)
  output$Categories <- paste(output$Species, output$Foliage, output$Distance, sep = "_")
  
  return(output)
}

############### Start analysis

##### Find amps for each species

BGfrequency <- 6.8750
BGpath <- "../../OneDriveUNC/AudioMoths/ForestAcoustics/20231105/Concatenated/BG_123.wav"
BG_output <- relativeAmp(BGpath, BGfrequency, -30)

YBCfrequency <- 1.0625
YBCpath <- "../../OneDriveUNC/AudioMoths/ForestAcoustics/20231105/Concatenated/YBC_123.wav"
YBC_output <- relativeAmp(YBCpath, YBCfrequency, -28)

### at 2/3/50 there is NaN, because they were completely inaudible
MDfrequency <- 0.4375 
MDpath <- "../../OneDriveUNC/AudioMoths/ForestAcoustics/20231105/Concatenated/MD_123.wav"
MD_output <- relativeAmp(MDpath, MDfrequency, -30)

### at 2/3/50 there is NaN, because they were completely inaudible
AFfrequency <- 4.1875
AFpath <- "../../OneDriveUNC/AudioMoths/ForestAcoustics/20231105/Concatenated/AF_123.wav"
AF_output <- relativeAmp(AFpath, AFfrequency, -30)

CWfrequency <- 3.8750
CWpath <- "../../OneDriveUNC/AudioMoths/ForestAcoustics/20231105/Concatenated/CW_123.wav"
CW_output <- relativeAmp(CWpath, CWfrequency, -24)

EWPfrequency <- 4.1250
EWPpath <- "../../OneDriveUNC/AudioMoths/ForestAcoustics/20231105/Concatenated/EWP_123.wav"
EWP_output <- relativeAmp(EWPpath, EWPfrequency, -24)

##### Amp calculations

meanAmpBG <- meanAmp(BG_output)
meanAmpYBC <- meanAmp(YBC_output)
meanAmpMD <- meanAmp(MD_output)
meanAmpAF <- meanAmp(AF_output)
meanAmpCW <- meanAmp(CW_output)
meanAmpEWP <- meanAmp(EWP_output)

##### Plotting

# when plotting, make the y axis in magnitudes of 10, with 100% being 100%, 
# and each tick down is a factor of 10
# (10%, 1%, 0.1%, etc.)

### Make linear models

# BG

bgf1 <- meanAmpBG[1:2,]
bgf2 <- meanAmpBG[3:4,]
bgf3 <- meanAmpBG[5:6,]

BG_lm1 <- lm(MeanRelAmpPercent ~ Distance, data = bgf1)
BG_lm2 <- lm(MeanRelAmpPercent ~ Distance, data = bgf2)
BG_lm3 <- lm(MeanRelAmpPercent ~ Distance, data = bgf3)
 
# YBC

ybc1 <- meanAmpYBC[1:2,]
ybc2 <- meanAmpYBC[3:4,]
ybc3 <- meanAmpYBC[5:6,]

YBC_lm1 <- lm(MeanRelAmpPercent ~ Distance, data = ybc1)
YBC_lm2 <- lm(MeanRelAmpPercent ~ Distance, data = ybc2)
YBC_lm3 <- lm(MeanRelAmpPercent ~ Distance, data = ybc3)

# MD

md1 <- meanAmpMD[1:2,]
md2 <- meanAmpMD[3:4,]
md3 <- meanAmpMD[5:6,]

MD_lm1 <- lm(MeanRelAmpPercent ~ Distance, data = md1)
MD_lm2 <- lm(MeanRelAmpPercent ~ Distance, data = md2)
MD_lm3 <- lm(MeanRelAmpPercent ~ Distance, data = md3)

# AF

af1 <- meanAmpAF[1:2,]
af2 <- meanAmpAF[3:4,]
af3 <- meanAmpAF[5:6,]

AF_lm1 <- lm(MeanRelAmpPercent ~ Distance, data = af1)
AF_lm2 <- lm(MeanRelAmpPercent ~ Distance, data = af2)
AF_lm3 <- lm(MeanRelAmpPercent ~ Distance, data = af3)

# CW

cw1 <- meanAmpCW[1:2,]
cw2 <- meanAmpCW[3:4,]
cw3 <- meanAmpCW[5:6,]

CW_lm1 <- lm(MeanRelAmpPercent ~ Distance, data = cw1)
CW_lm2 <- lm(MeanRelAmpPercent ~ Distance, data = cw2)
CW_lm3 <- lm(MeanRelAmpPercent ~ Distance, data = cw3)

# EWP

ewp1 <- meanAmpEWP[1:2,]
ewp2 <- meanAmpEWP[3:4,]
ewp3 <- meanAmpEWP[5:6,]

EWP_lm1 <- lm(MeanRelAmpPercent ~ Distance, data = ewp1)
EWP_lm2 <- lm(MeanRelAmpPercent ~ Distance, data = ewp2)
EWP_lm3 <- lm(MeanRelAmpPercent ~ Distance, data = ewp3)

### Merge to make dfs by foliage level w all birds

# Foliage level 1
foliage1_allbirds <- rbind(bgf1, ybc1, md1, af1, cw1, ewp1)

# Foliage level 2
foliage2_allbirds <- rbind(bgf2, ybc2, md2, af2, cw2, ewp2)

# Foliage level 3
foliage3_allbirds <- rbind(bgf3, ybc3, md3, af3, cw3, ewp3)

#### Graphs

par(mfrow=c(2,3))

## plots of different species

# plot BG
plot(meanAmpBG$Distance, meanAmpBG$MeanRelAmpPercent, col="black",lty="dotted", pch = 16, cex = 1.7,
     ylab = "Relative Amplitude (%)", xlab = "Distance (m)", xaxp = c(0, 50, 2), ylim = c(60,100), yaxt = "n", main="BG Amp v Distance")
axis(c(100,10,1,0.1, 0.01), side = 2, at = c(100, 90, 80, 70, 60), las = 1)
abline(BG_lm1, lwd = 4, col = "#A6D1FF")
abline(BG_lm2, lwd = 4, col = "#4488D2")
abline(BG_lm3, lwd = 4, col = "#003975")
legend("topright", legend = c("Foliage1", "Foliage2", "Foliage3"), pch = 16, cex = 1.5, col = c("#A6D1FF", "#4488D2", "#003975"))

# plot ybc
plot(meanAmpYBC$Distance, meanAmpYBC$MeanRelAmpPercent, col="black",lty="dotted", pch = 16, cex = 1.7,
     ylab = "Relative Amplitude (%)", xlab = "Distance (m)", xaxp = c(0, 50, 2), ylim = c(60,100), yaxt = "n", main="YBC Amp v Distance")
axis(c(100,10,1,0.1, 0.01), side = 2, at = c(100, 90, 80, 70, 60), las = 1)
abline(YBC_lm1, lwd = 4, col = "#FFDE84")
abline(YBC_lm2, lwd = 4, col = "#F0B924")
abline(YBC_lm3, lwd = 4, col = "#9D7300")
legend("topright", legend = c("Foliage1", "Foliage2", "Foliage3"), pch = 16, cex = 1.5, col = c("#FFDE84", "#F0B924", "#9D7300"))

# plot md
plot(meanAmpMD$Distance, meanAmpMD$MeanRelAmpPercent, col="black",lty="dotted", pch = 16, cex = 1.7, yaxt = "n",
     ylab = "Relative Amplitude (%)", xlab = "Distance (m)", xaxp = c(0, 50, 2), ylim = c(60,100), main="MD Amp v Distance")
axis(c(100,10,1,0.1, 0.01), side = 2, at = c(100, 90, 80, 70, 60), las = 1)
abline(MD_lm1, lwd = 4, col = "#D58DFF")
abline(MD_lm2, lwd = 4, col = "#9715E3")
abline(MD_lm3, lwd = 4, col = "#4C0078")
legend("topright", legend = c("Foliage1", "Foliage2", "Foliage3"), pch = 16, cex = 1.5, col = c("#D58DFF", "#9715E3", "#4C0078"))

# plot af
plot(meanAmpAF$Distance, meanAmpAF$MeanRelAmpPercent, col="black",lty="dotted", pch = 16, cex = 1.7, yaxt = "n",
     ylab = "Relative Amplitude (%)", xlab = "Distance (m)", xaxp = c(0, 50, 2), ylim = c(60,100), main="AF Amp v Distance")
axis(c(100,10,1,0.1, 0.01), side = 2, at = c(100, 90, 80, 70, 60), las = 1)
abline(AF_lm1, lwd = 4, col = "#FFA555")
abline(AF_lm2, lwd = 4, col = "#FF7700")
abline(AF_lm3, lwd = 4, col = "#8F4300")
legend("topright", legend = c("Foliage1", "Foliage2", "Foliage3"), pch = 16, cex = 1.5, col = c("#FFA555", "#FF7700", "#8F4300"))

# plot cw
plot(meanAmpCW$Distance, meanAmpCW$MeanRelAmpPercent, col="black",lty="dotted", pch = 16, cex = 1.7, yaxt = "n",
     ylab = "Relative Amplitude (%)", xlab = "Distance (m)", xaxp = c(0, 50, 2), ylim = c(60,100), main="CW Amp v Distance")
axis(c(100,10,1,0.1, 0.01), side = 2, at = c(100, 90, 80, 70, 60), las = 1)
abline(CW_lm1, lwd = 4, col = "#7CFFC6")
abline(CW_lm2, lwd = 4, col = "#0ACE79")
abline(CW_lm3, lwd = 4, col = "#00693B")
legend("topright", legend = c("Foliage1", "Foliage2", "Foliage3"), pch = 16, cex = 1.5, col = c("#7CFFC6", "#0ACE79", "#00693B"))

# plot ewp
plot(meanAmpEWP$Distance, meanAmpEWP$MeanRelAmpPercent, col="black",lty="dotted", pch = 16, cex = 1.7, yaxt = "n", ylab = "Relative Amplitude (%)", xlab = "Distance (m)", xaxp = c(0, 50, 2), ylim = c(60,100), main="EWP Amp v Distance" )
axis(c(100,10,1,0.1, 0.01), side = 2, at = c(100, 90, 80, 70, 60), las = 1)
abline(EWP_lm1, lwd = 4, col = "#FFBED9")
abline(EWP_lm2, lwd = 4, col = "#FD569C")
abline(EWP_lm3, lwd = 4, col = "#870C3F")
legend("topright", legend = c("Foliage1", "Foliage2", "Foliage3"), pch = 16, cex = 1.5, col = c("#FFBED9", "#FD569C", "#870C3F"))

## plots of different foliage levels 

par(mfrow=c(3,1))

# foliage level 1
species <- c("BG", "YBC", "MD", "AF", "CW", "EWP")
freqs <- c(6.88, 4.21, 1.70)
Slope_foliage1 <- c(-0.08531, -0.07161, 0.04256)
fol1 <- data.frame(species, freqs, Slope_foliage1)
fol1lm <- lm(Slope_foliage1 ~ freqs, data=fol1)

plot(fol1$freqs, fol1$Slope_foliage1, col=c("#A6D1FF", "#FFBED9", "#FFDE84"),lty="dotted", pch = 16, cex = 2, ylab = "Slope of Relative Amp vs Distance", xlab = "Freq", main="Foliage 1")
abline(fol1lm, lwd = 4, col = "black")
legend("topright", legend = c("BG", "EWP", "YBC"), pch = 16, cex = 1, col = c("#A6D1FF", "#FFBED9", "#FFDE84"))

# foliage level 2
species <- c("BG", "EWP", "YBC")
freqs <- c(6.88, 4.21, 1.70)
Slope_foliage2 <- c(-0.0563, -0.09979, -0.1304)
fol2 <- data.frame(species, freqs, Slope_foliage2)
fol2lm <- lm(Slope_foliage2 ~ freqs, data=fol2)

plot(fol2$freqs, fol2$Slope_foliage2, col=c("#4488D2", "#FD569C", "#F0B924"),lty="dotted", pch = 16, cex = 2, ylab = "Slope of Relative Amp vs Distance", xlab = "Freq",  main="Foliage 2")
abline(fol2lm, lwd = 4, col = "black")
legend("topleft", legend = c("BG", "EWP", "YBC"), pch = 16, cex = 1, col = c("#4488D2", "#FD569C", "#F0B924"))

# foliage level 3
species <- c("BG", "EWP", "YBC")
freqs <- c(6.88, 4.21, 1.70)
Slope_foliage3 <- c(-0.1333, -0.03998, -0.05494 )
fol3 <- data.frame(species, freqs, Slope_foliage3)
fol3lm <- lm(Slope_foliage3 ~ freqs, data=fol3)

plot(fol3$freqs, fol3$Slope_foliage3, col=c("#00438A", "#BF0251", "#D3A117"),lty="dotted", pch = 16, cex = 2,ylab = "Slope of Relative Amp vs Distance", xlab = "Freq",  main="Foliage 3")
abline(fol3lm, lwd = 4, col = "black")
legend("topright", legend = c("BG", "EWP", "YBC"), pch = 16, cex = 1, col = c("#00438A", "#BF0251", "#D3A117"))

#########################################################################



wavfile <- "../../Desktop/pnt3BGs.wav"



bg225 <- readWave("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/trimmed_wav/2_25_BG.wav")
bg250 <- readWave("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/trimmed_wav/2_50_BG.wav")
bg125 <- readWave("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/trimmed_wav/1_25_BG.wav")
bg150 <- readWave("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/trimmed_wav/1_50_BG.wav")
bg325 <- readWave("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/trimmed_wav/3_25_BG.wav")
bg350 <- readWave("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/trimmed_wav/3_50_BG.wav")

meanAmpOfCall("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/trimmed_wav/1_25_BG.wav", -25, 0, 3)
meanAmpOfCall("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/trimmed_wav/1_50_BG.wav", -25, 0, 3)
meanAmpOfCall("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/trimmed_wav/2_25_BG.wav", -25, 0, 3)
meanAmpOfCall("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/trimmed_wav/2_50_BG.wav", -25, 0, 3)
meanAmpOfCall("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/trimmed_wav/3_25_BG.wav", -25, 0, 3)
meanAmpOfCall("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/trimmed_wav/3_50_BG.wav", -25, 0, 3)

pt3bgs <- readWave("../../Desktop/pnt3BGs.wav")
pt3bgsspect <- seewave::spectro(pt3bgs)
pt3bgsspect_25 <- pt3bgsspect %>%
  filter(pt3bgsspect$time < 2.6)

spectro(pt3bgs)
bg125spectro <- seewave::spectro(bg350)
spectro(bg325)
bg125spectro
par(new=TRUE); plot(bg125spectro$time, bg125spectro$amp[134,], type = 'l')
