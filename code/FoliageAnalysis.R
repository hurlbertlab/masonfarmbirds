######################################
#### Foliage Analysis             ####
#### Last updated: 10/24/2023      ####
######################################

# Load libraries

library(dplyr)
library(tidyverse)
library(ggplot2)
library(tuneR)
library(seewave)

############### Function to find mean amps

meanAmpOfCall = function(wavfile, threshold, cut_from, cut_to) {
  
  # read in file
  tmpwav = readWave(wavfile)
  
  # create matrix of values
  tmpspec = seewave::spectro(tmpwav)
  
  # find freq of max amplitude
  locationOfMax = which(tmpspec$amp == max(tmpspec$amp), arr.ind = TRUE)
  #Row (first) value is the frequency of maximum amplitude
  
  # take all amplitudes at frequency of max amplitude
  ampsAtMaxFreq = tmpspec$amp[locationOfMax[1], tmpspec$time >= cut_from & tmpspec$time <= cut_to]
  
  # filter all amps to threshold
  ampsOfCall = ampsAtMaxFreq[ampsAtMaxFreq >= threshold]
  
  # take mean of filtered amps
  meanAmplitude = mean(ampsOfCall)
  
  
  return(meanAmplitude)
}

############### Make analysis table

# Pull all file names
file_names <- list.files("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/concatenated/all_foliage/")

##### For loop to acquire amplitude mean in the frequency range for each species

output <- data.frame(Point = NULL, Species = NULL, Distance = NULL, Amp = NULL)

for (i in (1:length(file_names))){
  # Read in wav file
  tmpwav <- readWave(paste("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/concatenated/all_foliage/", file_names[i], sep=""))
  species = word(file_names[i], 1, sep="_")
  if(species == "BG"){
    mean_amp_1_25 <- meanAmpOfCall(paste("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/concatenated/all_foliage/", file_names[i], sep=""), -12, 0, 5)
    mean_amp_1_50 <- meanAmpOfCall(paste("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/concatenated/all_foliage/", file_names[i], sep=""), -12, 5, 10)
    mean_amp_2_25 <- meanAmpOfCall(paste("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/concatenated/all_foliage/", file_names[i], sep=""), -12, 10, 15)
    mean_amp_2_50 <- meanAmpOfCall(paste("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/concatenated/all_foliage/", file_names[i], sep=""), -12, 15, 20)
    mean_amp_3_25 <- meanAmpOfCall(paste("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/concatenated/all_foliage/", file_names[i], sep=""), -12, 20, 25)
    mean_amp_3_50 <- meanAmpOfCall(paste("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/concatenated/all_foliage/", file_names[i], sep=""), -12, 25, 30)
  }
  if(species == "EWP"){
    mean_amp_1_25 <- meanAmpOfCall(paste("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/concatenated/all_foliage/", file_names[i], sep=""), -26, 0, 5)
    mean_amp_1_50 <- meanAmpOfCall(paste("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/concatenated/all_foliage/", file_names[i], sep=""), -26, 5, 10)
    mean_amp_2_25 <- meanAmpOfCall(paste("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/concatenated/all_foliage/", file_names[i], sep=""), -26, 10, 15)
    mean_amp_2_50 <- meanAmpOfCall(paste("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/concatenated/all_foliage/", file_names[i], sep=""), -26, 15, 20)
    mean_amp_3_25 <- meanAmpOfCall(paste("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/concatenated/all_foliage/", file_names[i], sep=""), -26, 20, 25)
    mean_amp_3_50 <- meanAmpOfCall(paste("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/concatenated/all_foliage/", file_names[i], sep=""), -26, 25, 30)
  }
  if(species == "YBC"){
    mean_amp_1_25 <- meanAmpOfCall(paste("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/concatenated/all_foliage/", file_names[i], sep=""), -16, 0, 5)
    mean_amp_1_50 <- meanAmpOfCall(paste("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/concatenated/all_foliage/", file_names[i], sep=""), -16, 5, 10)
    mean_amp_2_25 <- meanAmpOfCall(paste("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/concatenated/all_foliage/", file_names[i], sep=""), -16, 10, 15)
    mean_amp_2_50 <- meanAmpOfCall(paste("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/concatenated/all_foliage/", file_names[i], sep=""), -16, 15, 20)
    mean_amp_3_25 <- meanAmpOfCall(paste("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/concatenated/all_foliage/", file_names[i], sep=""), -16, 20, 25)
    mean_amp_3_50 <- meanAmpOfCall(paste("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/concatenated/all_foliage/", file_names[i], sep=""), -16, 25, 30)
  }
    
  df <- data.frame(Species = species, Foliage = c(1, 1, 2, 2, 3, 3), Distance = c(25, 50, 25, 50, 25, 50), Amp = c(mean_amp_1_25, mean_amp_1_50, mean_amp_2_25, mean_amp_2_50, mean_amp_3_25, mean_amp_3_50))
  output <- rbind(output, df)
}



output$categories <- paste(output$Species, output$Foliage, output$Distance, sep="_")

ggplot(output, aes(x = Foliage, y = Amp, fill = categories)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Relative Amp by Foliage Level",
    x = "Foliage Level",
    y = "Relative Amp"
  )

#### Graphs

# make BG lms

bluegrays <- output %>%
  filter(Species == "BG")

bg1 <- bluegrays %>%
  filter(Foliage == 1)
bg2 <- bluegrays %>%
  filter(Foliage == 2)
bg3 <- bluegrays %>%
  filter(Foliage == 3)

bg1lm <- lm(Amp ~ Distance, data = bg1)
bg2lm <- lm(Amp ~ Distance, data = bg2)
bg3lm <- lm(Amp ~ Distance, data = bg3)

# make YBC lms

cuckoo <- output %>%
  filter(Species == "YBC")

ybc1 <- cuckoo %>%
  filter(Foliage == 1)
ybc2 <- cuckoo %>%
  filter(Foliage == 2)
ybc3 <- cuckoo %>%
  filter(Foliage == 3)

ybc1lm <- lm(Amp ~ Distance, data = ybc1)
ybc2lm <- lm(Amp ~ Distance, data = ybc2)
ybc3lm <- lm(Amp ~ Distance, data = ybc3)

# make ewp lms

pewee <- output %>%
  filter(Species == "EWP")

ewp1 <- pewee %>%
  filter(Foliage == 1)
ewp2 <- pewee %>%
  filter(Foliage == 2)
ewp3 <- pewee %>%
  filter(Foliage == 3)

ewp1lm <- lm(Amp ~ Distance, data = ewp1)
ewp2lm <- lm(Amp ~ Distance, data = ewp2)
ewp3lm <- lm(Amp ~ Distance, data = ewp3)

par(mfrow=c(2,2))

## plots of different species

# plot BG
plot(bluegrays$Distance, bluegrays$Amp, col="black",lty="dotted", pch = 16, cex = 1.7,
     ylab = "Relative Amplitude", xlab = "Distance (m)", xaxp = c(0, 50, 2), ylim = c(-12,-7), main="BG Amp v Distance")
abline(bg1lm, lwd = 4, col = "#A6D1FF")
abline(bg2lm, lwd = 4, col = "#4488D2")
abline(bg3lm, lwd = 4, col = "#003975")
legend("topright", legend = c("Foliage1", "Foliage2", "Foliage3"), pch = 16, cex = 1, col = c("#A6D1FF", "#4488D2", "#003975"))

# plot ybc
plot(cuckoo$Distance, cuckoo$Amp, col="black",lty="dotted", pch = 16, cex = 1.7,
     ylab = "Relative Amplitude", xlab = "Distance (m)", xaxp = c(0, 50, 2), ylim =c(-15,-10), main="YBC Amp v Distance")
abline(ybc1lm, lwd = 4, col = "#FFDE84")
abline(ybc2lm, lwd = 4, col = "#F0B924")
abline(ybc3lm, lwd = 4, col = "#9D7300")
legend("topleft", legend = c("Foliage1", "Foliage2", "Foliage3"), pch = 16, cex = 0.8, col = c("#FFDE84", "#F0B924", "#9D7300"))

# plot ewp
plot(pewee$Distance, pewee$Amp, col="black",lty="dotted", pch = 16, cex = 1.7,
     ylab = "Relative Amplitude", xlab = "Distance (m)", xaxp = c(0, 50, 2), main="EWP Amp v Distance" )
abline(ewp1lm, lwd = 4, col = "#FFBED9")
abline(ewp2lm, lwd = 4, col = "#FD569C")
abline(ewp3lm, lwd = 4, col = "#870C3F")
legend("topright", legend = c("Foliage1", "Foliage2", "Foliage3"), pch = 16, cex = 0.8, col = c("#FFBED9", "#FD569C", "#870C3F"))

## plots of different foliage levels 

par(mfrow=c(2,2))

# foliage level 1
species <- c("BG", "EWP", "YBC")
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
