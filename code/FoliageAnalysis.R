######################################
#### Foliage Analysis             ####
#### Last updated: 10/5/2023      ####
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
file_names <- list.files("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/concatenated/")

##### For loop to acquire amplitude mean in the frequency range for each species

output <- data.frame(Point = NULL, Species = NULL, Distance = NULL, Amp = NULL)

for (i in (1:length(file_names))){
  # Read in wav file
  tmpwav <- readWave(paste("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/concatenated/", file_names[i], sep=""))
  species = substr(word(file_names[i], 2, sep="_"), 1, nchar(word(file_names[i], 2, sep="_")) -4)
  point = word(file_names[i], 1, sep="_")
  mean_amp_25 <- meanAmpOfCall(paste("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/concatenated/", file_names[i], sep=""), -25, 0, 5)
  mean_amp_50 <- meanAmpOfCall(paste("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/concatenated/", file_names[i], sep=""), -25, 5, 10)
  df <- data.frame(Point = point, Species = species, Distance = c(25, 50), Amp = c(mean_amp_25, mean_amp_50))
  output <- rbind(output, df)
}









output$categories <- paste(output$Species, output$Distance, sep="")

ggplot(output, aes(x = Point, y = Amp, fill = categories)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Relative Amp by Foliage Level",
    x = "Foliage Level",
    y = "Relative Amp"
  )



foliage1_ewp <- selection_table_full %>%
  filter(species == "EWP") %>%
  filter(foliage_level == 0 | foliage_level == 1)
plot(foliage1_ewp$distance_m, foliage1$relative.amp, pch = 16, cex = 2, 
     ylab = "Relative amplitude", xlab = "Distance (m)")

foliage1_bg <- selection_table_full %>%
  filter(species == "BG") %>%
  filter(foliage_level == 0 | foliage_level == 1)
plot(foliage1_bg$distance_m, foliage1_bg$relative.amp, pch = 16, cex = 2, 
     ylab = "Relative amplitude", xlab = "Distance (m)")

foliage2_bg <- selection_table_full %>%
  filter(species == "BG") %>%
  filter(foliage_level == 0 | foliage_level == 2)
plot(foliage2_bg$distance_m, foliage2_bg$relative.amp, pch = 16, cex = 2, 
     ylab = "Relative amplitude", xlab = "Distance (m)")

foliage3_bg <- selection_table_full %>%
  filter(species == "BG") %>%
  filter(foliage_level == 0 | foliage_level == 3)
plot(foliage3_bg$distance_m, foliage3_bg$relative.amp, pch = 16, cex = 2, 
     ylab = "Relative amplitude", xlab = "Distance (m)")





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
