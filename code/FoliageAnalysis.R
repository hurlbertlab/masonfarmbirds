######################################
#### Foliage Analysis             ####
#### Last updated: 10/5/2023      ####
######################################

# Load libraries

library(dplyr)
library(tidyverse)
library(ggplot2)
library(tuneR)

############### Make analysis table

# Pull all file names
file_names <- list.files("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/trimmed_wav/")

# Pull in principle calls to get frequency at max amplitude

##### Visualizing all principal calls

# List of principal recording files

principal_calls <- list.files("../../OneDriveUNC/AudioMoths/ForestAcoustics/PricipalRecordings/mp3/")

# YBC

ybcmp3 <- readMP3("../../OneDriveUNC/AudioMoths/ForestAcoustics/PricipalRecordings/mp3/YBC_XC20962.mp3")
ybc_amps = seewave::spec(ybcmp3) %>%   # get a plot of amplitude (dB) vs frequency for the recording
  data.frame()
head(ybc_amps)                    # this just creates a matrix of the x and y values of that plot

max_amplitude_ybc = max(ybc_amps[,2])

freq_at_max_amp_ybc <- ybc_amps$x[ybc_amps$y == max_amplitude_ybc]

# EWP

ewpmp3 <- readMP3("../../OneDriveUNC/AudioMoths/ForestAcoustics/PricipalRecordings/mp3/EWP_XC649905_clipped.mp3")
ewp_amps = seewave::spec(ewpmp3) %>%   # get a plot of amplitude (dB) vs frequency for the recording
  data.frame()
head(ewp_amps)                    # this just creates a matrix of the x and y values of that plot

max_amplitude_ewp = max(ewp_amps[,2])

freq_at_max_amp_ewp <- ewp_amps$x[ewp_amps$y == max_amplitude_ewp]

# BG

bgmp3 <- readMP3("../../OneDriveUNC/AudioMoths/ForestAcoustics/PricipalRecordings/mp3/BG_XC726192_clipped.mp3")
bg_amps = seewave::spec(bgmp3) %>%   # get a plot of amplitude (dB) vs frequency for the recording
  data.frame()
head(bg_amps)                    # this just creates a matrix of the x and y values of that plot

max_amplitude_bg = max(bg_amps[,2])

freq_at_max_amp_bg <- bg_amps$x[bg_amps$y == max_amplitude_bg]

# Make df with all frequencies at max amplitude, the range will be + or - 0.05 kHz
# Note: all frequencies are in kHz

freqs <- c(freq_at_max_amp_ewp, freq_at_max_amp_ybc, freq_at_max_amp_bg)

species_freqs = data.frame(species = c("EWP", "YBC", "BG"), bottom.freq = (freqs - 0.05), top.freq = (freqs + 0.05))

# Make selection_table will all file names, and join with species_freqs

selection_table <- data.frame(sound.files = file_names, selec = (1:length(file_names)))
selection_table$species <- substr(selection_table$sound.files, 1, nchar(selection_table$sound.files) - 4)
selection_table$species <- word(selection_table$species, sep="_", 3)

selection_table_full <- left_join(selection_table, species_freqs, by = "species") 

##### For loop to acquire amplitude mean in the frequency range for each species

output_amps <- c()

for (i in (1:length(file_names))){
  # Read in wav file
  tmpwav <- readWave(paste("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/trimmed_wav/", file_names[i], sep=""))
  # Amp over Freq. data frame
  tmp_amps = seewave::spec(tmpwav) %>%
    data.frame()
  # Filter only frequencies of bird call
  amp_range <- tmp_amps %>%
    filter(x >= selection_table_full$bottom.freq[selection_table_full$sound.files == file_names[i]],
           x <= selection_table_full$top.freq[selection_table_full$sound.files == file_names[i]])
  mean_amp <- mean(amp_range$y)
  output_amps[i] <- mean_amp
}

# Add output amplitudes and filtering out CW3 and CW5 as they are redundant in this analysis
# Add distance & vol columns & remove columns unnecessary for analysis

selection_table_full$relative.amp <- output_amps

selection_table_full$distance_m <- word(selection_table_full$sound.files, sep="_", 2)
selection_table_full$foliage_level <- word(selection_table_full$sound.files, sep="_", 1)
selection_table_full$categories <- paste(selection_table_full$species, selection_table_full$distance_m, "m", sep=" ")

ggplot(selection_table_full, aes(x = foliage_level, y = relative.amp, fill = categories)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Relative Amp by Foliage Level",
    x = "Foliage Level",
    y = "Relative Amp"
  ) +
  scale_fill_manual(values = c("#2085F9", "#61ABFF", "#B2D6FF", "#D00000", "#D04C4C", "#D79A9A", "#FEAD00", "#FCC953", "#FFE8B5")) +
  theme_minimal() +
  theme(legend.position = "top")



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