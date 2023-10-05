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
file_names <- list.files("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/3sec_wav/")

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

species_freqs = data.frame(species = c("AF", "CW5", "CW3", "CW1", "EWP", "YBC", "BG"), bottom.freq = (freqs - 0.05), top.freq = (freqs + 0.05))

# Make selection_table will all file names, and join with species_freqs

selection_table <- data.frame(sound.files = file_names, selec = (1:length(file_names)))
selection_table$species <- substr(selection_table$sound.files, 1, nchar(selection_table$sound.files) - 4)
selection_table$species <- word(selection_table$species, sep="_", 3)

selection_table_full <- left_join(selection_table, species_freqs, by = "species") 

##### For loop to acquire amplitude mean in the frequency range for each species

output_amps <- c()

for (i in (1:length(file_names))){
  # Read in wav file
  tmpwav <- readWave(paste("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230925/Clipped/", file_names[i], sep=""))
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





foliage1 <- selection_table_full %>%
  filter(species == "EWP")
plot(foliage1$distance_m, foliage1$relative.amp, pch = 16, cex = 2, 
     ylab = "Relative amplitude", xlab = "Distance (m)")
