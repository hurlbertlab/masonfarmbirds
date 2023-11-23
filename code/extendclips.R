#######################################

#######    Make Clips Longer    #######

#######################################

# load libraries
library(dplyr)
library(tidyverse)
library(tuneR)
library(beepr)

### for loop to grab every file, attach silence wav to extend, and save in new folder

files <- list.files("../../OneDriveUNC/AudioMoths/ForestAcoustics/20231105_foliageanalysis/Trimmed/")

silence <- readWave("data/audiofiles/silence.wav")

#begin loop

for(a in 1:length(files)){

  audio <- readWave(paste("../../OneDriveUNC/AudioMoths/ForestAcoustics/20231105_foliageanalysis/Trimmed/", files[a], sep=""))
  concatenated <- bind(audio, downsample(silence, as.numeric(audio@samp.rate)))
  writeWave(concatenated, paste("../../OneDriveUNC/AudioMoths/ForestAcoustics/20231105_foliageanalysis/Extended/", files[a], sep = ""))

}

beep()

