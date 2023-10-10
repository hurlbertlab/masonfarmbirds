#######################################

#######    Make Clips Longer    #######

#######################################

# load libraries
library(dplyr)
library(tidyverse)
library(tuneR)
library(beepr)

### for loop to grab every file, attach silence wav to extend, and save in new folder

files <- list.files("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230301/Wav/")

silence <- readWave("../../OneDriveUNC/AudioMoths/ForestAcoustics/silence.wav")

#begin loop

for(a in 1:length(files)){

  audio <- readWave(paste("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230301/Wav/", files[a], sep=""))
  concatenated <- bind(audio, silence)
  writeWave(concatenated, paste("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230301/ExtendedClips/", files[a], sep = ""))

}

beep()

