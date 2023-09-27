#######################################

#######    Make Clips Longer    #######

#######################################

# load libraries
library(dplyr)
library(tidyverse)
library(tuneR)
library(sound)

### for loop to grab every file, attach silence wav to extend, and save in new folder

files <- list.files("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230301/Wav/")

silence <- "../../OneDriveUNC/AudioMoths/ForestAcoustics/silence.wav"

#begin loop

for(a in 1:length(files)){}


  file <- readWave(paste("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230301/Wav/", files[a], sep=""))
  concatenated <- cbind(file, silence)
  writeWave(concatendated, paste("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230301/ExtendedClips/" files[a], sep = ""))

}

beep()





file <- "../Wrens/13_25E_CW3_75.mp3.wav"
params <- fitSampleParameters(file, silence)
fileparams <- params[1]
silenceparams <- params[2]
extended <- appendSample(fileparams, silenceparams)

saveSample(extended, "../../OneDriveUNC/AudioMoths/ForestAcoustics/20230301/ExtendedClips/wren1.wav", overwrite = FALSE)
           

           