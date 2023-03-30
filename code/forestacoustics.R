library(tuneR)
library(warbleR)
library(seewave)

## Sound Pressure Level (dB)

onemp3 <- readMP3("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230301/Clipped/13_25E_AF_100.mp3")  ## assuming file is in your working directory!

writeWave(onemp3,"onetmp.wav",extensible=FALSE)

selec_table = data.frame(sound.files = 'onetmp.wav', selec = 1, start = 0, end = 100, bottom.freq = 2000, top.freq = 8000)

output = sound_pressure_level(selec_table, path='temp')

output


# Now w our files...

file_names <- list.files("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230301/Clipped/")

### WIP

# Convert all to wav...
for (i in (1:length(file_names))){
  writeWave(paste("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230301/Clipped/", file_names[i], sep=""), filename = paste(file_names[i], ".wav", sep = ""),extensible=FALSE)
}

data = data.frame(sound.files = file_names, selec = (1:135), start = 0, end = 20, bottom.freq = 2000, top.freq = 8000)

output = sound_pressure_level(data[1,], path='../../OneDriveUNC/AudioMoths/ForestAcoustics/20230301/Clipped/')

#################################################

# Frequency at max amplitude
# This doesn't work :-)

library(dplyr)

onemp3 <- readMP3("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230301/Clipped/13_25E_AF_100.mp3")
one_amps = seewave::spec(onemp3)  # get a plot of amplitude (dB) vs frequency for the recording
head(one_amps)                    # this just creates a matrix of the x and y values of that plot

max_amplitude = max(one_amps[,2])    

# the maximum value of the second column, which is                                                                              # the highest point on the graph
freq_at_max_amplitude = one_amps[one_amps[,2] == max_amplitude, 1]    # this returns the x value(column 1, which is   frequency) at which the y value (column 2) is equal to the maximum value in column 2 (i.e. the highest amplitude)

# We can check that this is doing what we think. First plot the spectrogram:
seewave::spectro(onemp3)
abline(h = freq_at_max_amplitude, col = "orange")       # draw a horizontal line at the frequency of max amplitude, and see that it goes right through the red portion

