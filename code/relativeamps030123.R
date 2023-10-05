library(tuneR)
library(warbleR)
library(seewave)
library(stringr)
library(dplyr)

#########################################
###### Extract Relative Amplitudes ######
###### Last update: 10/05/2023     ######
#########################################

##### Problems to fix:
# - in analysis, there should be a way to simply change a variable name and analyze a different date - rn it's hard coded 

######

# All file names

file_names <- list.files("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230301/Clipped/")

# Convert all files to .wav (not necessary for seewave analysis, but necessary for warbleR and others -- ignore for now)

for (i in (1:length(file_names))){
  tmpmp3 <- readMP3(paste("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230301/Clipped/", file_names[i], sep=""))
  writeWave(tmpmp3, filename = paste("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230301/Wav/", substr(file_names[i], 1, nchar(file_names[i]) - 4), ".wav", sep = ""))
}


##### Visualizing all principal calls

# List of principal recording files

principal_calls <- list.files("../../OneDriveUNC/AudioMoths/ForestAcoustics/PricipalRecordings/mp3/")

# We're going to look at each call specifically

# AF

afmp3 <- readMP3("../../OneDriveUNC/AudioMoths/ForestAcoustics/PricipalRecordings/mp3/AF_XC583856_clipped.mp3")
af_amps = seewave::spec(afmp3) %>%   # get a plot of amplitude (dB) vs frequency for the recording
  data.frame()
head(af_amps)                    # this just creates a matrix of the x and y values of that plot

max_amplitude_af = max(af_amps[,2])

freq_at_max_amp_af <- af_amps$x[af_amps$y == max_amplitude_af]

# CW

cw5mp3 <- readMP3("../../OneDriveUNC/AudioMoths/ForestAcoustics/PricipalRecordings/mp3/CW_XC738783_clipped5.mp3")
cw5_amps = seewave::spec(cw5mp3) %>%   # get a plot of amplitude (dB) vs frequency for the recording
  data.frame()
head(cw5_amps)                    # this just creates a matrix of the x and y values of that plot

max_amplitude_cw5 = max(cw5_amps[,2])

freq_at_max_amp_cw5 <- cw5_amps$x[cw5_amps$y == max_amplitude_cw5]

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

###### Make df with all frequencies at max amplitude, the range will be + or - 0.05 kHz

# Note: all frequencies are in kHz

freqs <- c(freq_at_max_amp_af, freq_at_max_amp_cw5, freq_at_max_amp_cw5, freq_at_max_amp_cw5, freq_at_max_amp_ewp, freq_at_max_amp_ybc, freq_at_max_amp_bg)

species_freqs = data.frame(species = c("AF", "CW5", "CW3", "CW1", "EWP", "YBC", "BG"), bottom.freq = (freqs - 0.05), top.freq = (freqs + 0.05))

# Make selection_table will all file names, and join with species_freqs

selection_table <- data.frame(sound.files = file_names, selec = (1:length(file_names)), start = 0, end = 20)
selection_table$species <- word(selection_table$sound.files, sep="_", 3)

selection_table_full <- left_join(selection_table, species_freqs, by = "species") 

##### For loop to acquire amplitude mean in the frequency range for each species

output_amps <- c()

for (i in (1:length(file_names))){
  # Read in mp3 file
  tmpwav <- readMP3(paste("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230301/Clipped/", file_names[i], sep=""))
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

analysis_table <- selection_table_full %>%
  filter(species != "CW3", species != "CW1")

colors_dir = c("#BC4749", "#79A356", "#FFB703", "#2A9D8F")
colors_spec = c("#BC4749","#79A356", "#FFB703", "#2A9D8F", "#283618")

analysis_table$dist <- word(analysis_table$sound.files, sep="_", 2)
analysis_table$distance_m <- as.numeric(substr(analysis_table$dist, 1, nchar(analysis_table$dist) - 1))
analysis_table$relative_dir <- substr(analysis_table$dist, nchar(analysis_table$dist), nchar(analysis_table$dist))
analysis_table$TA <- word(analysis_table$sound.files, sep="_", 5)
analysis_table$vol <- word(analysis_table$sound.files, sep="_", 4)
analysis_table$vol <- gsub(".mp3", "", analysis_table$vol)

analysis_table$col_dir <- case_when(analysis_table$relative_dir == "N" ~ colors_dir[1],
                                analysis_table$relative_dir == "S" ~ colors_dir[2],
                                analysis_table$relative_dir == "E" ~ colors_dir[3],
                                analysis_table$relative_dir == "W" ~ colors_dir[4])
analysis_table$col_spec <- case_when(analysis_table$species == "BG" ~ colors_spec[1],
                                     analysis_table$species == "AF" ~ colors_spec[2],
                                     analysis_table$species == "EWP" ~ colors_spec[3],
                                     analysis_table$species == "CW5" ~ colors_spec[4],
                                     analysis_table$species == "YBC" ~ colors_spec[5])

# Toward and Away, this is dirty code that definitely needs to be fixed but for now it works 

analysis_table <- analysis_table %>% mutate(TA = case_when(
  TA == "A.mp3" ~ "A",
  TA != "A.mp3" ~ "T",
  (is.na(TA)) == TRUE ~ "T",
))

analysis_table

# Write out CSV

write.csv(analysis_table,"data/analysis_table.csv")

### End script, create new scripts for AM and MA analysis

# AF analysis

AF <- analysis_table %>%
  filter(species == "AF", vol == "100")

plot(AF$distance_m, log(AF$relative.amp), col=AF$col, pch = 16, cex = 2, 
     ylab = "log Relative amplitude", xlab = "Distance (m)")
legend("topright", legend = c("N", "S", "E", "W"), pch = 16, cex = 2, col = colors[1:4])

AFdistMod = lm(log(relative.amp) ~ distance_m, data = AF)

# Also do CW analysis






#################################################

aftmp <- readMP3("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230301/Clipped/20_50N_AF_100.mp3")

tmp_amps = seewave::spec(afmp3) %>%
  data.frame()

max_amplitude_af = max(af_amps[,2])

freq_at_max_amp_af <- af_amps$x[af_amps$y == max_amplitude_af]

spectro(aftmp, tlim = c(0.6,1.1), flim = c(1,10))
abline(h = freq_at_max_amp_af, col = "red", lwd = 2)

# the maximum value of the second column, which is                                                                              # the highest point on the graph
freq_at_max_amplitude = one_amps[one_amps[,2] == max_amplitude, 1]    # this returns the x value(column 1, which is   frequency) at which the y value (column 2) is equal to the maximum value in column 2 (i.e. the highest amplitude)

# We can check that this is doing what we think. First plot the spectrogram:
seewave::spectro(onemp3)
abline(h = freq_at_max_amplitude, col = "orange")       # draw a horizontal line at the frequency of max amplitude, and see that it goes right through the red portion


spectro_analysis(data[1,], path='../../OneDriveUNC/AudioMoths/ForestAcoustics/20230301/Clipped/')


#### Probably not what we'll use

## Sound Pressure Level (dB)

onemp3 <- readMP3("../../OneDriveUNC/AudioMoths/ForestAcoustics/20230301/Clipped/13_25E_AF_100.mp3")  ## assuming file is in your working directory!

writeWave(onemp3,"onetmp.wav",extensible=FALSE)

selec_table = data.frame(sound.files = 'onetmp.wav', selec = 1, start = 0, end = 100, bottom.freq = 2, top.freq = 8)

output = sound_pressure_level(selec_table, path='temp')

output


data = data.frame(sound.files = file_names, selec = (1:length(file_names)), start = 0, end = 20, bottom.freq = 2, top.freq = 8)

output = sound_pressure_level(data[1,], path='../../OneDriveUNC/AudioMoths/ForestAcoustics/20230301/Clipped/')

