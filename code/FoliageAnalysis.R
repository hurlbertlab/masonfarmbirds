######################################
#### Foliage Analysis             ####
#### Last updated: 11/30/2023     ####
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

############### Find Mean Relative Amp

relativeAmp = function(path, frequency, threshold) {
  # Pull all file names
  # Create DF with all amp values
  sp <- word(word(paste(path), 5, sep="/"), 1, sep = "_")
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

############### Find confidence of BirdNET results for each species

abrev <- c("BG", "YBC", "MD", "AF", "CW", "EWP")
commonname <- c("Blue-gray Gnatcatcher", "Yellow-billed Cuckoo", "Mourning Dove", 
                "Acadian Flycatcher", "Carolina Wren", "Eastern Wood-Pewee")
species_list <- data.frame(abrev, commonname)

bnConfidence = function(file_path, species) {
  
  # read in path
  BNoutputfiles <- list.files(file_path)
  
  # For loop to gather information from BN output files
  output <- data.frame(Foliage = NULL, Rep = NULL, Distance = NULL, Confidence = NULL)
  
  for(a in 1:length(BNoutputfiles)){
    file = read_csv(paste(file_path, BNoutputfiles[a], sep=""), show_col_types = FALSE)
    # BirdNET trims calls into 3 second segments - to avoid very low confidence
    # that is associated with the end of a call, only take the first 3 seconds
    # for all calls except mourning dove, this is not an issue
    file_trimmed <- file %>%
      filter(file$'Start (s)' == 0)
    #get foliage, rep, and distance from file name
    foliage <- word(BNoutputfiles[a], sep="_", 1)
    rep <- word(BNoutputfiles[a], sep="_", 2)
    distance <- as.numeric(word(BNoutputfiles[a], sep="_", 3))
    file_name <- BNoutputfiles[a]
    file_spec <- substr(BNoutputfiles[a], 8, nchar(BNoutputfiles[a])-20)
    
    file_trimmed$Confidence <- as.numeric(file_trimmed$Confidence) 
    if(species == file_spec){
      common_name <- species_list$commonname[which(species == species_list$abrev)]
      if (common_name %in% file_trimmed$`Common name` == FALSE){
        file_trimmed[nrow(file_trimmed)+1,4] <- c(common_name)
        file_trimmed[nrow(file_trimmed),5] <- c(0)
        
      }
      # trim file down to relevant data and add other relevant information
      # compile all information into one dataframe 
      data <- file_trimmed[,4:5]
      data$Foliage <- foliage
      data$Rep <- rep
      data$Distance <- distance
      data$Filename <- file_name
      
      #now add the data to out output dataset
      output <- rbind(output, data)
    }
    else{}
}
  return(output)
}


############### Start analysis

##### Find amps for each species

BGfrequency <- 6.8750
BGpath <- "data/audiofiles/20231105_foliageanalysis/Concatenated/BG_123.wav"
BG_output <- relativeAmp(BGpath, BGfrequency, -30)
BG_output$relaamps <- (max(BG_output$Amp, na.rm = TRUE) / BG_output$Amp) * 100

YBCfrequency <- 1.0625
YBCpath <- "data/audiofiles/20231105_foliageanalysis/Concatenated/YBC_123.wav"
YBC_output <- relativeAmp(YBCpath, YBCfrequency, -28)
YBC_output$relaamps <- (max(YBC_output$Amp, na.rm = TRUE) / YBC_output$Amp) * 100

### at 2/3/50 there is NaN, because they were completely inaudible
MDfrequency <- 0.4375 
MDpath <- "data/audiofiles/20231105_foliageanalysis/Concatenated/MD_123.wav"
MD_output <- relativeAmp(MDpath, MDfrequency, -30)
MD_output$relaamps <- (max(MD_output$Amp, na.rm = TRUE) / MD_output$Amp) * 100

### at 2/3/50 there is NaN, because they were completely inaudible
AFfrequency <- 4.1875
AFpath <- "data/audiofiles/20231105_foliageanalysis/Concatenated/AF_123.wav"
AF_output <- relativeAmp(AFpath, AFfrequency, -30)
AF_output$relaamps <- (max(AF_output$Amp, na.rm = TRUE) / AF_output$Amp) * 100

CWfrequency <- 3.8750
CWpath <- "data/audiofiles/20231105_foliageanalysis/Concatenated/CW_123.wav"
CW_output <- relativeAmp(CWpath, CWfrequency, -24)
CW_output$relaamps <- (max(CW_output$Amp, na.rm = TRUE) / CW_output$Amp) * 100

EWPfrequency <- 4.1250
EWPpath <- "data/audiofiles/20231105_foliageanalysis/Concatenated/EWP_123.wav"
EWP_output <- relativeAmp(EWPpath, EWPfrequency, -24)
EWP_output$relaamps <- (max(EWP_output$Amp, na.rm = TRUE) / EWP_output$Amp) * 100

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

par(mfrow=c(2,3), mar = c(5, 5, 3, 1))

## plots of different species

# plot BG
plot(BG_output$Distance, BG_output$relaamp, col="white",lty="dotted", pch = 16, cex = 1.7,
     ylab = "Relative Amplitude (%)", xlab = "Distance (m)", xaxp = c(0, 50, 2), ylim = c(60,100), yaxt = "n", main="(A) BG", cex.axis = 1.5, cex.lab = 1.8, cex.main = 2)
axis(c(100,10,1,0.1, 0.01), side = 2, at = c(100, 90, 80, 70, 60), las = 1, cex.axis = 1.25)
points(jitter(BG_output[BG_output$Foliage == 1,]$Distance, .1), BG_output[BG_output$Foliage == 1,]$relaamps, col = "#A6D1FF", pch = 17, cex = 2.3)
points(jitter(BG_output[BG_output$Foliage == 2,]$Distance, .1), BG_output[BG_output$Foliage == 2,]$relaamps, col = "#4488D2", pch = 18, cex = 2.3)
points(jitter(BG_output[BG_output$Foliage == 3,]$Distance, .1), BG_output[BG_output$Foliage == 3,]$relaamps, col = "#003975", pch = 19, cex = 2.3)
abline(BG_lm1, lwd = 4, col = "#A6D1FF")
abline(BG_lm2, lwd = 4, col = "#4488D2")
abline(BG_lm3, lwd = 4, col = "#003975")
legend("topright", legend = c("Low Foliage", "Medium Foliage", "High Foliage"), pch = c(17, 18, 19), cex = 1.6, col = c("#A6D1FF", "#4488D2", "#003975"))

# plot ewp
plot(EWP_output$Distance, EWP_output$relaamp, col="white",lty="dotted", pch = 16, cex = 1.7, yaxt = "n", ylab = "Relative Amplitude (%)", xlab = "Distance (m)", xaxp = c(0, 50, 2), ylim = c(60,100), main="(C) EWP", cex.axis = 1.5, cex.lab = 1.8, cex.main = 2)
axis(c(100,10,1,0.1, 0.01), side = 2, at = c(100, 90, 80, 70, 60), las = 1, cex.axis = 1.25)
points(jitter(EWP_output[EWP_output$Foliage == 1,]$Distance, .1), EWP_output[EWP_output$Foliage == 1,]$relaamps, col = "#FFBED9", pch = 17, cex = 2.3)
points(jitter(EWP_output[EWP_output$Foliage == 2,]$Distance, .1), EWP_output[EWP_output$Foliage == 2,]$relaamps, col = "#FD569C", pch = 18, cex = 2.3)
points(jitter(EWP_output[EWP_output$Foliage == 3,]$Distance, .1), EWP_output[EWP_output$Foliage == 3,]$relaamps, col = "#870C3F", pch = 19, cex = 2.3)
abline(EWP_lm1, lwd = 4, col = "#FFBED9")
abline(EWP_lm2, lwd = 4, col = "#FD569C")
abline(EWP_lm3, lwd = 4, col = "#870C3F")
legend("topright", legend = c("Low Foliage", "Medium Foliage", "High Foliage"), pch = c(17, 18, 19), cex  = 1.6, col = c("#FFBED9", "#FD569C", "#870C3F"))

# plot af
plot(AF_output$Distance, AF_output$relaamp, col="white",lty="dotted", pch = 16, cex = 1.7, yaxt = "n",
     ylab = "Relative Amplitude (%)", xlab = "Distance (m)", xaxp = c(0, 50, 2), ylim = c(60,100), main="(B) AF", cex.axis = 1.5, cex.lab = 1.8, cex.main = 2)
axis(c(100,10,1,0.1, 0.01), side = 2, at = c(100, 90, 80, 70, 60), las = 1, cex.axis = 1.25)
points(jitter(AF_output[AF_output$Foliage == 1,]$Distance, .1), AF_output[AF_output$Foliage == 1,]$relaamps, col = "#FFA555", pch = 17, cex = 2.3)
points(jitter(AF_output[AF_output$Foliage == 2,]$Distance, .1), AF_output[AF_output$Foliage == 2,]$relaamps, col = "#FF7700", pch = 18, cex = 2.3)
points(jitter(AF_output[AF_output$Foliage == 3,]$Distance, .1), AF_output[AF_output$Foliage == 3,]$relaamps, col = "#8F4300", pch = 19, cex = 2.3)
abline(AF_lm1, lwd = 4, col = "#FFA555")
abline(AF_lm2, lwd = 4, col = "#FF7700")
abline(AF_lm3, lwd = 4, col = "#8F4300")
legend("bottomleft", legend = c("Low Foliage", "Medium Foliage", "High Foliage"), pch = c(17, 18, 19), cex  = 1.6, col = c("#FFA555", "#FF7700", "#8F4300"))

# plot cw
plot(CW_output$Distance, CW_output$relaamp, col="white",lty="dotted", pch = 16, cex = 1.7, yaxt = "n",
     ylab = "Relative Amplitude (%)", xlab = "Distance (m)", xaxp = c(0, 50, 2), ylim = c(60,100), main="(D) CW", cex.axis = 1.5, cex.lab = 1.8, cex.main = 2)
axis(c(100,10,1,0.1, 0.01), side = 2, at = c(100, 90, 80, 70, 60), las = 1, cex.axis = 1.25)
points(jitter(CW_output[CW_output$Foliage == 1,]$Distance, .1), CW_output[CW_output$Foliage == 1,]$relaamps, col = "#7CFFC6", pch = 17, cex = 2.3)
points(jitter(CW_output[CW_output$Foliage == 2,]$Distance, .1), CW_output[CW_output$Foliage == 2,]$relaamps, col = "#0ACE79", pch = 18, cex = 2.3)
points(jitter(CW_output[CW_output$Foliage == 3,]$Distance, .1), CW_output[CW_output$Foliage == 3,]$relaamps, col = "#00693B", pch = 19, cex = 2.3)
abline(CW_lm1, lwd = 4, col = "#7CFFC6")
abline(CW_lm2, lwd = 4, col = "#0ACE79")
abline(CW_lm3, lwd = 4, col = "#00693B")
legend("bottomleft", legend = c("Low Foliage", "Medium Foliage", "High Foliage"), pch = c(17, 18, 19), cex  = 1.6, col = c("#7CFFC6", "#0ACE79", "#00693B"))

# plot ybc
plot(YBC_output$Distance, YBC_output$relaamp, col="white",lty="dotted", pch = 16, cex = 1.7,
     ylab = "Relative Amplitude (%)", xlab = "Distance (m)", xaxp = c(0, 50, 2), ylim = c(60,100), yaxt = "n", main="(E) YBC", cex.axis = 1.8, cex.lab = 1.8, cex.main = 2)
axis(c(100,10,1,0.1, 0.01), side = 2, at = c(100, 90, 80, 70, 60), las = 1, cex.axis = 1.25)
points(jitter(YBC_output[YBC_output$Foliage == 1,]$Distance, .1), YBC_output[YBC_output$Foliage == 1,]$relaamps, col = "#FFDE84", pch = 17, cex = 2.3)
points(jitter(YBC_output[YBC_output$Foliage == 2,]$Distance, .1), YBC_output[YBC_output$Foliage == 2,]$relaamps, col = "#F0B924", pch = 18, cex = 2.3)
points(jitter(YBC_output[YBC_output$Foliage == 3,]$Distance, .1), YBC_output[YBC_output$Foliage == 3,]$relaamps, col = "#9D7300", pch = 19, cex = 2.3)
abline(YBC_lm1, lwd = 4, col = "#FFDE84")
abline(YBC_lm2, lwd = 4, col = "#F0B924")
abline(YBC_lm3, lwd = 4, col = "#9D7300")
legend("bottomleft", legend = c("Low Foliage", "Medium Foliage", "High Foliage"), pch = c(17, 18, 19), cex  = 1.6, col = c("#FFDE84", "#F0B924", "#9D7300"))

# plot md
plot(MD_output$Distance, MD_output$relaamp, col="white",lty="dotted", pch = 16, cex = 1.7, yaxt = "n",
     ylab = "Relative Amplitude (%)", xlab = "Distance (m)", xaxp = c(0, 50, 2), ylim = c(60,100), main="(F) MD", cex.axis = 1.5, cex.lab = 1.8, cex.main = 2)
axis(c(100,10,1,0.1, 0.01), side = 2, at = c(100, 90, 80, 70, 60), las = 1, cex.axis = 1.25)
points(jitter(MD_output[MD_output$Foliage == 1,]$Distance, .1), MD_output[MD_output$Foliage == 1,]$relaamps, col = "#D58DFF", pch = 17, cex = 2.3)
points(jitter(MD_output[MD_output$Foliage == 2,]$Distance, .1), MD_output[MD_output$Foliage == 2,]$relaamps, col = "#9715E3", pch = 18, cex = 2.3)
points(jitter(MD_output[MD_output$Foliage == 3,]$Distance, .1), MD_output[MD_output$Foliage == 3,]$relaamps, col = "#4C0078", pch = 19, cex = 2.3)
abline(MD_lm1, lwd = 4, col = "#D58DFF")
abline(MD_lm2, lwd = 4, col = "#9715E3")
abline(MD_lm3, lwd = 4, col = "#4C0078")
legend("bottomleft", legend = c("Low Foliage", "Medium Foliage", "High Foliage"), pch = c(17, 18, 19), cex  = 1.6, col = c("#D58DFF", "#9715E3", "#4C0078"))

## plots of different foliage levels 

par(mfrow=c(1,3), mar = c(5, 5, 3, 1))

# foliage level 1
species <- c("BG", "AF", "EWP", "CW", "YBC", "MD")
freqs <- c(6.88, 5.27, 4.21, 3.00, 1.7, 0.50)
Slope_foliage1 <- c(-0.6236, -0.4299, -0.5401, -0.1381, -0.1757, -0.1749)
fol1 <- data.frame(species, freqs, Slope_foliage1)
fol1lm <- lm(Slope_foliage1 ~ freqs, data=fol1)

plot(fol1$freqs, fol1$Slope_foliage1, col=c("#4488D2", "#FF7700", "#FD569C", "#0ACE79","#F0B924", "#9715E3"),lty="dotted", pch = 16, cex = 3, ylab = "Slope of Relative Amp vs Distance", xlab = "Freq (kHz)", main="Foliage 1 (Low)",cex.axis = 1.5, cex.lab = 1.8, cex.main = 1.9, ylim = c(-1.2,0))
abline(fol1lm, lwd = 4, col = "black")
legend("bottomleft", legend = c("BG: 6.88 kHz", "AF: 5.27 kHZ", "EWP: 4.21 kHZ", "CW: 3.00 kHz", "YBC: 1.70 kHz", "MD: 0.50 kHz"), pch = 16, cex = 1.4, col=c("#4488D2", "#FF7700", "#FD569C", "#0ACE79","#F0B924", "#9715E3"))
legend("topright", legend = c("p = 0.025, R2 = 0.75"), cex = 1.4)

# foliage level 2
species <- c("BG", "AF", "EWP", "CW", "YBC", "MD")
freqs <- c(6.88, 5.27, 4.21, 3.00, 1.7, 0.50)
Slope_foliage2 <- c(-0.2826, -0.5636, -0.7861, -0.2312, -0.1812, -0.2703)
fol2 <- data.frame(species, freqs, Slope_foliage2)
fol2lm <- lm(Slope_foliage2 ~ freqs, data=fol2)

plot(fol2$freqs, fol2$Slope_foliage2, col=c("#4488D2", "#FF7700", "#FD569C", "#0ACE79","#F0B924", "#9715E3" ),lty="dotted", pch = 16, cex = 3, ylab = "Slope of Relative Amp vs Distance", xlab = "Freq (kHz)",  main="Foliage 2 (Medium)", cex.axis = 1.5, cex.lab = 1.8, cex.main = 1.9, ylim = c(-1.2,0))
abline(fol2lm, lwd = 4, col = "black")
legend("bottomleft", legend = c("BG: 6.88 kHz", "AF: 5.27 kHZ", "EWP: 4.21 kHZ", "CW: 3.00 kHz", "YBC: 1.70 kHz", "MD: 0.50 kHz"), pch = 16, cex = 1.4, col = c("#4488D2", "#FF7700", "#FD569C", "#0ACE79","#F0B924", "#9715E3" ))
legend("topright", legend = c("p = 0.46, R2 = 0.14"), cex = 1.4)

# foliage level 3
species <- c("BG", "AF", "EWP", "CW", "YBC", "MD")
freqs <- c(6.88, 5.27, 4.21, 3.00, 1.7, 0.50)
Slope_foliage3 <- c(-0.5708, -0.6096, -0.9931, -1.052, -0.2027, -0.4556)
fol3 <- data.frame(species, freqs, Slope_foliage3)
fol3lm <- lm(Slope_foliage3 ~ freqs, data=fol3)

plot(fol3$freqs, fol3$Slope_foliage3, col=c("#4488D2", "#FF7700", "#FD569C", "#0ACE79","#F0B924", "#9715E3"),lty="dotted", pch = 16, cex = 3, ylab = "Slope of Relative Amp vs Distance", xlab = "Freq (kHz)",  main="Foliage 3 (High)", cex.axis = 1.5, cex.lab = 1.8, cex.main = 1.9, ylim = c(-1.2,0))
abline(fol3lm, lwd = 4, col = "black")
legend("bottomleft", legend = c("BG: 6.88 kHz", "AF: 5.27 kHZ", "EWP: 4.21 kHZ", "CW: 3.00 kHz", "YBC: 1.70 kHz", "MD: 0.50 kHz"), pch = 16, cex = 1.4, col = c("#4488D2", "#FF7700", "#FD569C", "#0ACE79","#F0B924", "#9715E3"))
legend("topright", legend = c("p = 0.580, R2 = 0.08"), cex = 1.4)

#########################################################################

##### BirdNET Analysis

BNpath <- "data/BN_results_foliage_20231105/"

BGconfidence <- bnConfidence(BNpath, "BG")
AFconfidence <- bnConfidence(BNpath, "AF")
EWPconfidence <- bnConfidence(BNpath, "EWP")
CWconfidence <- bnConfidence(BNpath, "CW")
YBCconfidence <- bnConfidence(BNpath, "YBC")
MDconfidence <- bnConfidence(BNpath, "MD")

# Join confidence with individual species outputs
# Select for only the species we are looking for, disregard other species 
# that BirdNET picked up for now

BG_conf <- BGconfidence %>%
  filter(BGconfidence$`Common name` == "Blue-gray Gnatcatcher")

AF_conf <- AFconfidence %>%
  filter(AFconfidence$`Common name` == "Acadian Flycatcher")

EWP_conf <- EWPconfidence %>%
  filter(EWPconfidence$`Common name` == "Eastern Wood-Pewee")

CW_conf <- CWconfidence %>%
  filter(CWconfidence$`Common name` == "Carolina Wren")

YBC_conf <- YBCconfidence %>%
  filter(YBCconfidence$`Common name` == "Yellow-billed Cuckoo")

MD_conf <- MDconfidence %>%
  filter(MDconfidence$`Common name` == "Mourning Dove")

BG_confamp <- merge(BG_output, BG_conf, by=c("Foliage","Rep", "Distance"))
AF_confamp <- merge(AF_output, AF_conf, by=c("Foliage","Rep", "Distance"))
EWP_confamp <- merge(EWP_output, EWP_conf, by=c("Foliage","Rep", "Distance"))
CW_confamp <- merge(CW_output, CW_conf, by=c("Foliage","Rep", "Distance"))
YBC_confamp <- merge(YBC_output, YBC_conf, by=c("Foliage","Rep", "Distance"))
MD_confamp <- merge(MD_output, MD_conf, by=c("Foliage","Rep", "Distance"))

BG_confamplm <- lm(Confidence ~ relaamps, data = BG_confamp)
AF_confamplm <- lm(Confidence ~ relaamps, data = AF_confamp)
EWP_confamplm <- lm(Confidence ~ relaamps, data = EWP_confamp)
CW_confamplm <- lm(Confidence ~ relaamps, data = CW_confamp)
YBC_confamplm <- lm(Confidence ~ relaamps, data = YBC_confamp)
MD_confamplm <- lm(Confidence ~ relaamps, data = MD_confamp)

### Plots

par(mfrow=c(2,3))

# plot BG all foliage


plot(BG_confamp$relaamps, BG_confamp$Confidence, col="white",lty="dotted", pch = 16, cex = 1.7,
     ylab = "Confidence", xlab = "Relative Amplitude (%)", main="BG Relative Amp v Confidence", ylim = c(0, 1), xlim = c(60,100), cex.main = 1.9, cex.axis = 1.7, cex.lab = 2) 
points(jitter(BG_confamp[BG_confamp$Foliage == 1,]$relaamps, .1), BG_confamp[BG_confamp$Foliage == 1,]$Confidence, col = "#A6D1FF", pch = 17, cex = 2.7)
points(jitter(BG_confamp[BG_confamp$Foliage == 2,]$relaamps, .1), BG_confamp[BG_confamp$Foliage == 2,]$Confidence, col = "#4488D2", pch = 18, cex = 2.7)
points(jitter(BG_confamp[BG_confamp$Foliage == 3,]$relaamps, .1), BG_confamp[BG_confamp$Foliage == 3,]$Confidence, col = "#003975", pch = 19, cex = 2.7)
abline(BG_confamplm, lwd = 4, col = "black")
legend("topleft", legend = c("p = 0.07, R2 = 0.19"), cex = 1.6)

# plot AF all foliage

plot(AF_confamp$relaamps,  AF_confamp$Confidence, col="white",lty="dotted", pch = 16, cex = 1.7,
     xlab = "Relative Amplitude (%)", ylab = "Confidence", main="AF Relative Amp v Confidence", ylim = c(0, 1), xlim = c(60,100), cex.main = 1.9, cex.axis = 1.7, cex.lab = 2)
points(jitter(AF_confamp[AF_confamp$Foliage == 1,]$relaamps, .1), AF_confamp[AF_confamp$Foliage == 1,]$Confidence, col = "#FFA555", pch = 17, cex = 2.7)
points(jitter(AF_confamp[AF_confamp$Foliage == 2,]$relaamps, .1), AF_confamp[AF_confamp$Foliage == 2,]$Confidence, col = "#FF7700", pch = 18, cex = 2.7)
points(jitter(AF_confamp[AF_confamp$Foliage == 3,]$relaamps, .1), AF_confamp[AF_confamp$Foliage == 3,]$Confidence, col = "#8F4300", pch = 19, cex = 2.7)
abline(AF_confamplm, lwd = 4, col = "black")
legend("bottomleft", legend = c("Foliage1", "Foliage2", "Foliage3"), pch = c(17, 18, 19), cex = 1.8, col = c("#9A9A9A","#5F5F5F", "black"))
legend("topleft", legend = c("p = 0.27, R2 = 0.07"), cex = 1.6)

# plot EWP all foliage

plot(EWP_confamp$relaamps,  EWP_confamp$Confidence, col="white",lty="dotted", pch = 16, cex = 1.7,
     ylab = "Confidence", xlab = "Relative Amplitude (%)", main="EWP Relative Amp v Confidence", ylim = c(0, 1), xlim = c(60,100), cex.main = 1.9, cex.axis = 1.7, cex.lab = 2)
points(jitter(EWP_confamp[EWP_confamp$Foliage == 1,]$relaamps, .1), EWP_confamp[EWP_confamp$Foliage == 1,]$Confidence, col = "#FFBED9", pch = 17, cex = 2.7)
points(jitter(EWP_confamp[EWP_confamp$Foliage == 2,]$relaamps, .1), EWP_confamp[EWP_confamp$Foliage == 2,]$Confidence, col = "#FD569C", pch = 18, cex = 2.7)
points(jitter(EWP_confamp[EWP_confamp$Foliage == 3,]$relaamps, .1), EWP_confamp[EWP_confamp$Foliage == 3,]$Confidence, col = "#870C3F", pch = 19, cex = 2.7)
abline(EWP_confamplm, lwd = 4, col = "black")
legend("topleft", legend = c("p = 0.06, R2 = 0.19"), cex = 1.6)

# plot CW all foliage

plot(CW_confamp$relaamps,  CW_confamp$Confidence, col="white",lty="dotted", pch = 16, cex = 1.7,
     ylab = "Confidence", xlab = "Relative Amplitude (%)", main="CW Relative Amp v Confidence", ylim = c(0, 1), xlim = c(60,100), cex.main = 1.9, cex.axis = 1.7, cex.lab = 2)
points(jitter(CW_confamp[CW_confamp$Foliage == 1,]$relaamps, .1), CW_confamp[CW_confamp$Foliage == 1,]$Confidence, col = "#7CFFC6", pch = 17, cex = 2.7)
points(jitter(CW_confamp[CW_confamp$Foliage == 2,]$relaamps, .1), CW_confamp[CW_confamp$Foliage == 2,]$Confidence, col = "#0ACE79", pch = 18, cex = 2.7)
points(jitter(CW_confamp[CW_confamp$Foliage == 3,]$relaamps, .1), CW_confamp[CW_confamp$Foliage == 3,]$Confidence, col = "#00693B", pch = 19, cex = 2.7)
abline(CW_confamplm, lwd = 4, col = "black")
legend("topleft", legend = c("p = 0.51, R2 = 0.02"), cex = 1.6)

# plot YBC all foliage

plot(YBC_confamp$relaamps,  YBC_confamp$Confidence, col="white",lty="dotted", pch = 16, cex = 1.7,
     ylab = "Confidence", xlab = "Relative Amplitude (%)", main="YBC Relative Amp v Confidence", ylim = c(0, 1), xlim = c(60,100), cex.main = 1.9, cex.axis = 1.7, cex.lab = 2)
points(jitter(YBC_confamp[YBC_confamp$Foliage == 1,]$relaamps, .1), YBC_confamp[YBC_confamp$Foliage == 1,]$Confidence, col = "#FFDE84", pch = 17, cex = 2.7)
points(jitter(YBC_confamp[YBC_confamp$Foliage == 2,]$relaamps, .1), YBC_confamp[YBC_confamp$Foliage == 2,]$Confidence, col = "#F0B924", pch = 18, cex = 2.7)
points(jitter(YBC_confamp[YBC_confamp$Foliage == 3,]$relaamps, .1), YBC_confamp[YBC_confamp$Foliage == 3,]$Confidence, col = "#9D7300", pch = 19, cex = 2.7)
abline(YBC_confamplm, lwd = 4, col = "black")
legend("topleft", legend = c("p = 0.48, R2 = 0.03"), cex = 1.6)

# plot MD all foliage

plot(MD_confamp$relaamps,  MD_confamp$Confidence, col="white",lty="dotted", pch = 16, cex = 1.7,
     ylab = "Confidence", xlab = "Relative Amplitude (%)", main="MD Relative Amp v Confidence", ylim = c(0, 1), xlim = c(60,100), cex.main = 1.9, cex.axis = 1.7, cex.lab = 2)
points(jitter(MD_confamp[MD_confamp$Foliage == 1,]$relaamps, .1), MD_confamp[MD_confamp$Foliage == 1,]$Confidence, col = "#D58DFF", pch = 17, cex = 2.7)
points(jitter(MD_confamp[MD_confamp$Foliage == 2,]$relaamps, .1), MD_confamp[MD_confamp$Foliage == 2,]$Confidence, col = "#9715E3", pch = 18, cex = 2.7)
points(jitter(MD_confamp[MD_confamp$Foliage == 3,]$relaamps, .1), MD_confamp[MD_confamp$Foliage == 3,]$Confidence, col = "#4C0078", pch = 19, cex = 2.7)
abline(MD_confamplm, lwd = 4, col = "black")
legend("topleft", legend = c("p = 0.0007, R2 = 0.54"), cex = 1.6)

# Combined
all_confamp <- rbind(BG_confamp, AF_confamp, EWP_confamp, CW_confamp, YBC_confamp, MD_confamp)
all_confampplm <- lm(Confidence ~ relaamps, data = all_confamp)

par(mfrow=c(1,1))
plot(all_confamp$relaamps,  all_confamp$Confidence, col="white",lty="dotted", pch = 16, cex = 1.7,
     ylab = "BirdNET Confidence", xlab = "Relative Amplitude (%)", main="Relative Amp v Confidence", ylim = c(0, 1), xlim = c(60,100), cex.main = 1.9, cex.axis = 1.7, cex.lab = 2)
points(BG_confamp$relaamps, BG_confamp$Confidence, col = "#4488D2", pch = 16, cex = 1.5)
points(AF_confamp$relaamps, AF_confamp$Confidence, col = "#FF7700", pch = 16, cex = 1.5)
points(EWP_confamp$relaamps, EWP_confamp$Confidence, col = "#FD569C", pch = 16, cex = 1.5)
points(CW_confamp$relaamps, CW_confamp$Confidence, col = "#0ACE79", pch = 16, cex = 1.5)
points(YBC_confamp$relaamps, YBC_confamp$Confidence, col = "#F0B924", pch = 16, cex = 1.5)
points(MD_confamp$relaamps, MD_confamp$Confidence, col = "#9715E3", pch = 16, cex = 1.5)
abline(all_confampplm, lwd = 4, col = "black")
legend("topleft", legend = c("p = 0.006, R2 = 0.06"), cex = 1.5)
legend(58.45,0.916, legend = c("BG", "AF", "EWP", "CW", "YBC", "MD"), pch = 16, cex = 1.2, col = c("#4488D2", "#FF7700", "#FD569C", "#0ACE79","#F0B924", "#9715E3"))
#MD only
plot(all_confamp$relaamps,  all_confamp$Confidence, col="white",lty="dotted", pch = 16, cex = 1.7,
     ylab = "BirdNET Confidence", xlab = "Relative Amplitude (%)", main="Relative Amp v Confidence", ylim = c(0, 1), xlim = c(60,100), cex.main = 1.9, cex.axis = 1.7, cex.lab = 2)
points(MD_confamp$relaamps, MD_confamp$Confidence, col = "#9715E3", pch = 16, cex = 1.5)
abline(all_confampplm, lwd = 4, col = "black")
legend("topleft", legend = c("p = 0.006, R2 = 0.06"), cex = 1.5)
legend(58.45,0.916, legend = c("BG", "AF", "EWP", "CW", "YBC", "MD"), pch = 16, cex = 1.2, col = c("#4488D2", "#FF7700", "#FD569C", "#0ACE79","#F0B924", "#9715E3"))
abline(MD_confamplm, lwd = 4, col = "#9715E3")
legend("bottomleft", legend = c("p = 0.0007, R2 = 0.54"), pch = 16, cex = 1.5, col= c("#9715E3") )
#CW only
plot(all_confamp$relaamps,  all_confamp$Confidence, col="white",lty="dotted", pch = 16, cex = 1.7,
     ylab = "BirdNET Confidence", xlab = "Relative Amplitude (%)", main="Relative Amp v Confidence", ylim = c(0, 1), xlim = c(60,100), cex.main = 1.9, cex.axis = 1.7, cex.lab = 2)
points(CW_confamp$relaamps, CW_confamp$Confidence, col = "#0ACE79", pch = 16, cex = 1.5)
abline(all_confampplm, lwd = 4, col = "black")
legend("topleft", legend = c("p = 0.006, R2 = 0.06"), cex = 1.5)
legend(58.45,0.916, legend = c("BG", "AF", "EWP", "CW", "YBC", "MD"), pch = 16, cex = 1.2, col = c("#4488D2", "#FF7700", "#FD569C", "#0ACE79","#F0B924", "#9715E3"))
abline(CW_confamplm, lwd = 4, col = "#0ACE79")
legend("bottomleft", legend = c("p = 0.51, R2 = 0.02"), pch = 16, cex = 1.5, col= c("#0ACE79") )

##############
## Linear models
## amp ~ foliage + distance + foliage * distance
## by species

# BG

BGAmpFolLinModel <- lm(Amp ~ Distance + Foliage + Distance*Foliage, data = BG_output)
summary(BGAmpFolLinModel)

# AF
AFAmpFolLinModel <- lm(Amp ~ Distance + Foliage + Distance*Foliage, data = AF_output)
summary(AFAmpFolLinModel)

# EWP
EWPAmpFolLinModel <- lm(Amp ~ Distance + Foliage + Distance*Foliage, data = EWP_output)
summary(EWPAmpFolLinModel)

# CW
CWAmpFolLinModel <- lm(Amp ~ Distance + Foliage + Distance*Foliage, data = CW_output)
summary(CWAmpFolLinModel)

# YBC
YBCAmpFolLinModel <- lm(Amp ~ Distance + Foliage + Distance*Foliage, data = YBC_output)
summary(YBCAmpFolLinModel)

# MD
MDAmpFolLinModel <- lm(Amp ~ Distance + Foliage + Distance*Foliage, data = MD_output)
summary(MDAmpFolLinModel)
