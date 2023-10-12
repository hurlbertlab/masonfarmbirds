#######################################

#####      BirdNET Analysis       #####

#######################################

#load libraries
library(dplyr)
library(tidyverse)
library(beepr)

############################################################
# WREN ANALYSIS

#read in bn output files

bnoutput_files <- list.files("data/BN_results_wren/")

#create output data frame, refresh every time
output <- data.frame()

#for loop to get relevant information from the birdnet output files

for(a in 1:length(bnoutput_files)){
file = read_csv(paste("data/BN_results_wren/", bnoutput_files[a], sep=""))
#get distance and direction phrase from file name
distdir <- word(bnoutput_files[a], sep="_", 2)
#look at last character in disdir
distance <- as.numeric(substr(distdir, 1, nchar(distdir) - 1))
direction <- substr(distdir, nchar(distdir), nchar(distdir))
phrase5 <- word(bnoutput_files[a], sep="_", 5)
if (is.na(phrase5) == FALSE){
  TA <- "A"
}
else{
  TA <- "T"
}
#make sure carolina wren was detected - otherwise confidence = 0
file$Confidence <- as.numeric(file$Confidence)
if ('Carolina Wren' %in% file$`Common name` == FALSE){
  file[nrow(file)+1,4] <- c("Carolina Wren")
  file[nrow(file),5] <- c(0)
}
else{}
  #number of phrases played is indicated in the file name
specphrases <- word(bnoutput_files[a], sep="_", 3)
phrases <- substr(specphrases, 3, 3)
  #pull species detected and confidence from csvs
data <- file[,4:5]
  #pull filename for data management purposes
filename <- substr(bnoutput_files[a], 1, nchar(bnoutput_files[a])-20)
filename
  #compile all information into one dataframe 
data$distance <- distance
data$direction <- direction
data$phrases <- phrases
data$filename <- filename
data$TA <- TA
    #now add the data to out output dataset
    output <- rbind(output, data)
}
  
beep()


# some analysis

CWbnoutputs <- output %>%
  filter(output$direction == "N" & output$TA == "T")


hist(CWbnoutputs$Confidence, main = paste("Histogram of Confidence"), xlab = "Confidence")
table(CWbnoutputs$"Common name")

cw5_output <- CWbnoutputs %>%
  filter(CWbnoutputs$phrases == 5)
cw3_output <- CWbnoutputs %>%
  filter(CWbnoutputs$phrases == 3)
cw1_output <- CWbnoutputs %>%
  filter(CWbnoutputs$phrases == 1)
cwdistMod = lm(Confidence ~ distance, data = CWbnoutputs)
cw1distMod = lm(Confidence ~ distance, data = cw1_output)
cw3distMod = lm(Confidence ~ distance, data = cw3_output)
cw5distMod = lm(Confidence ~ distance, data = cw5_output)

#note, there are only 2 cw3 audios that were processed due to the length being
#less than 3 seconds

par(mfrow=c(2,2))

colors = c("#D81B60", "#1E88E5", "#FFC107", "#00C16C")
plot(CWbnoutputs$distance, CWbnoutputs$Confidence, pch = 16, cex = 2, 
     ylab = "confidence", xlab = "distance", xaxp = c(0, 100, 4))
abline(cw1distMod, lwd = 4, col = colors[1])
abline(cw3distMod, lwd = 4, col = colors[2])
abline(cw5distMod, lwd = 4, col = colors[3])
legend(78.5,1, legend = c("CW 1", "CW 3", "CW 5"), pch = 16, cex = 1.1, col = colors[1:3])

plot(cw1_output$distance, cw1_output$Confidence, pch = 16, cex = 2, 
     ylab = "confidence", xlab = "distance", xaxp = c(0, 100, 4), main = "CW 1, p = 0.4443, R2 = -0.049")
abline(cw1distMod, lwd = 4, col = colors[1])

plot(cw3_output$distance, cw3_output$Confidence, pch = 16, cex = 2, 
     ylab = "confidence", xlab = "distance", xaxp = c(0, 100, 4), main = "CW 3, p = 0.04232*, R2 = 0.514")
abline(cw3distMod, lwd = 4, col = colors[2])

plot(cw5_output$distance, cw5_output$Confidence, pch = 16, cex = 2, 
     ylab = "confidence", xlab = "distance", xaxp = c(0, 100, 4), main = "CW 5, p = 0.00159**, R2 = 0.487")
abline(cw5distMod, lwd = 4, col = colors[3])

## Confidence histograms for different syllables
par(mfrow=c(2,2))
hist(CWbnoutputs$Confidence, main = paste("Histogram of CW Confidence"), xlab = "Confidence")
hist(cw1_output$Confidence, main = paste("CW 1 Confidence"), xlab = "Confidence")
hist(cw3_output$Confidence, main = paste("CW 3 Confidence"), xlab = "Confidence")
hist(cw5_output$Confidence, main = paste("CW 5 Confidence"), xlab = "Confidence")

## Confidence histograms at different distance

par(mfrow=c(2,2))

hist(cw_output$Confidence[cw_output$distance == 25], main = paste("Histogram of CW Confidence 25m"), xlab = "Confidence", ylim = c(0,7), xlim = c(0,1))

hist(cw_output$Confidence[cw_output$distance == 50], main = paste("Histogram of CW Confidence 50m"), xlab = "Confidence", ylim = c(0,7), xlim = c(0,1))

hist(cw_output$Confidence[cw_output$distance == 75], main = paste("Histogram of CW Confidence 75m"), xlab = "Confidence", ylim = c(0,7), xlim = c(0,1))

hist(cw_output$Confidence[cw_output$distance == 100], main = paste("Histogram of CW Confidence 100m"), xlab = "Confidence", ylim = c(0,7), xlim = c(0,1))

############################################################
# FOLIAGE ANALYSIS

#read in bn output files

bnoutput_files <- list.files("../data/BN_results_foliage_raw/")

#create output data frame, refresh every time
output <- data.frame()

#for loop to get relevant information from the birdnet output files

for(a in 1:length(bnoutput_files)){
  file = read_csv(paste("../data/BN_results_foliage_raw/", bnoutput_files[a], sep=""))
  #extract information from file name
  distance <- as.numeric(word(bnoutput_files[a], sep="_", 2))
  foliagelevel <- word(bnoutput_files[a], sep="_", 1)
  species <- substr(word(bnoutput_files[a], sep="_", 3), 1, 2)
  #make sure bird species were detected - otherwise confidence = 0
  file$Confidence <- as.numeric(file$Confidence)
  if (species == "BG"){
    if ('Blue-gray Gnatcatcher' %in% file$`Common name` == FALSE){
      file[nrow(file)+1,4] <- c("Blue-gray Gnatcatcher")
      file[nrow(file),5] <- c(0)
    }
  }
  if (species == "EW"){
    if ('Eastern Wood-Pewee' %in% file$`Common name` == FALSE){
      file[nrow(file)+1,4] <- c("Eastern Wood-Pewee")
      file[nrow(file),5] <- c(0)
      }
  }
  if (species == "YB"){
    if ('Yellow-billed Cuckoo' %in% file$`Common name` == FALSE){
      file[nrow(file)+1,4] <- c("Yellow-billed Cuckoo")
      file[nrow(file),5] <- c(0)
    }
  }
  else{}
  #pull species detected and confidence from csvs
  data <- file[,4:5]
  #pull filename for data management purposes
  filename <- substr(bnoutput_files[a], 1, nchar(bnoutput_files[a])-20)
  filename
  #compile all information into one dataframe 
  data$distance <- distance
  data$foliagelevel <- foliagelevel
  data$filename <- filename
  #now add the data to out output dataset
  output <- rbind(output, data)
}

beep()




output_birds <- output %>%
  filter(output$`Common name` == "Blue-gray Gnatcatcher"  | output$`Common name` == "Eastern Wood-Pewee" | output$`Common name` == "Yellow-billed Cuckoo")
names(output_birds) = c("Species", "confidence", "distance", "foliagelevel", "filename")
output_birds$categories <- paste(output_birds$Species, output_birds$distance, "m", sep=" ")

all_plot <- ggplot(output_birds, aes(x = foliagelevel, y = confidence, fill = categories)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Confidence by Foliage Level",
    x = "Foliage Level",
    y = "Confidence"
  ) +
  scale_fill_manual(values = c("#2085F9", "#61ABFF", "#B2D6FF", "#D00000", "#D04C4C", "#D79A9A", "#FEAD00", "#FCC953", "#FFE8B5")) +
  theme_minimal() +
  theme(legend.position = "top")

output_peewees <- output_birds %>%
  filter(Species == "Eastern Wood-Pewee")

ewp_plot <- ggplot(output_peewees, aes(x = foliagelevel, y = confidence, fill = categories)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "EWP Confidence by Foliage Level",
    x = "Foliage Level",
    y = "Confidence"
  ) +
  scale_fill_manual(values = c("#D00000", "#D04C4C", "#D79A9A")) +
  theme_minimal() +
  theme(legend.position = "top")

output_bg <- output_birds %>%
  filter(Species == "Blue-gray Gnatcatcher")

bg_plot <- ggplot(output_bg, aes(x = foliagelevel, y = confidence, fill = categories)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "BG Confidence by Foliage Level",
    x = "Foliage Level",
    y = "Confidence"
  ) +
  scale_fill_manual(values = c("#2085F9", "#61ABFF", "#B2D6FF")) +
  theme_minimal() +
  theme(legend.position = "top")

output_ybc <- output_birds %>%
  filter(Species == "Yellow-billed Cuckoo")

ybc_plot <- ggplot(output_ybc, aes(x = foliagelevel, y = confidence, fill = categories)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "YBC Confidence by Foliage Level",
    x = "Foliage Level",
    y = "Confidence"
  ) +
  scale_fill_manual(values = c("#FEAD00", "#FCC953", "#FFE8B5")) +
  theme_minimal() +
  theme(legend.position = "top")

all_plot
ewp_plot
bg_plot
ybc_plot
grid.arrange(all_plot, ewp_plot, bg_plot, ybc_plot, ncol = 2)
