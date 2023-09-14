#######################################

#####  Compile BirdNET Outputs 2  #####

#######################################

#load libraries
library(dplyr)
library(tidyverse)

#read in bn output files

bnoutput_files <- list.files("data/BN_results_wren/")

#create output data frame
output <- data.frame()

#for loop to get relevent information from the birdnet output files\

for(a in 1:length(bnoutput_files)){
file = read_csv(paste("data/BN_results_wren/", bnoutput_files[a], sep=""))
#get distance and direction phrase from file name
distdir <- word(bnoutput_files[a], sep="_", 2)
#look at last character in disdir
last_char <- substr(distdir, nchar(distdir), nchar(distdir))
  #We named the files with the distance, direction, and direction of playback
  #(Toward/Away) in the second phrase; some of the phrases are longer than others
if (last_char == "A"){
  distance <- as.numeric(substr(distdir, 1, nchar(distdir) - 2))
  direction <- substr(distdir, nchar(distdir)-1, nchar(distdir))
}
else{
  distance <- as.numeric(substr(distdir, 1, nchar(distdir) - 1))
  direction <- substr(distdir, nchar(distdir), nchar(distdir))
}
  #number of phrases played is indicated in the file name
specphrases <- word(bnoutput_files[a], sep="_", 3)
phrases <- substr(specphrases, 3, 3)
  #pull species detected and confidence from csvs
data <- file %>%
  select(4,5)
  #pull filename for data management purposes
filename <- substr(bnoutput_files[a], 1, nchar(bnoutput_files[a])-24)
filename
  #compile all information into one dataframe 
data$distance <- distance
data$direction <- direction
data$phrases <- phrases
data$filename <- filename
    #now add the data to out output dataset
    output <- rbind(output, data)
}
  
beep()

#some analysis

hist(output$Confidence, main = paste("Histogram of Confidence"), xlab = "Confidence")
table(output$"Common name")
hist(output$Confidence[output$"Common name" == "Carolina Wren"], main = paste("Histogram of CW Confidence"), xlab = "Confidence")

cw_output <- output %>%
  filter(output$`Common name` == "Carolina Wren")
cw5_output <- cw_output %>%
  filter(cw_output$phrases == 5)
cw3_output <- cw_output %>%
  filter(cw_output$phrases == 3)
cwdistMod = lm(Confidence ~ distance, data = cw_output)
cw3distMod = lm(Confidence ~ distance, data = cw3_output)
cw5distMod = lm(Confidence ~ distance, data = cw5_output)
distMod = lm(Confidence ~ distance, data = output)

#note, there are only 2 cw3 audios that were processed due to the legnth being
#less than 3 seconds

colors = c("#D81B60", "#1E88E5", "#FFC107", "#00C16C")
plot(output$distance, output$Confidence, pch = 16, cex = 2, 
     ylab = "confidence", xlab = "distance", xaxp = c(0, 100, 4))
abline(cw3distMod, lwd = 4, col = colors[1])
abline(cw5distMod, lwd = 4, col = colors[2])
abline(cwdistMod, lwd = 4, col = colors[3])
abline(distMod, lwd = 4, col = colors[4])
legend(78.5,1, legend = c("CW 3", "CW 5", "All Carolina Wren", "All Species"), pch = 16, cex = 1.3, col = colors[1:4])

