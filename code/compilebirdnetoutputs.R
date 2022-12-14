#####################################

#####  Compile BirdNET Outputs  #####

#####################################

# Load libraries
library(gsheet)
library(dplyr)
library(tidyverse)
library(stringr)
library(beepr)

# Read in data

test_bn = read_csv("data/pointcount_birdnetresults/20190612_K11.BirdNET.results.csv")

pointcountURL = 'https://docs.google.com/spreadsheets/d/1TzHlLGW95utQSb62aXR-ksdh01Egmipqc1ypSJYpl6U/edit#gid=23822252'

missedbirdsURL = 'https://docs.google.com/spreadsheets/d/1TzHlLGW95utQSb62aXR-ksdh01Egmipqc1ypSJYpl6U/edit#gid=1227608871'

# Convert google sheets to tables
pointcounts = gsheet2tbl(pointcountURL)

missed = gsheet2tbl(missedbirdsURL)

# Make DF with stake, dates, start times, and end times
pointcounts_DF = pointcounts %>%
  group_by(Date, SurveyID, Stake, ActualStartTime, ActualEndTime, Period) %>%
  count(n_distinct(SurveyID, Stake, ActualStartTime))

pointcounts_DF = pointcounts_DF[,c(1, 2, 3, 4, 5, 6)]
pointcounts_DF$StartTime = NA
pointcounts_DF$EndTime = NA

# For loop to copy start times and end times in seconds
for (i in 1:nrow(pointcounts_DF)) {
  if (pointcounts_DF$Period[i] == 1) {
    pointcounts_DF$StartTime[i] = pointcounts_DF$ActualStartTime[i]
    pointcounts_DF$EndTime[i] = pointcounts_DF$ActualEndTime[i]
  }
  if (pointcounts_DF$Period[i] == 2) {
    pointcounts_DF$StartTime[i] = pointcounts_DF$ActualStartTime[i]
    pointcounts_DF$EndTime[i] = pointcounts_DF$ActualEndTime[i]
  }
  if (pointcounts_DF$Period[i] == 3) {
    pointcounts_DF$StartTime[i] = pointcounts_DF$ActualStartTime[i]
    pointcounts_DF$EndTime[i] = pointcounts_DF$ActualEndTime[i]
  }
  
}

# Add date to pointcounts df
for (i in 1:nrow(pointcounts_DF)) {
  if (pointcounts_DF$Date[i] == "6/12/2019") {
    pointcounts_DF$Date[i] = "20190612"
  }
  if (pointcounts_DF$Date[i] == "6/17/2019") {
    pointcounts_DF$Date[i] = "20190617"
  }
  if (pointcounts_DF$Date[i] == "6/18/2019") {
    pointcounts_DF$Date[i] = "20190618"
  }
  if (pointcounts_DF$Date[i] == "6/19/2019") {
    pointcounts_DF$Date[i] = "20190619"
  }
  if (pointcounts_DF$Date[i] == "6/29/2019") {
    pointcounts_DF$Date[i] = "20190629"
  }
  
  
}

# get rid of original start and end time columns
pointcounts_DF = pointcounts_DF[,c(1, 2, 3, 6, 7, 8)]

# For loop trimming all audiomoth outputs into correct segments

bnoutput_files <- list.files("data/pointcount_birdnetresults/")
date = "blank"
filename = "blank"
stake = "blank"
filtered = "blank"
starts = "blank"
ends = "blank"

# NEED TO RESET DATA FRAME EVERY YOU RUN
output <- data.frame(Start = NULL, End = NULL, Scientific = NULL, Species = NULL, Confidence = NULL, Date = NULL, Stake = NULL, Period = NULL)


# make a column in pointcounts_DF that is filename 
pointcounts_DF$filename <- paste(pointcounts_DF$Date, "_", pointcounts_DF$Stake, ".BirdNET.results.csv", sep = "")

#Filter the birdnet output files to the period start&end times, and add the results to our output dataframe
for(a in 1:length(bnoutput_files)){
  birdnetfilename <- bnoutput_files[a]
  for(periodnum in 1:3) {
    
    relevantinformation <- pointcounts_DF %>% filter(filename == birdnetfilename, Period == periodnum)
    
  date = relevantinformation$Date
  stake = relevantinformation$Stake
  file = read_csv(paste("data/pointcount_birdnetresults/", birdnetfilename, sep=""))
  names(file) = c("Start", "End", "Scientific", "Species", "Confidence")
  filtered <- file %>% 
    filter(Start > relevantinformation$StartTime) %>%
    filter(End < relevantinformation$EndTime) %>%
    mutate(Date = date,
           Stake = stake,
           Period = periodnum)
  
  #now add the filtered data to out output dataset
  output <- rbind(output, filtered)
  }
  
}
beep()


# Write out CSV of compiled birdnet outputs
write.csv(output, paste("data/CompiledBirdNetResults_2019PointCounts.csv", sep = ""))



# Some prelim analysis
hist(output$Confidence, main = paste("Histogram of Confidence"), xlab = "Confidence")
table(output$Species)
output %>% filter(Species == "Eastern Towhee") 
hist(output$Confidence[output$Species == "Swainson's Warbler"])
table(output$Stake[output$Species == "Eastern Towhee"])
