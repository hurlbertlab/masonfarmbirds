################################

#####  Test BirdNET Stuff  #####

################################

# Load libraries
library(gsheet)
library(dplyr)
library(tidyverse)
library(stringr)

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




### Need to write a for loop that looks at birdnet output CSV name: date and stake number
### Using that, it needs to go into a dataframe that has the start and end times (from Pointcounts 
### Google spreadsheet, converted from hours and minutes to seconds)
### Needs to take the start and end time and dplyr filter birdnet CSV 

### Then, we can do a group_by of species listed
### Decide what birds are absurd
### Filter for confidence interval
### Add column that are 1's that indicate detected by BirdNet
### Merge with compare_all dataframe



# For loop trimming all audiomoth outputs into correct segments

bnoutput_files <- list.files("data/pointcount_birdnetresults/")
date = "blank"
filename = "blank"
stake = "blank"
filtered = "blank"
starts = "blank"
ends = "blank"

# NEED TO RESET DATA FRAME EVERY YOU RUN
blankdataframe <- pointcounts_DF[1,] 
blankdataframe$Date <- NA; blankdataframe$SurveyID <- NA; blankdataframe$Stake <- NA; blankdataframe$Period <- NA; blankdataframe$StartTime <- NA; blankdataframe$EndTime <- NA; blankdataframe$BirdnetDetected <- 1;

# work in progress..........
# do we need to read in all of the files or can we write them all to a new
# data frame that houses the birds picked up in each period (group_by), stake
# surveyID, date, etc., so that we can left_join with all_compare





for(a in 1:length(bnoutput_files)){
  filename <- bnoutput_files[a]
  date = str_sub(filename, start = 1, end = 8)
  stake = str_sub(filename, start = 10, end = 12)
  if(substr(stake,3,3) == ".") {
      stake = substr(stake,1,2)
  }
  file = read_csv(paste("data/pointcount_birdnetresults/", filename, sep=""))
  names(file) = c("Start", "End", "Science", "Species", "Confidence")
  for(b in 1:nrow(pointcounts_DF)) {
    for(periodnum in 1:3) {
  if (stake == pointcounts_DF$Stake[b] && date == pointcounts_DF$Date[b] && periodnum == pointcounts_DF$Period[b]){
    starts = pointcounts_DF$StartTime[b]
    ends = pointcounts_DF$EndTime[b]
    filtered = file %>%
      filter(Start > starts) %>%
      filter(End < ends)
    filtered$Date <- date
    filtered$Stake <- stake
    filtered$Period <- periodnum
  }
      blankdataframe <- rbind(blankdataframe, filtered)
    }
  }
  
  
}

table(blankdataframe$Stake, blankdataframe$Date)
