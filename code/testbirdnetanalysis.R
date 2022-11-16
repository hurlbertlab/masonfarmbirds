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
  group_by(SurveyID, Stake, ActualStartTime, ActualEndTime, Period) %>%
  count(n_distinct(SurveyID, Stake, ActualStartTime))

pointcounts_DF = pointcounts_DF[,c(1, 2, 3, 4, 5)]
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

# get rid of original start and end time columns
pointcounts_DF = pointcounts_DF[,c(1, 2, 5, 6, 7)]




### Need to write a for loop that looks at birdnet output CSV name: date and stake number
### Using that, it needs to go into a dataframe that has the start and end times (from Pointcounts 
### Google spreadsheet, converted from hours and minutes to seconds)
### Needs to take the start and end time and dplyr filter birdnet CSV 

### Then, we can do a group_by of species listed
### Decide what birds are absurd
### Filter for confidence interval
### Add column that are 1's that indicate detected by BirdNet
### Merge with pointcount and manual detection dataframe



# For loop trimming all audiomoth outputs into correct segments

bnoutput_files <- list.files("data/pointcount_birdnetresults/")
date = "blank"
filename = "blank"
stake = "blank"
filtered = "blank"
starts = "blank"
ends = "blank"

# work in progress..........
# do we need to read in all of the files or can we write them all to a new
# data frame that houses the birds picked up in each period (group_by), stake
# surveyID, date, etc., so that we can left_join with all_compare

for(a in 1:length(bnoutput_files)){
  filename <- bnoutput_files[a]
  date = str_sub(filename, start = 1, end = 8)
  stake = str_sub(filename, start = 10, end = 11)
  file = read_csv(paste("data/pointcount_birdnetresults/", filename, sep=""))
  if (stake == pointcounts_DF$Stake){
    starts = pointcounts_DF$StartTime
    filtered = file %>%
      filter(file[,c(1) > starts])
    filtered = file %>%
      filter(file[,c(1) < ends])
  }
}




