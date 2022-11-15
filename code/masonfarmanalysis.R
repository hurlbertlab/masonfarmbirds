################################

### Mason Farm AudioMoth EDA ###

################################

# Load libraries
library(gsheet)
library(dplyr)
library(tidyverse)
library(stringr)

### Read in data
pointcountURL = 'https://docs.google.com/spreadsheets/d/1TzHlLGW95utQSb62aXR-ksdh01Egmipqc1ypSJYpl6U/edit#gid=23822252'

missedbirdsURL = 'https://docs.google.com/spreadsheets/d/1TzHlLGW95utQSb62aXR-ksdh01Egmipqc1ypSJYpl6U/edit#gid=1227608871'

# Convert google sheets to tables
pointcounts = gsheet2tbl(pointcountURL)

missed = gsheet2tbl(missedbirdsURL)

################################

### Exploratory data analysis

# Summarize data
birdsummary = pointcounts %>%
  group_by(Species, Observer) %>%
  summarize(numPeriods = n_distinct(SurveyID, Period),
            AMmiss = sum(AudiomothDetected == 0)) %>%
  arrange(desc(numPeriods))

# Audiomoth misses and detected
num_misses = pointcounts %>%
  count(AudiomothDetected)

# Find species missed by observers
observermissed = missed %>%
  group_by(ManualSpeciesDetection, Observer) %>%
  count(n_distinct(SurveyID, ManualSpeciesDetection))

o_missed = observermissed[,c(1, 2, 4)]
names(o_missed) = c("Species", "Observer", "ObserverMiss")

# Add species missed by observers to data summary
birdsum = left_join(birdsummary, o_missed, by=c("Species", "Observer"))

# For loop changing NA to 0
for (i in 1:nrow(birdsum)) {
  if (is.na(birdsum$ObserverMiss[i]) == TRUE){
    birdsum$ObserverMiss[i] = 0
  }
    
}

################################

### BirdNet Output Analysis

# Making df with distinct periods, observer, and species combo to compare birdnet, audiomoth, and observer

compare = pointcounts %>%
  group_by(SurveyID, Stake, Observer, Species, Period, AudiomothDetected) %>%
  count(n_distinct(SurveyID, Period, Species))

manuallycaught = missed %>%
  group_by(SurveyID, Stake, Observer, ManualSpeciesDetection, Period) %>%
  count(n_distinct(SurveyID, Period, ManualSpeciesDetection))

manual = manuallycaught[,c(1, 2, 3, 4, 5, 7)]

names(manual) = c("SurveyID", "Stake", "Observer", "Species", "Period", "AudiomothDetected")

manual$AudiomothDetected = as.numeric(manual$AudiomothDetected) 

manual$Period = as.character(manual$Period)

compare$ObserverDetected = 1

comparison = compare[,c(1, 2, 3, 4, 5, 6, 9)]

all_compare = merge(comparison, manual, by=c("SurveyID", "Stake", "Observer", "Species", "Period", "AudiomothDetected"), all.x = TRUE, all.y = TRUE)

# For loop changing NA to 0
for (i in 1:nrow(all_compare)) {
  if (is.na(all_compare$ObserverDetected[i]) == TRUE){
    all_compare$ObserverDetected[i] = 0
  }
  
}

