#################################

# Mason Farm AudioMoth Analysis #

#################################

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

# Read in compiled BirdNET CSV
birdnet_output = read_csv("data/CompiledBirdNetResults_2019PointCounts.csv")
birdnet_output = birdnet_output[,c(2, 3, 4, 5, 6, 7, 8, 9)]

################################

### Exploratory data analysis

# Summarize data

# Calculating number of periods each species (by observer) occurred in
foo = pointcounts %>%
  distinct(Observer, Species, SurveyID, Period)

numPeriodsPerObserverSpecies = missed %>%
  rename(Species = ManualSpeciesDetection) %>%
  distinct(Observer, Species, SurveyID, Period) %>%
  rbind(foo) %>%
  distinct(Observer, Species, SurveyID, Period) %>%
  count(Observer, Species) %>%
  rename(totalNumPeriods = n)

pointCountSummary = pointcounts %>%
  group_by(Species, Observer) %>%
  summarize(missByManual = sum(AudiomothDetected == 0))


# Find species missed by observers
manualSummary = missed %>%
  rename(Species = ManualSpeciesDetection) %>%
  group_by(Species, Observer) %>%
  summarize(missByObserver = n_distinct(SurveyID, Species))


overallSummary = numPeriodsPerObserverSpecies %>%
  left_join(pointCountSummary, by = c("Species", "Observer")) %>%
  left_join(manualSummary, by = c("Species", "Observer"))

overallSummary$missByManual[is.na(overallSummary$missByManual)] = 0
overallSummary$missByObserver[is.na(overallSummary$missByObserver)] = 0


################################

### BirdNet Output Analysis

# Goal here is to mark a 1 or a 0 for whether or not it was picked up in a period

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

### Short hand transcription - do not move, needs to be after compare

shorthand = compare %>%
  group_by(Species) %>%
  count(n_distinct(Species))

shorthand = shorthand[,c(1)]
names(shorthand) = c("Abbrev")

shorthand$CommonName = NA
shorthand$CommonName = c("Acadian Flycatcher", 
                         "American Crow", 
                         "Blue-gray Gnatcatcher",
                         "Brown-headed Cowbird", 
                         "Blue Jay",
                         "Carolina Chickadee",
                         "Carolina Wren",
                         "Downy Woodpecker",
                         "Eastern Bluebird",
                         "Eastern Wood Pewee",
                         "Fish Crow",
                         "Great Crested Flycatcher",
                         "Unidentified Hawk",
                         "Hairy Woodpecker",
                         "Mourning Dove",
                         "Northern Cardinal",
                         "Northern Parula",
                         "Pileated Woodpecker",
                         "Red-bellied Woodpecker",
                         "Red-shouldered Hawk",
                         "Red-eyed Vireo",
                         "Scarlet Tanager",
                         "Summer Tanager",
                         "Tufted Titmouse",
                         "White-breasted Nuthatch",
                         "Worm-eating Warbler",
                         "Unidentified Woodpecker",
                         "Yellow-billed Cuckoo",
                         "Yellow-throated Vireo")

# How to merge the abbreviations with the all_compare, while also bringing over the birds that are not included
# in the shorthand abbrev bc theyre not common?

# BirdNET 50% Threshold

birdnet_50 <- birdnet_output %>%
  filter(Confidence >= 0.5) %>%
  select(Date, Stake, Period, Start, End, Scientific, Species, Confidence) %>%
  distinct(Date, Stake, Period, Species) %>%
  mutate(BirdNET50_Detected = 1, CommonName = Species) %>%
  left_join(shorthand, by = c("CommonName"))

#### Shall we also do 60, 70, 80, 90?









all_compare = merge(comparison, manual, by=c("SurveyID", "Stake", "Observer", "Species", "Period", "AudiomothDetected"), all.x = TRUE, all.y = TRUE)







