#################################

# Mason Farm AudioMoth Analysis #

#################################

# Load libraries
library(gsheet)
library(dplyr)
library(tidyverse)
library(stringr)
library(ggrepel)

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
                         "Eastern Wood-Pewee",
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

### Working w Comparison

# Adding dates to comparison so that merging is easier
for (i in 1:nrow(comparison)) {
  if (comparison$SurveyID[i] < 9) {
    comparison$Date[i] = "20190612"
  }
  if (comparison$SurveyID[i] > 9 && comparison$SurveyID[i] < 17) {
    comparison$Date[i] = "20190617"
  }
  if (comparison$SurveyID[i] > 17 && comparison$SurveyID[i] < 25) {
    comparison$Date[i] = "20190618"
  }
  if (comparison$SurveyID[i] > 25 && comparison$SurveyID[i] < 33) {
    comparison$Date[i] = "20190619"
  }
  if (comparison$SurveyID[i] > 33) {
    comparison$Date[i] = "20190629"
  }
}

# Converting Short Hand to Long Names in Comparison

comparison$SpeciesName <- with(
  shorthand,
  CommonName[match(comparison$Species, Abbrev)]
)

comparison2 <- comparison[,c(8, 2, 9, 5, 6, 7)]
names(comparison2)[names(comparison2) == 'SpeciesName'] <- 'Species'



# BirdNET Distinct
birdnet_dist <- birdnet_output %>% 
  distinct(Date, Stake, Period, Species)

# Number of birds detected with no min confidence
num_dist_birds <- nrow(birdnet_dist)

birdnet_dist2 <- birdnet_dist %>%
  count(Species) %>%
  rename(Number = n)

ggplot(birdnet_dist2, aes(x = reorder(Species, -Number), y = Number)) +
  geom_bar(binwidth = 1, stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle('Birds Detected by BirdNET with No Minimum Confidence') +
  theme(text = element_text(size = 10)) +
  ylab("Count") +
  xlab("Species")

ggplot(birdnet_dist, aes(x=Species)) +
  geom_bar(binwidth = 1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle('Birds Detected by BirdNET with No Minimum Confidence') +
  theme(text = element_text(size = 15))

# BirdNET 50% Threshold

birdnet_50 <- birdnet_output %>%
  filter(Confidence >= 0.5) %>%
  select(Date, Stake, Period, Start, End, Scientific, Species, Confidence) %>%
  distinct(Date, Stake, Period, Species)

birdnet_50$BirdNET50Detected <- 1

birdnet_50$Date <- as.character(birdnet_50$Date)
birdnet_50$Period <- as.character(birdnet_50$Period)

all_compare = comparison2 %>%
  merge(birdnet_50, by = c("Date", "Stake", "Period", "Species"), all.x = TRUE, all.y = TRUE)

all_compare$AudiomothDetected[is.na(all_compare$AudiomothDetected)] = 0
all_compare$ObserverDetected[is.na(all_compare$ObserverDetected)] = 0
all_compare$BirdNET50Detected[is.na(all_compare$BirdNET50Detected)] = 0


# Number of bird detected with a 50% threshold

num_50_birds <- nrow(birdnet_50)

birdnet_50_2 <- birdnet_50 %>%
  count(Species) %>%
  rename(Number = n)

ggplot(birdnet_50_2, aes(x = reorder(Species, -Number), y = Number)) +
  geom_bar(binwidth = 1, stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle('Birds Detected by BirdNET with a Min Confidence of 50%') +
  theme(text = element_text(size = 15))+
  ylab("Count") +
  xlab("Species")

birdnet_50$BirdNET50Detected <- 1

# Prepping for merge
birdnet_50$Date <- as.character(birdnet_50$Date)
birdnet_50$Period <- as.character(birdnet_50$Period)

# BirdNET 60%+ Threshold

birdnet_60 <- birdnet_output %>%
  filter(Confidence >= 0.6) %>%
  select(Date, Stake, Period, Start, End, Scientific, Species, Confidence) %>%
  distinct(Date, Stake, Period, Species)

birdnet_60$BirdNET60Detected <- 1

birdnet_60$Date <- as.character(birdnet_60$Date)
birdnet_60$Period <- as.character(birdnet_60$Period)

birdnet_70 <- birdnet_output %>%
  filter(Confidence >= 0.7) %>%
  select(Date, Stake, Period, Start, End, Scientific, Species, Confidence) %>%
  distinct(Date, Stake, Period, Species)

birdnet_70$BirdNET70Detected <- 1

birdnet_70$Date <- as.character(birdnet_70$Date)
birdnet_70$Period <- as.character(birdnet_70$Period)

birdnet_80 <- birdnet_output %>%
  filter(Confidence >= 0.8) %>%
  select(Date, Stake, Period, Start, End, Scientific, Species, Confidence) %>%
  distinct(Date, Stake, Period, Species)

birdnet_80$BirdNET80Detected <- 1

birdnet_80$Date <- as.character(birdnet_80$Date)
birdnet_80$Period <- as.character(birdnet_80$Period)

birdnet_90 <- birdnet_output %>%
  filter(Confidence >= 0.9) %>%
  select(Date, Stake, Period, Start, End, Scientific, Species, Confidence) %>%
  distinct(Date, Stake, Period, Species)

birdnet_90$BirdNET90Detected <- 1

birdnet_90$Date <- as.character(birdnet_90$Date)
birdnet_90$Period <- as.character(birdnet_90$Period)

### Merge BirdNET Outputs - form all_compare

all_compare = comparison2 %>%
  merge(birdnet_50, by = c("Date", "Stake", "Period", "Species"), all.x = TRUE, all.y = TRUE)  %>%
  left_join(birdnet_60, by = c("Date", "Stake", "Period", "Species")) %>%
  left_join(birdnet_70, by = c("Date", "Stake", "Period", "Species")) %>%
  left_join(birdnet_80, by = c("Date", "Stake", "Period", "Species"))

all_compare$AudiomothDetected[is.na(all_compare$AudiomothDetected)] = 0
all_compare$ObserverDetected[is.na(all_compare$ObserverDetected)] = 0
all_compare$BirdNET50Detected[is.na(all_compare$BirdNET50Detected)] = 0
all_compare$BirdNET60Detected[is.na(all_compare$BirdNET60Detected)] = 0
all_compare$BirdNET70Detected[is.na(all_compare$BirdNET70Detected)] = 0
all_compare$BirdNET80Detected[is.na(all_compare$BirdNET80Detected)] = 0


# Find Props Detected BirdNET + Manual
AMBN_Compare <- all_compare[,c(1, 2, 3, 4, 5, 7, 8, 9, 10)]

numPeriodsAMBN = AMBN_Compare %>%
  distinct(Date, Stake, Species, Period) %>%
  count(Species) %>%
  rename(totalNumPeriods = n)

AMBN_detection = AMBN_Compare %>%
  group_by(Species) %>%
  summarize(ManualDetect = sum(AudiomothDetected == 1), BN50Detect = sum(BirdNET50Detected == 1), BN60Detect = sum(BirdNET60Detected == 1), 
            BN70Detect = sum(BirdNET70Detected == 1), BN80Detect = sum(BirdNET80Detected == 1)) %>%
  left_join(numPeriodsAMBN, by = c("Species"))

AMBN_detection$BN50PropDetect = AMBN_detection$BN50Detect / AMBN_detection$totalNumPeriods
AMBN_detection$ManualPropDetect = AMBN_detection$ManualDetect / AMBN_detection$totalNumPeriods

ggplot(AMBN_detection, aes(x = BN50PropDetect, y = ManualPropDetect, label = Species)) +
  geom_point() +
  geom_label_repel(aes(label = Species),
                   box.padding   = 0.8, 
                   point.padding = 0.5,
                   segment.color = 'grey50',
                   label.size = 0.05,
                   max.overlaps = 30) 

# Finding rate of error of all birds 
rerror50percent <- (abs(sum(AMBN_detection$BN50Detect) - sum(AMBN_detection$totalNumPeriods)) / sum(AMBN_detection$totalNumPeriods))*100
rerror60percent <- (abs(sum(AMBN_detection$BN60Detect) - sum(AMBN_detection$totalNumPeriods)) / sum(AMBN_detection$totalNumPeriods))*100
rerror70percent <- (abs(sum(AMBN_detection$BN70Detect) - sum(AMBN_detection$totalNumPeriods)) / sum(AMBN_detection$totalNumPeriods))*100
rerror80percent <- (abs(sum(AMBN_detection$BN80Detect) - sum(AMBN_detection$totalNumPeriods)) / sum(AMBN_detection$totalNumPeriods))*100

num_60_birds <- nrow(birdnet_60)
num_70_birds <- nrow(birdnet_70)
num_80_birds <- nrow(birdnet_80)

AMBN_detection2<- AMBN_detection %>%
  filter(ManualDetect != 0)
  
ggplot(AMBN_detection2, aes(x = reorder(Species, -ManualDetect), y = ManualDetect)) +
  geom_bar(binwidth = 1, stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle('Birds Detected by Manual Detection') +
  theme(text = element_text(size = 20)) +
  xlab("Species") +
  ylab("Count")



