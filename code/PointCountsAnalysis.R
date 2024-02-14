##################################

####   Point Count Analysis   ####

##################################

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

# Count the number of point count periods in which a species was detected in the point count but not via AudioMoth
pointCountSummary = pointcounts %>%
  group_by(Species, Observer) %>%
  summarize(missByManual = sum(AudiomothDetected == 0))

manualSummary = missed %>%
  rename(Species = ManualSpeciesDetection) %>%
  group_by(Species, Observer) %>%
  summarize(missByObserver = n_distinct(SurveyID, Period))  # changed from n_distinct(SurveyID, Species)


overallSummary = numPeriodsPerObserverSpecies %>%
  left_join(pointCountSummary, by = c("Species", "Observer")) %>%
  left_join(manualSummary, by = c("Species", "Observer"))

overallSummary$missByManual[is.na(overallSummary$missByManual)] = 0
overallSummary$missByObserver[is.na(overallSummary$missByObserver)] = 0

######Point Count vs Manual Analysis of Recordings Graph##############


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
# 4 letter codes from institute for bird populations
shorthand$four_letter = c("ACFL", 
                          "AMCR", 
                          "BGGN",
                          "BHCO", 
                          "BLJA",
                          "CACH",
                          "CARW",
                          "DOWO",
                          "EABL",
                          "EAWP",
                          "FICR",
                          "GCFC",
                          "n/a",
                          "HAWO",
                          "MODO",
                          "NOCA",
                          "NOPA",
                          "PIWO",
                          "RBWO",
                          "RSHA",
                          "REVI",
                          "SCTA",
                          "SUTA",
                          "TUTI",
                          "WBNU",
                          "WEWA",
                          "n/a",
                          "YBCU",
                          "YTVI")

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

#################### 1/31/2024 analysis

#get total number of detections (Point Count + Manual Analysis of Recording)
# Add together Hurlbert and Wiley Observations
PropDetected = overallSummary %>% group_by(Species) %>% mutate(totalNum = sum(totalNumPeriods), missManual = sum(missByManual), missObserver = sum(missByObserver))
PropDetected = PropDetected[c(2, 6, 7, 8)]

PropDetected$DetectedbyManual = (PropDetected$totalNum - PropDetected$missManual)
PropDetected$DetectedbyObserver = (PropDetected$totalNum - PropDetected$missObserver)

PropDetected = PropDetected %>% distinct(Species, totalNum, missManual, missObserver, DetectedbyManual, DetectedbyObserver)

# Filter for only species that had over 4 detections
PropDetected <- PropDetected %>%
  filter(totalNum > 4)

ggplot(PropDetected, aes(x = DetectedbyManual, y = DetectedbyObserver, label = Species)) +
  geom_point(size = 3) +
  geom_abline(a=1, b=0) +
  geom_label_repel(aes(label = Species),
                   box.padding   = 1, 
                   point.padding = 0.5,
                   segment.color = 'grey50',
                   label.size = 0.01,
                   max.overlaps = 100) 
# y-axis is total detection (Point Count + Manual Analysis)
# x-axis is number detected by method ()


# Make Confirmed Detection vs BirdNET Detection no confidence threshold with 1:1 line graph


#add abbreviated species names 

BirdNETtotdetections = 
  left_join(shorthand, birdnet_dist2, by = c("CommonName"= "Species"))
            
#Add BirdNET total detected to table

PropDetectedTotal = 
  left_join(BirdNETtotdetections, PropDetected, by = c("Abbrev" = "Species") )

colnames(PropDetectedTotal)[3] <- "DetectedbyBirdNET"



#add abbreviated species names 

BirdNETtotdetections = 
  left_join(shorthand, birdnet_dist2, by = c("CommonName"= "Species"))

#Add BirdNET total detected to table

PropDetectedTotal = 
  left_join(BirdNETtotdetections, PropDetected, by = c("Abbrev" = "Species") )

colnames(PropDetectedTotal)[4] <- "DetectedbyBirdNET"

# add axis titles, move addition of birdnet detections to before we filter for less than 4 because many NAs will be filtered out earlier, color  code above/below/on line, make font sizes larger, make four-letter abrv. 

#Create BirdNET data set for confidence intervals 0, 25, 50
birdnetCounts = birdnet_output %>% group_by(Species, Period, Stake, Date) %>% summarize(maxConf = max(Confidence)) %>% group_by(Species) %>% summarize(n0 = n(), n25 = sum(maxConf >= 0.25), n50 = sum(maxConf >= 0.5))

#Combine BirdNET data set with full data set

FullDataSet =
  left_join(birdnetCounts, PropDetectedTotal, by = c("Species" = "CommonName") )

#filter out birdNET detections <4
FullDataSet <- FullDataSet %>%
  filter(DetectedbyBirdNET > 4)

#Make NAs = 0
FullDataSet[is.na(FullDataSet)] <- 0

#Graph for BirdNET with no confidence interval

ggplot(FullDataSet, aes(x = totalNum, y = DetectedbyBirdNET)) +
  geom_point(size = 3) +
  geom_abline(a=1, b=0) +
  geom_label_repel(aes(label = four_letter),
                   box.padding   = 1, 
                   point.padding = 0.5,
                   segment.color = 'grey50',
                   label.size = 0.01,
                   max.overlaps = 100) 

#Graph for BirdNET confidence interval 0, 25, 50 with arrows:

plot(FullDataSet$totalNum, FullDataSet$n0, pch = 16)
text(FullDataSet$totalNum, FullDataSet$n0+3, FullDataSet$four_letter, cex = 1)
arrows(FullDataSet$totalNum, FullDataSet$n0, FullDataSet$totalNum, FullDataSet$n50, length = .1)
points(FullDataSet$totalNum, FullDataSet$n25, pch = 18, col = 'red')
abline(a=0, b = 1)


