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

filter_bn = test_bn %>%
  filter(test_bn[,c(1)] > 2000)


### Need to write a for loop that looks at birdnet output CSV name: date and stake number
### Using that, it needs to go into a dataframe that has the start and end times (from Pointcounts 
### Google spreadsheet, converted from hours and minutes to seconds)
### Needs to take the start and end time and dplyr filter birdnet CSV 

### Then, we can do a group_by of species listed
### Decide what birds are absurd
### Filter for confidence interval
### Add column that are 1's that indicate detected by BirdNet
### Merge with pointcount and manual detection dataframe
