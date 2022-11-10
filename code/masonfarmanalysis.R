# Load libraries
library(gsheet)
library(dplyr)
library(stringr)


pointcountURL = 'https://docs.google.com/spreadsheets/d/1TzHlLGW95utQSb62aXR-ksdh01Egmipqc1ypSJYpl6U/edit#gid=23822252'

missedbirdsURL = 'https://docs.google.com/spreadsheets/d/1TzHlLGW95utQSb62aXR-ksdh01Egmipqc1ypSJYpl6U/edit#gid=1227608871'

pointcounts = gsheet2tbl(pointcountURL)

missed = gsheet2tbl(missedbirdsURL)

birdsummary = pointcounts %>%
  group_by(Species, Observer) %>%
  summarize(numPeriods = n_distinct(SurveyID, Period),
            AMmiss = sum(AudiomothDetected == 0, na.rm = T)) %>%
  arrange(desc(numPeriods))