library(ggplot2)
library(dplyr)

overallSummary$PropMissedbyObserver = overallSummary$missByObserver / overallSummary$totalNumPeriods
overallSummary$PropMissedbyManual = overallSummary$missByManual / overallSummary$totalNumPeriods

# creating grouped bar plot for proportion observer missed and manual missed

# NOTE: see below for a more concise way of filtering all of those species in one line
misses_props = overallSummary %>%
  filter(!Species %in% c("Hawk", "BHCO", "DW", "EABL", "FICR", "GCF", "HWP", "NC", "PW", "RSH", "SCT", "SUT", "WP", "YTV"))

#proportion missed comparing observers in point counts
ggplot(misses_props, aes(fill = Observer, x = reorder(Species, -PropMissedbyObserver), y = PropMissedbyObserver)) +
  geom_bar(position = "dodge", stat="identity")+
  xlab("Species") +
  ylab("Proportion Missed by Observer") +
  ggtitle("Proportion of Birds Missed")

#frequency missed comparing observers in point counts
ggplot(misses_props, aes(fill = Observer, x = reorder(Species, -missByObserver), y = missByObserver)) +
  geom_bar(position = "dodge", stat="identity")+
  xlab("Species") +
  ylab("Total Number of Birds Missed by Observer") +
  ggtitle("Total Number of Birds Missed by Occurance")


# creating grouped bar plot comparing total missed by manual and total missed by point counts

comparePCtoManual <- overallSummary[c(2, 3, 4, 5)] %>%
  group_by(Species) %>%
  summarize(totalNumPeriodsAG = sum(totalNumPeriods), totalMissedManual = sum(missByManual), 
            totalMissedObserver = sum(missByObserver))

# what was the purpose of this line? was it an old attempt?
#comparePCtoManual <- aggregate(comparePCtoManual$totalNumPeriods, list=comparePCtoManual$Species, FUN=sum)


comparePCtoManual$PropObserverMissed = comparePCtoManual$totalMissedObserver / comparePCtoManual$totalNumPeriodsAG
comparePCtoManual$PropManualMissed = comparePCtoManual$totalMissedManual / comparePCtoManual$totalNumPeriodsAG

# There is no object called 'comparetomanual', only comparePCtoManual, so this and subsequent lines error
names(comparetomanual) <- c("Observer", "Species", "TotalAgPeriods", "TotalMissed", "PropMissed") 

# Why where these species being filtered out? Explain why with a comment
comparetomanual = comparetomanual %>%
  filter(Species != ("EABL")) %>%
  filter(Species != ("FICR")) %>%
  filter(Species != ("GCF")) %>%
  filter(Species != ("Hawk")) %>%
  filter(Species != ("SCT")) %>%
  filter(Species != ("SUT")) 

#Proportion missed comparing point counts to manual
ggplot(comparetomanual, aes(fill = Observer, x = reorder(Species, -PropMissed), y = PropMissed)) +
  geom_bar(position = "dodge", stat="identity") +
  xlab("Species") +
  ylab("Proportion of Birds Missed") +
  ggtitle("Proportion of Birds Missed by Manual Detection versus Observers")

#Frequency missed comparing point counts to manual
ggplot(comparetomanual, aes(fill = Observer, x = reorder(Species, -TotalMissed), y = TotalMissed)) +
  geom_bar(position = "dodge", stat="identity")+
  xlab("Species") +
  ylab("Total Number of Birds Missed") +
  ggtitle("Total Number of Birds Missed: Manual Detection versus Observers")











