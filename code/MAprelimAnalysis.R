library(ggplot2)
library(dplyr)

overallSummary$PropMissedbyObserver = overallSummary$missByObserver / overallSummary$totalNumPeriods
overallSummary$PropMissedbyManual = overallSummary$missByManual / overallSummary$totalNumPeriods

# creating grouped bar plot for proportion observer missed and manual missed

misses_props = overallSummary %>%
  filter(Species != ("Hawk")) %>%
  filter(Species != ("BHCO")) %>%
  filter(Species != ("DW")) %>%
  filter(Species != ("EABL")) %>%
  filter(Species != ("FICR")) %>%
  filter(Species != ("GCF")) %>%
  filter(Species != ("HWP")) %>%
  filter(Species != ("NC")) %>%
  filter(Species != ("PW")) %>%
  filter(Species != ("RSH")) %>%
  filter(Species != ("SCT")) %>%
  filter(Species != ("SUT")) %>%
  filter(Species != ("WP")) %>%
  filter(Species != ("YTV"))

#proportion missed comparing observers in point counts
ggplot(misses_props, aes(fill = Observer, x = Species, y = PropMissedbyObserver)) +
  geom_bar(position = "dodge", stat="identity")

#frequency missed comparing observers in point counts
ggplot(misses_props, aes(fill = Observer, x = Species, y = missByObserver)) +
  geom_bar(position = "dodge", stat="identity")


# creating grouped bar plot comparing total missed by manual and total missed by point counts

comparePCtoManual <- overallSummary[c(2, 3, 4, 5)] %>%
  group_by(Species) %>%
  summarize(totalNumPeriodsAG = sum(totalNumPeriods), totalMissedManual = sum(missByManual), 
            totalMissedObserver = sum(missByObserver))
comparePCtoManual <- aggregate(comparePCtoManual$totalNumPeriods, list=comparePCtoManual$Species, FUN=sum)


comparePCtoManual$PropObserverMissed = comparePCtoManual$totalMissedObserver / comparePCtoManual$totalNumPeriodsAG
comparePCtoManual$PropManualMissed = comparePCtoManual$totalMissedManual / comparePCtoManual$totalNumPeriodsAG

names(comparetomanual) <- c("Observer", "Species", "TotalAgPeriods", "TotalMissed", "PropMissed") 

comparetomanual = comparetomanual %>%
  filter(Species != ("EABL")) %>%
  filter(Species != ("FICR")) %>%
  filter(Species != ("GCF")) %>%
  filter(Species != ("Hawk")) %>%
  filter(Species != ("SCT")) %>%
  filter(Species != ("SUT")) 

#Proportion missed comparing point counts to manual
ggplot(comparetomanual, aes(fill = Observer, x = Species, y = PropMissed)) +
  geom_bar(position = "dodge", stat="identity")

#Frequency missed comparing point counts to manual
ggplot(comparetomanual, aes(fill = Observer, x = Species, y = TotalMissed)) +
  geom_bar(position = "dodge", stat="identity")











