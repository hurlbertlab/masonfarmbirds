# Supplemental Figure conveying the distribution of BirdNET confidence scores by species

# Requires dataframe 'shorthand' from PointCountsAnalysis.R
library(vioplot)
library(dplyr)

compiledConfidence = read.csv('data/CompiledBirdNetResults_2019PointCounts.csv') %>%
  filter(Species %in% shorthand$CommonName,
         Species != "Brown-headed Cowbird")

# Step 1: Compute mean confidence per species
means <- tapply(compiledConfidence$Confidence, compiledConfidence$Species, mean)

# Step 2: Order species by decreasing mean
ordered_species <- names(sort(means, decreasing = TRUE))

# Step 3: Reorder factor levels of Species in the dataframe
compiledConfidence$Species <- factor(compiledConfidence$Species, levels = ordered_species)

# Step 4: Plot violin plots without x-axis labels
par(mar = c(12, 5, 1, 1))

# Create a vector of colors to assign based on species colors
viocolors = c(rep('gray50', 5), "#FF7700", 'gray50', 'gray50', "#4488D2", 'gray50', "#F0B924", "#FD569C", 'gray50', "#0ACE79", rep('gray50', 6), "#D58DFF", rep('gray50', 5))

vioplot(Confidence ~ Species, data = compiledConfidence,
        ylab = "BirdNET Confidence", xaxt = "n", xlab = "", col = viocolors)

axis(1, at = 1:26, labels = ordered_species, las = 2)
