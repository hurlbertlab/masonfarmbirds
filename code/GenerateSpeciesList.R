##Generate a species list formatted for birdnet analysis just from the species names
#mostly pseudocode right now, need file list to work with first to finish code

library(dplyr)

#read in common name file
commonnames <- read.csv()

#read in taxonomy file
taxonomy <- read.csv("data/ebird_taxonomy_v2022.csv")

#join files together
df <-left_join(commonnames, taxonomy, "" = "PRIMARY_COM_NAME")

#create part of table that's going to have it formatted the way we want
df <- df %>% 
  mutate(
    textformat <- paste(species, genus, commonname)
  )

text <- df$textformat

#format as text file and save
write.table(df,"/Users/admin/fileOut.txt")

