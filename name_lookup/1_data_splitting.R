# The script takes a dataset with scientific names and conservation status info, 
# as well as an output of GBIF species lookup tool, and returns two files: list 
# of two with species lookup (matching) results, and csv with names require manual
# editing.
# 
# Inputs:
# 1) full spreadsheet with Latin names of taxa and data on their inclusion/status in
# various red lists ("checklist-protected-species-....csv").
# 2) Same Latin names after GBIF species lookup (matching) tool (https://www.gbif.org/tools/species-lookup),
# with data on matchType (EXACT, FUZZY, HIGHERRANK), taxon key in GBIF Backbone Taxonomy, 
# scientific name proposed by GBIF based on matching, taxonomic status (accepted, synonym,...),
# rank, and higher classification. verbatimScientificName keeps our original Latin name.
# 
# Outputs:
# 1) matches.Rdata - list of two with the result of name matching. 
#   [[1]] - "goodmatch" - Names whose matching is satisfactory - we will then look 
#   for occurrences for these names using taxon keys.
#   [[2]] - "badmatch" - Names whose matching is supposed to be wrong/unsatisfactory. 
#   We will then look for occurrences for these names using concatenated strings 
#   of possible verbatim scientific names.
# 2) "higherrank.csv" - csv file with Latin names that need to be completed by all
#   possible verbatim scientific names available in GBIF manually.
# 3) "attributes.csv" - csv file with unique ID for each taxon, accompanied by data 
#   on its higher taxonomy, Ukrainian name, and conservation status.


rm(list = ls()) # Reset R`s brain

# Load libraries for data manipulations
library(tidyr)
library(dplyr)

# Load input data from csv files
df.fullredlist <- read.csv("./name_lookup/checklist-protected-species-UA-template-v24-cleaned_20230522.csv")
df.normalized <- read.csv("./name_lookup/normalized.csv")

# Quick lookup at the data
glimpse(df.fullredlist)
glimpse(df.normalized)

# Select only essential taxonomic information, convert all character variables to factors
df.normalized <- df.normalized %>%
  select(2:9) %>%
  mutate_if(is.character, as.factor)

# Check data visually
head(df.normalized) # look at the first 6 rows
levels(df.normalized$matchType) # levels (variants) of matching type available

# Get some statistics about matching type
# How many Exact matching, accepted names?
df.normalized %>% 
  filter(matchType == "EXACT",
         status == "ACCEPTED") %>% 
  nrow()

# How many Exact matching, synonyms?
df.normalized %>% 
  filter(matchType == "EXACT",
         status == "SYNONYM") %>% 
  nrow()

# How many Exact matching, doubtful?
df.normalized %>% 
  filter(matchType == "EXACT",
         status == "DOUBTFUL") %>% 
  nrow()


# Merge full checklist with normalized data
df.merged <- df.fullredlist %>% # get raw list
  left_join(df.normalized, by = join_by(verbatimScientificName)) %>% # join with normalized (matched) data
  mutate(ID = 1:nrow(df.normalized)) %>% # Assign internal IDs for each row (name)
  # Following steps are technical:
  relocate(ID) %>% # relocate ID variable to the 1st place
  relocate(key, .after = ID) %>% # relocate key variable next to IDs
  relocate(scientificName, .after = verbatimScientificName) %>% # relocate names
  select(-kingdom.y) %>% # get rid of duplicated variables
  rename(kingdom = kingdom.x) # rename kingdom


# Split data on Ukrainian name, kingdom, and conservation status from scientific name(s),
# left ID as only internal unique identifier for each taxon.
df.merged %>% 
  select(!(key:scientificName)) %>% 
  select(!(matchType:rank)) %>% 
  write.csv("./name_lookup/attributes.csv")


# Splitting full normalized data by matchType
# Names whose matching is satisfactory - we will then look for occurrences for these
# names using taxon keys.
goodmatch <- df.merged %>% 
  filter(matchType != "HIGHERRANK")

# Names whose matching is supposed to be wrong/unsatisfactory. We will then look
# for occurrences for these names using concatenated strings of possible verbatim
# scientific names.
badmatch <- df.merged %>% 
  filter(matchType == "HIGHERRANK")

# Save both parts as named list 
matches <- list(goodmatch, badmatch)
names(matches) <- c("goodmatch", "badmatch")
# Write data to the drive
save(matches, file = "./name_lookup/matches.Rdata")

# Write the names with 'bad' matching to csv file (need manual reviewing before 
# the next step).
badmatch %>% 
  select(ID, verbatimScientificName) %>% 
  write.csv("higherrank.csv")

# "higherrank.csv" contains names (with their internal IDs) that can not be searched
# for occurrences by taxon key efficiently. The only option left is to search by 
# the verbatinScientificName field (names, assigned by data authors). Since each 
# taxa can be represented in GBIF datasets with various verbatim (scientific) names, 
# we need to edit "higherrank.csv" manually, adding all possible spellings for each name.
# The results must be saved as "higherrank_nameVariants.csv".
# Do not remove/rename current "higherrank_nameVariants.csv", unless you prepared 
# the next version by yourself! It required a lot of manual work - be respectful.

rm(list = ls()) # Clean-up the Environment

# End of script
