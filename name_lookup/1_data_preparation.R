# ----------------------------------
# The script takes a dataset with scientific names and conservation status info, 
# and returns two files: list of two with species lookup (matching) results, and 
# csv with names require manual editing.
# 
# Inputs:
# full spreadsheet with Latin names of taxa and data on their inclusion/status in
# various red lists ("checklist-protected-species-....csv").
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


rm(list = ls()) # Reset R`s brain

# Load libraries for data manipulations
library(tidyr)
library(dplyr)
library(rgbif)

# Load input data from csv files
# df.fullredlist <- read.csv("checklist-protected-species-UA-template-v24-cleaned_20230710.csv")
df.fullredlist <- read.csv("./data/Species-protected-list-v26_cleaned_20230801.csv")

# Match taxonomic names against the GBIF Backbone Taxonomy
name_data <- df.fullredlist %>% 
  select(verbatimScientificName, kingdom) %>% 
  rename(name = verbatimScientificName)

lookup_results <- name_backbone_checklist(name_data)

lookup_results_selected <- lookup_results %>% rename(verbatimScientificName = verbatim_name) %>%
  rename(ID = verbatim_index) %>% # Assign internal IDs for each row (name)
  # Following steps are technical:
  relocate(ID) %>% # relocate ID variable to the 1st place
  relocate(usageKey, .after = ID) %>% # relocate key variable next to IDs
  relocate(verbatimScientificName, .after = usageKey) %>% # relocate verbatim name variable next to taxon key
  relocate(scientificName, .after = verbatimScientificName) %>% # relocate names
  relocate(verbatim_kingdom, .after = kingdom) %>% 
  select(1:11) %>% # Select only essential taxonomic information
  mutate_if(is.character, as.factor) # convert all character variables to factors



# Merge full checklist with lookup results
df.merged <- df.fullredlist %>% # get raw list
  left_join(lookup_results_selected, by = join_by(verbatimScientificName)) %>% # join with normalized (matched) data
  # Following steps are technical:
  relocate(ID) %>% # relocate ID variable to the 1st place
  relocate(usageKey, .after = ID) %>% # relocate key variable next to IDs
  rename(taxonKey = usageKey) %>% 
  relocate(scientificName, .after = verbatimScientificName) %>% # relocate names
  select(-kingdom.y,
         -phylum,
         -class,
         -canonicalName,
         -rank,
         -confidence,
         -verbatim_kingdom) %>% # get rid of the variables don't needed anymore
  rename(kingdom = kingdom.x) # rename kingdom



# Splitting full normalized data by matchType
# Names whose matching is satisfactory - we will then look for occurrences for these
# names using taxonKey field.
goodmatch <- df.merged %>% 
  filter(matchType != "HIGHERRANK")

# Names whose matching is supposed to be wrong/unsatisfactory. We will then look
# for occurrences for these names using concatenated strings of possible verbatim
# scientific names.
badmatch <- df.merged %>% 
  filter(matchType == "HIGHERRANK")

# Save both parts as a named list 
matches <- list(goodmatch, badmatch)
names(matches) <- c("goodmatch", "badmatch")
# Write data to the drive
save(matches, file = "./temp/matches.Rdata")

# Write the names with 'bad' matching to csv file (need manual reviewing before 
# the next step).
badmatch %>% 
  select(ID, verbatimScientificName) %>% 
  write.csv("./temp/higherrank.csv")

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
