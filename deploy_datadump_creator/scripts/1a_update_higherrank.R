# REQUIRE MANUAL OPERATIONS! DON'T RUN IT IN THE BACKGROUND!

# --------------------------------
# The script is recommended if the user updates the data after a long time since
# the last update. After any major GBIF Backbone Taxonomy updates, it might happen
# that the set of names which were matched to 'higherrank' changes. Most of 
# them, we can expect, remain the same, but to be sure and to fix possible issues 
# we need to check that again.
# To avoid tedious manual work, we use data from previous update to fullfil possible 
# variants of name spellings in the new 'higherrank' dataframe. For that reason,
# we should always keep previous version of files.

rm(list = ls()) # Reset R`s brain

# Load libraries for data manipulations
library(tidyr)
library(dplyr)
library(DataEditR) # GUI for dataframe editing

# Load input data from csv files
# Taxon names matched as 'higherrank' during previous dump update
previous_higherrank <- read.csv("./data/higherrank_nameVariants.csv") %>% 
  select(-X, -ID)

# Taxon names matched as 'higherrank' during current session
new_higherrank <- read.csv("./temp_1/higherrank.csv") %>% 
  select(-X)

# Fulfill names with spelling variants from the previous match, for cases where 
# names remain the same.
updated_higherrank <- new_higherrank %>% 
  left_join(previous_higherrank, by = "verbatimScientificName")

# Separate existing names from that appear just this time
# (after recent Backbone updates), and write them into a new variable
not_changed_since_last_time <- updated_higherrank %>% 
  filter(!is.na(X.1))

to_be_fulfilled_manually <- updated_higherrank %>% 
  filter(is.na(X.1))


# Launch GUI and add necessary name variants right to the names
manually_fulfilled <- data_edit(to_be_fulfilled_manually)
# When the editing is done, press 'synchronise' button, then 'Done' to finish editing

# Combine previous and newcomer parts of the data
higherrank_nameVariants <- not_changed_since_last_time %>% 
  bind_rows(manually_fulfilled)

# Write data as CSV
# Previous version (as back-up)
write.csv(previous_higherrank, "./data/higherrank_nameVariants_prev.csv")
# Current version
write.csv(higherrank_nameVariants, "./data/higherrank_nameVariants.csv")
