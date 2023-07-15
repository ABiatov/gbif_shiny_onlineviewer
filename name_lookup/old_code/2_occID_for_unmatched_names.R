rm(list = ls()) # Reset R`s brain

# Load libraries
library(tidyr)
library(dplyr)
library(rgbif)

# Load data saved at step #1
load(file = "./name_lookup/matches.Rdata")
list2env(matches, .GlobalEnv)
rm(matches)

# Set priors
current_country <- "UA" # country code
limit <- 100000 # Max number of occurrences retrieved

# Load names that should be searched by verbatim scientific names
speciesnames <- read.csv("./name_lookup/higherrank_nameVariants.csv") %>%
  select(ID, nameStringConcatenated)
# Resulting dataframe contains two variables: internal ID, which help us later 
# to merge their occurrence data with occ. data for "goodmatch", and concatenated 
# string of possible spellings for verbatin scientific names for each taxon. 


# Create an empty list
search_results <- list()

# Get existing occurrenceID for each name ID from badmatch 
for (i in 1:nrow(speciesnames)) {
  # Look up for occurrences
  # Returns tibble with occurrences if any, NULL if no occs found for the particular name
  df <- occ_search(verbatimScientificName = speciesnames[i, 2],
                   hasCoordinate = TRUE,
                   country = "UA",
                   limit = 1000)$data
  
  # If the tibble in not empty, create ID column according to
  # previously defined nameID, and append it to the list
  
  if (!is.null(df)) {
    df %>% 
      select(key) %>% 
      mutate(ID = speciesnames[i, 1]) %>%
      relocate(ID) -> search_results[[i]]
    
  } else {
    print("No data for this scientificName")
  }
}

# Output of the previous step is a lost of many, where each sub-list contains 
# occurrences for the particular taxon.

# Merge results to get occurrence IDs associated with name IDs 
bind_rows(search_results) %>% 
  mutate_if(is.character, as.numeric) %>% 
  write.csv("./name_lookup/occIDforBadmatch.csv")
# Results - occurrenceIDs (occurrence keys) for each name ID in badmatch dataframe

# Extract name ID and taxonKey for successfuly matched names
goodmatch %>% 
  select(ID, key) %>% 
  write.csv("./name_lookup/taxonIDforGoodmatch.csv")
# Results - taxonKey (in GBIF Backbode Taxonomy) for each name ID in goodmatch dataframe

# Next we can download occurrences using either taxonKey (for goodmatch) or occurrenceID
# (for badmatch) separately, then merge them together using internal IDs as unique 
# identifier of each name.
# Then we can merge retrieved occurrences with Ukrainian names and conservation
# statuses from the "attributes.csv".

rm(list = ls()) # Clean-up the Environment

# End of script
