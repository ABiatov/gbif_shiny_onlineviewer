rm(list = ls()) # Reset R`s brain
setwd("~/git/gbif_shiny_omlineviewer/name_lookup")

library(tidyr)
library(dplyr)
library(rgbif)

# Load primary data
load(file = "matches.Rdata")
list2env(matches, .GlobalEnv)
rm(matches)

# Set priors
current_country <- "UA"
limit <- 1000 # Max num. of occurrences retrieved

# 
speciesnames <- read.csv("higherrank_nameVariants.csv") |> 
  select(ID, nameStringConcatenated)


# Create an empty list
search_results <- list()

# Get existing occurrenceID for each name ID from badmatch 
for (i in 1:nrow(speciesnames)) {
  # Look up for occurrences
  # Returns tibble with occurrences if any, NULL if no occs found
  df <- occ_search(verbatimScientificName = speciesnames[i, 2],
                   hasCoordinate = TRUE,
                   country = current_country,
                   limit = limit)$data
  
  # If the tibble in not empty, create ID column according to
  # previously defined nameID, and append it to the list
  
  if (!is.null(df)) {
    df |> 
      select(key) |> 
      mutate(ID = speciesnames[i, 1]) |>
      relocate(ID) -> search_results[[i]]
    
  } else {
    print("No data for this scientificName")
  }
}


# Merge results to get occurrence IDs associated with name IDs 
occIDforBadmatch <- bind_rows(search_results) |> 
  mutate_if(is.character, as.numeric)

# Results - list of occurrenceIDs (key) for each name ID in badmatch dataframe
