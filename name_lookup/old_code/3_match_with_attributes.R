# Environment preparation ####
rm(list = ls()) # Reset R`s brain

# Load libraries
library(tidyr)
library(dplyr)
library(stringr)
library(rgbif)

# Load data saved at step #1
load(file = "matches.Rdata")
list2env(matches, .GlobalEnv)
rm(matches)

# Load gbif occurrences
load(file = "occurrences_all.Rdata")

# Get IUCN Red List Categories for the 'goodmatch' data
library(purrr)

# Extract taxonKeys for each observation
taxonkeys <- pull(occurrences.all[, 5])

taxonkeys[13648]
# Drop non-unique keys

taxonkeys <- taxonkeys[!duplicated(taxonkeys)]

i = 2
name_usage(key = taxonkeys[i],
           data = "iucnRedListCategory")$data

name_usage(key = taxonkeys[i],
           data = "children")$data


# %>% 
#   mutate(taxonKey = taxonkeys[i]) %>% 
#   select(taxonKey, code)

# -------
iucn.codes <- list()
for (i in 1:4
     # length(taxonkeys)
) {
  # Look up all synonyms according to GBIF Backbone Taxonomy
  iucn.code <- name_usage(key = taxonkeys[i],
                          data = "all")$data 
  # %>%
  #   mutate(taxonKey = taxonkeys[i]) %>% 
  #   select(taxonKey, code)
  
  # if (nrow(iucn.code) == 0) {
  #   df <- tibble(taxonKey = taxonkeys[i],
  #                code= NA)
  # } else 
  # Append to the list
  iucn.codes[[i]] <- iucn.code
}



# Decompose list and append its content into a single data frame
iucn.df <- iucn.codes %>% bind_rows()
# %>% 
#   select(ID, key) %>% 
#   rename(taxonKey = key)

# Combine 