# Environment preparation ####
rm(list = ls()) # Reset R`s brain

# Load libraries
library(tidyr)
library(dplyr)
library(rgbif)

# Load data saved at step #1
load(file = "matches.Rdata")
list2env(matches, .GlobalEnv)
rm(matches)


# IUCN status update ####
iucn <- list() # make empty list
# Populate the list with IUCN Red List categories from GBIF
for (i in 1:nrow(goodmatch)) {
  # Look up IUCN Red List category
  category <- name_usage(key = goodmatch$taxonKey[i],
                         data = "iucnRedListCategory")$data[1, 5] %>% 
    as.character()
  
  if (!is.na(category)) {
    iucn[[i]] <- category
  } else {
    iucn[[i]] <- NA 
  }
}


goodmatch <- goodmatch %>% mutate(IUCN = unlist(iucn)) %>% 
  mutate_at("IUCN", as.factor)