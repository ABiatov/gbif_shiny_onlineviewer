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


# Load names that should be searched by verbatim scientific names
speciesnames <- read.csv("higherrank_nameVariants.csv") %>% 
  select(-X, -nameStringConcatenated)
# Resulting dataframe internal ID, which helps us later to merge their occurrence 
# data with occ. data for "goodmatch", and possible spellings for verbatim 
# scientific names for each taxon. 

# Taxa with bad match to GBIF Backbone Taxonomy ####
# Should be searched by verbatinScientificName term

# Create an empty list for search queries
queries <- list()
# Populate the list
for (i in 1:nrow(speciesnames)) {
  queries[[i]] <- occ_download_prep(
    pred_in("verbatimScientificName",
            unname(unlist(speciesnames[i, 2:ncol(speciesnames)]))
    ),
    pred("hasCoordinate", TRUE),
    pred("occurrenceStatus","PRESENT"), 
    pred_not(pred_in("basisOfRecord",c("FOSSIL_SPECIMEN","LIVING_SPECIMEN"))),
    pred("country", "UA"),  # TODO: Move "UA" to Config.R
    format = "SIMPLE_CSV"
    # user = gbif_user,
    # pwd = gbif_pwd,
    # email = gbif_email
  )
}

# Download occurrences by executing queries 
out <- occ_download_queue(.list = queries)

# Download occurrences as seria of tibbles to local drive, filter and mutate it
downloaded <- list()
for (i in 1:length(out)) {
  d <- occ_download_get(occ_download_meta(out[i])$key) %>%
    occ_download_import() %>% 
    select(
      gbifID,
      datasetKey,
      scientificName,
      verbatimScientificName,
      eventDate,
      decimalLatitude,
      decimalLongitude,
      coordinateUncertaintyInMeters,
      coordinatePrecision
    ) %>% 
    mutate(ID = speciesnames[i, 1]) %>% 
    relocate(ID) %>% # relocate ID variable to the 1st place
    mutate_at("ID", as.integer) %>% 
    mutate_at("gbifID", bit64::as.integer64) %>% 
    mutate_at(c("datasetKey",
                "scientificName",
                "verbatimScientificName",
                "eventDate"), as.character) %>% 
    mutate_at(c("decimalLatitude",
                "decimalLongitude",
                "coordinateUncertaintyInMeters",
                "coordinatePrecision"), as.double)
  
  downloaded[[i]] <- d
}

# Convert downloaded occurrences into a single tibble dataframe
occurrences_for_badmatch <- downloaded  %>%  bind_rows()
save(occurrences_for_badmatch, file = "occurrences_for_badmatch.Rdata")
gc()
# load(file = "occurrences_for_badmatch.Rdata")

# Taxa with good match to GBIF Backbone Taxonomy ####
# Should be searched by scientificName term

# Create an empty list for search queries
queries <- list()
# Populate the list
for (i in 1:nrow(goodmatch)) {
  queries[[i]] <- occ_download_prep(
    pred("scientificName",
            goodmatch[i, 4]),
    pred("hasCoordinate", TRUE),
    pred("occurrenceStatus","PRESENT"), 
    pred_not(pred_in("basisOfRecord",c("FOSSIL_SPECIMEN","LIVING_SPECIMEN"))),
    pred("country", "UA"),
    format = "DWCA"
    # user = gbif_user,
    # pwd = gbif_pwd,
    # email = gbif_email
  )
}

# Download occurrences by executing queries 
out <- occ_download_queue(.list = queries)

# Download occurrences as a list of tibbles to local drive, filter and mutate it
downloaded <- list()
for (i in 1:length(out)) {
  d <- occ_download_get(occ_download_meta(out[i])$key) %>%
    occ_download_import() %>% 
    select(gbifID,
           datasetKey,
           scientificName,
           verbatimScientificName,
           eventDate,
           decimalLatitude,
           decimalLongitude,
           coordinateUncertaintyInMeters,
           coordinatePrecision,
           iucnRedListCategory) %>% 
    mutate(ID = goodmatch[i, 1]) %>% 
    relocate(ID) %>% # relocate ID variable to the 1st place
    mutate_at("ID", as.integer) %>% 
    mutate_at("gbifID", bit64::as.integer64) %>% 
    mutate_at(c("datasetKey",
                "scientificName",
                "verbatimScientificName",
                "eventDate",
                "iucnRedListCategory"), as.character) %>% 
    mutate_at(c("decimalLatitude",
                "decimalLongitude",
                "coordinateUncertaintyInMeters",
                "coordinatePrecision"), as.double)
  
  downloaded[[i]] <- d
}

# Convert downloaded occurrences into a single tibble dataframe
occurrences_for_goodmatch <- downloaded  %>%  bind_rows()
save(occurrences_for_goodmatch, file = "occurrences_for_goodmatch.Rdata")
gc()
# load(file = "occurrences_for_badmatch.Rdata")

# End of script ####