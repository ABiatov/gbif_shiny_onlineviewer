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


# Load names that should be searched by verbatim scientific names
speciesnames <- read.csv("higherrank_nameVariants.csv") %>% 
  select(-X, -nameStringConcatenated)
# Resulting dataframe internal ID, which helps us later to merge their occurrence 
# data with occ. data for "goodmatch", and possible spellings for verbatim 
# scientific names for each taxon.


# Create an empty list
search_results <- list()

# Get existing occurrenceID for each name ID from badmatch 
for (i in 1:nrow(speciesnames)) {
  # Look up for occurrences
  # Returns tibble with occurrences if any, NULL if no occs found for the particular name
  df <- occ_search(verbatimScientificName = str_c(speciesnames[i, 2:ncol(speciesnames)], 
                                                 collapse = ";"),
                   hasCoordinate = TRUE,
                   country = "UA",
                   limit = 100000)$data
  
  # If the tibble in not empty, create ID column according to
  # previously defined name ID, and append it to the list.
  
  if (!is.null(df)) {
    df %>% 
      select(key) %>% 
      mutate(ID = speciesnames[i, 1]) %>%
      relocate(ID) -> search_results[[i]]
    print(paste0(paste0("Success for ID ", speciesnames[i, 1])))
    
  } else {
    print(paste0("No data for the scientificName ", speciesnames[i, 2], 
                 ", ID ", speciesnames[i, 1]))
  }
}


# Output of the previous step is a list of many, where each sub-list contains 
# occurrence IDs for the particular taxon.

# Merge results to get occurrence IDs associated with name IDs 
id.bad <- bind_rows(search_results) %>% 
  mutate_if(is.character, as.numeric)
# Results - occurrenceIDs (occurrence keys) for each name ID in badmatch dataframe
gc()


# Look up for occurrence IDs for successfully matched names
# Create an empty list
search_results <- list()

goodmatch$scientificName <- as.character(goodmatch$scientificName)

# Get existing occurrenceID for each name ID from goodmatch
for (i in 1:nrow(goodmatch)) {
  # Look up for occurrences
  # Returns tibble with occurrences if any, NULL if no occurrences found 
  # for the particular name
  df <- occ_search(scientificName = goodmatch[i, 3],
                   hasCoordinate = TRUE,
                   country = "UA",
                   limit = 100000)$data
  
  # If the tibble in not empty, create ID column according to
  # previously defined nameID, and append it to the list
  
  if (!is.null(df)) {
    df %>% 
      select(key) %>% 
      mutate(ID = goodmatch[i, 1]) %>%
      relocate(ID) -> search_results[[i]]
    print(paste0("Success for ID ", goodmatch[i, 1]))
    
  } else {
    print(paste0("No data for the scientificName ", goodmatch[i, 3], 
                 ", ID ", goodmatch[i, 1]))
  }
}

# Output of the previous step is a list of many, where each sub-list contains 
# occurrence IDs for the particular taxon.

# Merge results to get occurrence IDs associated with name IDs 
id.good <- bind_rows(search_results) %>% 
  mutate_if(is.character, as.numeric)
# Results - occurrenceIDs (occurrence keys) for each name ID in goodmatch dataframe


# Merge occurrenceIDs for the 'good' and 'bad' matches
occurrence.id <- id.good %>% bind_rows(id.bad)

save(occurrence.id, file = "occurrence_id.Rdata")
gc()
# rm(list = ls()) # Reset R`s brain
rm(id.bad, id.good, search_results)

# # Download extended occurrence data using extracted occurrenceIDs
# # Get vector with taxonKeys
# vector.occId <- id.bad$key
# 
# 
# 
# preforms the query
# response <- occ_download(
#   pred("gbifID", 2992081821),
#   pred("hasCoordinate", TRUE),
#   pred("occurrenceStatus","PRESENT"),
#   pred_not(pred_in("basisOfRecord",c("FOSSIL_SPECIMEN","LIVING_SPECIMEN"))),
#   pred("country", "UA"),  # TODO: Move "UA" to Config.R
#   # format = "SIMPLE_CSV",
#   format = "DWCA"
#   # user = gbif_user,
#   # pwd = gbif_pwd,
#   # email = gbif_email
# )
# 
# ?download_predicate_dsl
# 
# 
# # Retrieve download's metadata
# gbif_dataset_metadata <- occ_download_meta(response)
# 
# # desiccate metadata
# print(paste0("DOI: ", gbif_dataset_metadata$doi))
# print(paste0("https://doi.org/", gbif_dataset_metadata$doi)) # Example: https://doi.org/10.15468/dl.qpx7ya
# print(paste0("Dataset key: ", gbif_dataset_metadata$key))
# print(paste0("Download link: ", gbif_dataset_metadata$downloadLink))
# 
# 
# # Check download status with
# occ_download_wait(gbif_dataset_metadata$key)
# 
# # Create local archive with data on disk (project folder root directory)
# dump_dataset <- occ_download_get(gbif_dataset_metadata$key, overwrite = TRUE)
# 
# # Import occurrence data to the Environment
# occurrences_for_goodmatch <- occ_download_import(dump_dataset) %>%
#   select(taxonKey,
#          gbifID,
#          datasetKey,
#          scientificName,
#          verbatimScientificName,
#          eventDate,
#          decimalLatitude,
#          decimalLongitude,
#          coordinateUncertaintyInMeters,
#          coordinatePrecision,
#          iucnRedListCategory) %>%
#   mutate_at("gbifID", bit64::as.integer64) %>% 
#   mutate_at(c("datasetKey",
#               "scientificName",
#               "verbatimScientificName",
#               "eventDate"), as.character) %>% 
#   mutate_at(c("decimalLatitude",
#               "decimalLongitude",
#               "coordinateUncertaintyInMeters",
#               "coordinatePrecision"), as.double)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # Extract name ID and taxonKey for successfuly matched names
# goodmatch %>% 
#   select(ID, key) %>% 
#   write.csv("./name_lookup/taxonIDforGoodmatch.csv")
# # Results - taxonKey (in GBIF Backbode Taxonomy) for each name ID in goodmatch dataframe

# JW ####
library(rgbif)
library(dplyr)
library(purrr)

# get some gbif ids for example 
gbif_ids <- occ_search(country="UA",limit=20)$data$key 

gbif_ids

gbif_ids <- pull(occurrence.id[, 2])
gbif_ids[13648]


# get all of the occurrenceIds (which might take a while if you have a lot). 
occurrenceId1 <- gbif_ids[1:13647] %>%
  as.numeric() %>%
  map_chr(~ occ_get(.x, fields = "all")[[1]]$data$occurrenceID)

occurrenceId2 <- gbif_ids[13649:100000] %>%
  as.numeric() %>%
  map_chr(~ occ_get(.x, fields = "all")[[1]]$data$occurrenceID)

occurrenceId3 <- gbif_ids[100001:200000] %>%
  as.numeric() %>%
  map_chr(~ occ_get(.x, fields = "all")[[1]]$data$occurrenceID)

occurrenceId4 <- gbif_ids[200001:300000] %>%
  as.numeric() %>%
  map_chr(~ occ_get(.x, fields = "all")[[1]]$data$occurrenceID)

occurrenceId5 <- gbif_ids[300001:400000] %>%
  as.numeric() %>%
  map_chr(~ occ_get(.x, fields = "all")[[1]]$data$occurrenceID)

occurrenceId6 <- gbif_ids[400001:500000] %>%
  as.numeric() %>%
  map_chr(~ occ_get(.x, fields = "all")[[1]]$data$occurrenceID)

occurrenceId7 <- gbif_ids[500001:600000] %>%
  as.numeric() %>%
  map_chr(~ occ_get(.x, fields = "all")[[1]]$data$occurrenceID)

occurrenceId8 <- gbif_ids[600001:700000] %>%
  as.numeric() %>%
  map_chr(~ occ_get(.x, fields = "all")[[1]]$data$occurrenceID)

occurrenceId9 <- gbif_ids[700001:800000] %>%
  as.numeric() %>%
  map_chr(~ occ_get(.x, fields = "all")[[1]]$data$occurrenceID)

occurrenceId10 <- gbif_ids[800001:900000] %>%
  as.numeric() %>%
  map_chr(~ occ_get(.x, fields = "all")[[1]]$data$occurrenceID)

occurrenceId11 <- gbif_ids[900001:length(gbif_ids)] %>%
  as.numeric() %>%
  map_chr(~ occ_get(.x, fields = "all")[[1]]$data$occurrenceID)


occurrenceId <- c(occurrenceId1,
                  occurrenceId2,
                  occurrenceId3,
                  occurrenceId4,
                  occurrenceId5,
                  occurrenceId6,
                  occurrenceId7,
                  occurrenceId8,
                  occurrenceId9,
                  occurrenceId10,
                  occurrenceId11)

# Check for duplicated occurrenceIDs
length(occurrenceId)
length(occurrenceId[!duplicated(occurrenceId)])

# then send that to occ_download
occ_download(pred_in("occurrenceId", occurrenceId))


