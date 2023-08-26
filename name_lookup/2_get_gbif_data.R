# Environment preparation ####
rm(list = ls()) # Reset R`s brain

# Load libraries
library(tidyr)
library(dplyr)
library(stringr)
library(rgbif)

# Country code to download GBIF data
country <- "UA"

## Import GBIF credentials
source("gbif_ini.R")
# Load data saved at step #1
load(file = "./temp/matches.Rdata")
list2env(matches, .GlobalEnv)
rm(matches)

# Taxa with bad match to GBIF Backbone Taxonomy ####
# Should be searched by verbatinScientificName term

# Load names that should be searched by verbatim scientific names
speciesnames <- read.csv("./data/higherrank_nameVariants.csv") %>% 
  select(-X)
# Resulting dataframe internal ID, which helps us later to merge their occurrence 
# data with occ. data for "goodmatch", and possible spellings for verbatim 
# scientific names for each taxon.


# Create an empty list
search_results <- list()

# Get existing gbifID for the occurrences for each name ID from the 'badmatch' 
for (i in 1:nrow(speciesnames)) {
  # Look up for occurrences
  # Returns tibble with occurrences if any, NULL if no occs found for the particular name
  df <- occ_search(verbatimScientificName = str_c(speciesnames[i, 2:ncol(speciesnames)], 
                                                  collapse = ";"),
                   hasCoordinate = TRUE,
                   country = country,
                   fields = "gbifID",
                   limit = 100000)$data
  
  # If the tibble in not empty, create ID column according to
  # previously defined name ID, and append it to the list.
  
  if (!is.null(df)) {
    df %>% 
      mutate(ID = speciesnames[i, 1]) %>%
      relocate(ID) -> search_results[[i]]
    print(paste0(paste0("Success for the scientificName ", speciesnames[i, 2], 
                        ", ID ", speciesnames[i, 1])))
    
  } else {
    print(paste0("No data for the scientificName ", speciesnames[i, 2], 
                 ", ID ", speciesnames[i, 1]))
  }
}


# Output of the previous step is a list of many, where each sub-list contains 
# occurrence IDs for the particular taxon.


# Merge results to get occurrence IDs (gbifID) associated with name IDs 
bad.id <- bind_rows(search_results)
# Results - gbifID for each name ID in the 'badmatch' dataframe

gc() # Clean up unused memory

# Taxa with good match to GBIF Backbone Taxonomy ####
# Should be searched by scientificName term

# Look up for occurrences for successfully matched names
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
                   country = country,
                   fields = "gbifID",
                   limit = 100000)$data
  
  # If the tibble in not empty, create ID column according to
  # previously defined nameID, and append it to the list
  
  if (!is.null(df)) {
    df %>% 
      mutate(ID = goodmatch[i, 1]) %>%
      relocate(ID) -> search_results[[i]]
      print(paste0("Success for the scientificName ", goodmatch[i, 3], 
                 ", ID ", goodmatch[i, 1]))
    
  } else {
    print(paste0("No data for the scientificName ", goodmatch[i, 3], 
                 ", ID ", goodmatch[i, 1]))
  }
}

# Output of the previous step is a list of many, where each sub-list contains 
# occurrences for the particular taxon.

# Merge results to get occurrence IDs associated with name IDs
good.id <- bind_rows(search_results)
# Results - gbifIDs for each name ID in the 'goodmatch' dataframe


# Merge gbifIDs for the 'good' and 'bad' matches
all.id <- good.id %>% bind_rows(bad.id) %>% 
  mutate_at("gbifID", bit64::as.integer64) # convert to integer64 format

save(all.id, file = "./temp/gbifIDs.Rdata")
# load(file = "./temp/gbifIDs.Rdata") # if you need to write it from local drive
gc()
# rm(list = ls()) # Reset R`s brain
rm(good.id, bad.id, search_results)


# Get occurrence data for all Ukrainian GBIF occurrences to date ####
# preforms the query
response <- occ_download(
  pred("hasCoordinate", TRUE),
  pred("occurrenceStatus","PRESENT"), 
  pred_not(pred("basisOfRecord", "FOSSIL_SPECIMEN")),
  pred("country", country), 
  format = "DWCA"
  # user = gbif_user,
  # pwd = gbif_pwd,
  # email = gbif_email
)

# Retrieve download's metadata
gbif_dataset_metadata <- occ_download_meta(response)

# desiccate metadata, if needed
print(paste0("DOI: ", gbif_dataset_metadata$doi))
print(paste0("https://doi.org/", gbif_dataset_metadata$doi)) # Example: https://doi.org/10.15468/dl.qpx7ya
print(paste0("Dataset key: ", gbif_dataset_metadata$key))
print(paste0("Download link: ", gbif_dataset_metadata$downloadLink))


save(gbif_dataset_metadata,
     file = "./outputs/metadata.Rdata")
# load(file = "./outputs/metadata.Rdata")
# Check download status with
occ_download_wait(gbif_dataset_metadata$key)

# Create local archive with data on disk (project folder root directory)
dump_dataset <- occ_download_get(gbif_dataset_metadata$key, overwrite = TRUE)

# Import occurrence data to the Environment
all.occurrences <- occ_download_import(dump_dataset) %>%
  select(gbifID,
         occurrenceID,
         taxonKey,
         datasetKey,
         scientificName,
         verbatimScientificName,
         kingdom, #check is need it ?
         individualCount,
         organismQuantity,
         organismQuantityType,
         eventDate,
         year,
         decimalLatitude,
         decimalLongitude,
         coordinateUncertaintyInMeters,
         coordinatePrecision,
         verbatimLocality,
         iucnRedListCategory,
         license) %>% 
  mutate_at(c("gbifID",
              "datasetKey",
              "scientificName",
              "verbatimScientificName",
              "eventDate",
              "verbatimLocality",
              "iucnRedListCategory",
              "license"), as.character) %>% 
  mutate_at(c("decimalLatitude",
              "decimalLongitude",
              "coordinateUncertaintyInMeters",
              "coordinatePrecision"), as.double)


# Filter all downloaded occurrences by gbifIDs, generated before, and joint it
# with internal IDs.
# `gbifID` var should be in the same type (integer64) for both data frames
gbif.dump <- all.occurrences %>% 
  inner_join(all.id) %>% 
  relocate(ID) %>% 
  arrange(ID)


# Save occurrence data to local drive
save(gbif.dump, file = "./temp/gbif_data.Rdata")
# load(file = "./temp/gbif_data.Rdata")


# IUCN Red List species omitted by input data (not in the original species list),
# but present in the country download.
# LC category is dropped
iucn_omitted <- all.occurrences %>% 
  filter(iucnRedListCategory %in% c("EX", "EW", "CR", "EN", "VU", "NT", "DD")) %>% 
  left_join(all.id) %>% 
  filter(is.na(ID))

# Save occurrence data for names not included to the input data, but have IUCN RL
# category (except LC)
save(iucn_omitted, file = "./temp/iucn_omitted.Rdata")

# Clean-up the session ####

# Delete zip archives with occurrence downloads
# List all files matched pattern "zip" in the project directory
downloaded_files <- list.files(path = ".", pattern = "zip")
# Delete all listed files
for (i in 1:length(downloaded_files)) {
  if (file.exists(downloaded_files[i])) {
    file.remove(downloaded_files[i])
    cat("File deleted")
  } else {
    cat("No file found")
  }
}

rm(list = ls()) # Reset R`s brain
gc()            # Free unused R's memory

# End of script ####
