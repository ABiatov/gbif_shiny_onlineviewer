# ---------------------
# This script is fully automated and can be run in the background.
# The script takes Rdata/csv files created at the previous steps, then looks for
# gbifIDs (unique persistent identifier of any record published through GBIF).
# It also build a request to GBIF database and download all occurrences from the 
# territory of the country, which meet user-specified conditions. After that, 
# only those occurrences which belong to the mined gbifIDs are filtered.
# 
# Since GBIF controls species' IUCN Red List status on its own, and the number of
# taxa which possess some IUCN RL category is quite large, we didn't include IUCN 
# RL into the original data. Instead, we extract those data from GBIF downloads.
# We decided to exclude LC category since it contains a lot of species currently
# don't require special protection, which doesn't meet the main goal of the tool.
# 
# Inputs:
# 1) matches.Rdata - list of two with the result of name matching.
#   [[1]] - "goodmatch" - Names whose matching is satisfactory - we will then look 
#   for occurrences for these names using taxon keys.
#   [[2]] - "badmatch" - Names whose matching is supposed to be wrong/unsatisfactory.
# 
# 2) higherrank_nameVariants.csv - possible spelling variants / synonyms for the
#   names matched as 'higherrank' by GBIF matching tool.
# 
# Outputs:
# 1) gbifIDs.Rdata - dataframe with all existing gbifIDs, associated with the 
#   internal IDs, assigned automatically to each scientific name in the original data.
# 
# 2) metadata.Rdata - list of ..., containing metadata of GBIF occurrence download
#   (including DOI, download key, query date).
# 
# 3) gbif_data.Rdata - all existing occurrences, associated with the names (IDs)
#   from the original data.
# 
# 4) iucn_omitted.Rdata - all existing occurrences, don't associated with the 
#   names (IDs) from the original data set, but with species that have IUCN Red 
#   List category (except 'Least Concern', LC).
# 
# All output files are going to be stored within "./temp" directory.
# During GBIF data downloading, a large zip archive is created and stores in the
# "./temp" directory, too. The final part of the script automatically erase it 
# to prevent you running out of storage. But, if you work locally, or modify the
# code, it's recommended to keep it in case you will need to import the data during
# further R sessions and you don't want to perform the (quite large) query again. 


# Environment preparation ####
rm(list = ls()) # Reset R`s brain

# Load libraries
library(tidyr)
library(dplyr)
library(stringr)
library(rgbif)

## Import some variables
# source( "./scripts/config.R") 
source( paste0(Sys.getenv("APP_DIR"), "/config.R") )
## Import GBIF credentials
# source( "./scripts/gbif_ini.R") 
source( paste0(Sys.getenv("APP_DIR"), "/gbif_ini.R") )

# Load data saved at step #1
# load(file = "./temp_1/matches.Rdata")
load(file = paste0(Sys.getenv("TEMP_1_DATA_DIR"), "/matches.Rdata") )
list2env(matches, .GlobalEnv)
rm(matches)

# Taxa with bad match to GBIF Backbone Taxonomy ####
# Should be searched by verbatinScientificName term

# Load names that should be searched by verbatim scientific names
# speciesnames <- read.csv("./data/higherrank_nameVariants.csv") %>% 
speciesnames <- read.csv( paste0(Sys.getenv("INPUT_DATA_DIR"), "/higherrank_nameVariants.csv") ) %>%		
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
                   limit = occ_search_limit)$data
  
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
                   limit = occ_search_limit)$data
  
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
all.id <- good.id %>% bind_rows(bad.id) # %>% 
#  mutate_at("gbifID", bit64::as.integer64) # convert to integer64 format

# save(all.id, file = "./temp/gbifIDs.Rdata")
save(all.id, file = paste0(Sys.getenv("TEMP_2_DATA_DIR"), "/gbifIDs.Rdata") )
# load(file = "./temp_2/gbifIDs.Rdata") # if you need to read it from local drive

# Free unused R's memory and drop unused variables
gc()
rm(good.id, bad.id, search_results)


# Get occurrence data for all Ukrainian GBIF occurrences to date ####
# preforms the query
response <- occ_download(
  pred("hasCoordinate", TRUE),
  pred("occurrenceStatus","PRESENT"), 
  pred_not(pred("basisOfRecord", "FOSSIL_SPECIMEN")),
  pred("country", country), 
  format = "DWCA",
  # following arguments are needed only if you don't store your GBIF credentials
  # in either config file or R environment.
  user = gbif_user,
  pwd = gbif_pwd,
  email = gbif_email
)

# Retrieve download's metadata
gbif_dataset_metadata <- occ_download_meta(response)

# desiccate metadata, if needed
print(paste0("DOI: ", gbif_dataset_metadata$doi))
print(paste0("https://doi.org/", gbif_dataset_metadata$doi)) # Example: https://doi.org/10.15468/dl.qpx7ya
print(paste0("Dataset key: ", gbif_dataset_metadata$key))
print(paste0("Download link: ", gbif_dataset_metadata$downloadLink))


save(gbif_dataset_metadata,
#     file = "./gbif_data/metadata.Rdata")
	 file = paste0(Sys.getenv("GBIF_DATA_DIR"), "/metadata.Rdata") )
# load(file = "./gbif_data/metadata.Rdata") # if you need to read it from local drive
# Check download status with
occ_download_wait(gbif_dataset_metadata$key)

# Create local archive with data on disk (zip, stored at the project folder root)
dump_dataset <- occ_download_get(key = gbif_dataset_metadata$key,
#                                 path = "./temp_2",
								 path = Sys.getenv("TEMP_2_DATA_DIR"),
                                 overwrite = TRUE)


# In case you need to import data from the previously generated by 
# 'occ_download_get()' local zip archive, uncomment and run the following line
# dd <- occ_download_import(path = "./temp", key = gbif_dataset_metadata$key)
# then use `dd` as an input for the next code pipeline

# Import occurrence data to the Environment
all.occurrences <- occ_download_import(dump_dataset) %>%
  select(gbifID,
         occurrenceID,
         taxonKey,
         datasetKey,
         scientificName,
         verbatimScientificName,
         kingdom,
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
  # mutate_at("gbifID", bit64::as.integer64) %>%
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
# # `gbifID` var should be in the same type (integer64) for both data frames
# all.occurrences$gbifID <- bit64::as.integer64(all.occurrences$gbifID)		 
gbif.dump <- all.occurrences %>% 
  inner_join(all.id) %>% 
  relocate(ID) %>% 
  arrange(ID)


# Save occurrence data to local drive
# save(gbif.dump, file = "./temp_2/gbif_data.Rdata")
save(gbif.dump, file = paste0(Sys.getenv("TEMP_2_DATA_DIR"), "/gbif_data.Rdata") )																			 
# load(file = "./temp_2/gbif_data.Rdata") # if you need to read it from the local file later


# IUCN Red List species omitted by input data (not in the original species list),
# but present in the country download.
# LC category is dropped
iucn_omitted <- all.occurrences %>% 
  filter(iucnRedListCategory %in% iucnRL_category) %>% 
  left_join(all.id) %>% 
  filter(is.na(ID))

# Save occurrence data for names not included to the input data, but have IUCN RL
# category (except LC)
# save(iucn_omitted, file = "./temp_2/iucn_omitted.Rdata")
save(iucn_omitted, file = paste0(Sys.getenv("TEMP_2_DATA_DIR"), "/iucn_omitted.Rdata") )

# Clean-up the session ####

# # Delete zip archives with occurrence downloads
# # List all files matched pattern "zip" in the project directory
# downloaded_files <- list.files(path = "./temp_2", pattern = "zip")
# # Delete all listed files
# for (i in 1:length(downloaded_files)) {
#   if (file.exists(downloaded_files[i])) {
#     file.remove(downloaded_files[i])
#     cat("File deleted")
#   } else {
#     cat("No file found")
#   }
# }

rm(list = ls()) # Reset R`s brain
gc()            # Free unused R's memory

# End of script ####
