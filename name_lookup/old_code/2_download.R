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


# Taxa with good match to GBIF Backbone Taxonomy ####
# Should be searched by taxonKey term

# Get vector with taxonKeys
vector_species_taxonId <- goodmatch$taxonKey

# preforms the query
response <- occ_download(
  pred_in("taxonKey", vector_species_taxonId),
  pred("hasCoordinate", TRUE),
  pred("occurrenceStatus","PRESENT"), 
  pred_not(pred_in("basisOfRecord",c("FOSSIL_SPECIMEN","LIVING_SPECIMEN"))),
  pred("country", "UA"),  # TODO: Move "UA" to Config.R
  # format = "SIMPLE_CSV",
  format = "DWCA"
  # user = gbif_user,
  # pwd = gbif_pwd,
  # email = gbif_email
)

# Retrieve download's metadata
gbif_dataset_metadata <- occ_download_meta(response)

# desiccate metadata
print(paste0("DOI: ", gbif_dataset_metadata$doi))
print(paste0("https://doi.org/", gbif_dataset_metadata$doi)) # Example: https://doi.org/10.15468/dl.qpx7ya
print(paste0("Dataset key: ", gbif_dataset_metadata$key))
print(paste0("Download link: ", gbif_dataset_metadata$downloadLink))


# Check download status with
occ_download_wait(gbif_dataset_metadata$key)

# Create local archive with data on disk (project folder root directory)
dump_dataset <- occ_download_get(gbif_dataset_metadata$key, overwrite = TRUE)

# Import occurrence data to the Environment
occurrences_for_goodmatch <- occ_download_import(dump_dataset) %>%
  select(taxonKey,
         gbifID,
         datasetKey,
         scientificName,
         verbatimScientificName,
         eventDate,
         decimalLatitude,
         decimalLongitude,
         coordinateUncertaintyInMeters,
         coordinatePrecision,
         iucnRedListCategory) %>%
  mutate_at("gbifID", bit64::as.integer64) %>% 
  mutate_at(c("datasetKey",
              "scientificName",
              "verbatimScientificName",
              "eventDate"), as.character) %>% 
  mutate_at(c("decimalLatitude",
              "decimalLongitude",
              "coordinateUncertaintyInMeters",
              "coordinatePrecision"), as.double)



# Export occurrences to local drive
save(occurrences_for_goodmatch, file = "occurrences_for_goodmatch.Rdata")
# Free unused R's memory
gc()

# Synonyms and children taxa ####
# For the names with matchType one of "ACCEPTED" or "DOUBTFUL"  
# GBIF's occ_download() gets all occurrences for both the target (matched)
# taxonKey and all taxonKeys belong to their synonyms/children.
# Therefore, to know ID for each downloaded occurrence, we need to know
# which taxonKeys correspond to each taxonKey from our initial matching data frame.

## Synonyms ####
synonyms <- list()
for (i in 1:nrow(goodmatch)) {
  # Look up all synonyms according to GBIF Backbone Taxonomy
  df <- name_usage(key = goodmatch$taxonKey[i],
                   data = "synonyms")$data %>% 
    # select(key) %>% 
    # rename(taxonKey = key) %>% 
    mutate(ID = goodmatch$ID[i]) %>% 
    relocate(ID) %>% # relocate ID variable to the 1st place
    mutate_at("ID", as.integer)
  
  if (nrow(df) == 0) {
    df <- tibble(ID = goodmatch$ID[i],
                 key = goodmatch$taxonKey[i])
  } 

  # Append to the list
  synonyms[[i]] <- df
}

# Decompose list and append its content into a single data frame
synonyms_df <- synonyms %>% bind_rows() %>% 
  select(ID, key) %>% 
  rename(taxonKey = key)

## Children taxa ####
children <- list()
for (i in 1:nrow(goodmatch)) {
  # Look up all children according to GBIF Backbone Taxonomy
  df <- name_usage(key = goodmatch$taxonKey[i],
                   data = "children")$data %>%
    mutate(ID = goodmatch$ID[i]) %>% 
    relocate(ID) %>% # relocate ID variable to the 1st place
    mutate_at("ID", as.integer)
  
  if (nrow(df) == 0) {
    df <- tibble(ID = goodmatch$ID[i],
                 key = goodmatch$taxonKey[i])
  } 
  
  # Append to the list
  children[[i]] <- df
}

# Decompose list and append its content into a single data frame
children_df <- children %>% bind_rows() %>% 
  select(ID, key) %>% 
  rename(taxonKey = key)

# Merge synonyms with children taxa
synonyms.children <- synonyms_df %>% bind_rows(children_df) %>% 
  distinct() # remove duplicates



# Save it
save(synonyms.children, file = "synonyms_children.Rdata")
gc()
# load(file = "synonyms_children.Rdata")

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

# Merge downloaded occurrences with attribute information ####

# Merge occurrences with attribute data by ID
# 'Goodmatch' names (accepted, doubtful, synonyms)
alldata.goodmatch <- occurrences_for_goodmatch %>% 
  select(-c(
    scientificName,
    verbatimScientificName
  )) %>%
  left_join(goodmatch, by = "taxonKey",
            # relationship = "many-to-many"
            ) %>% 
  select(-c(status,
            matchType)) %>% 
  # Fill missing IDs for synonyms
  left_join(synonyms.children, by = "taxonKey", 
            # relationship = "many-to-many"
            ) %>% 
  mutate(
    ID = coalesce(ID.x, ID.y)
    , .keep = "unused" # Remove temporary columns ended with .x and .y
  ) %>% 
  relocate(ID) %>% 
  filter(!is.na(ID)) # Get rid of unmatched rows
  
# ---------
alldata.goodmatch <- occurrences_for_goodmatch %>% 
  select(-c(
    scientificName,
    verbatimScientificName
  )) %>%
  left_join(goodmatch, by = "taxonKey",
            # relationship = "many-to-many"
  ) %>% 
  select(-c(status,
            matchType)) %>% 
  # # Fill missing IDs for synonyms
  # left_join(synonyms.children, by = "taxonKey", 
  #           # relationship = "many-to-many"
  # ) %>% 
  # mutate(
  #   ID = coalesce(ID.x, ID.y)
  #   , .keep = "unused" # Remove temporary columns ended with .x and .y
  # ) %>% 
  # filter(!is.na(ID)) %>%  # Get rid of unmatched rows
  relocate(ID)

# All data for the names GBIF matching tool understand as accepted
alldata.accepted.names <- alldata.goodmatch %>% 
  filter(!is.na(ID))  # Get rid of unmatched rows

# All data for the names GBIF matching tool understand as synonyms or children names
alldata.synonyms.children <- alldata.goodmatch %>% 
  filter(is.na(ID)) %>% 
  # Fill missing IDs for synonyms
  left_join(synonyms.children, by = "taxonKey",
            # relationship = "many-to-many"
  )
  mutate(
    ID = coalesce(ID.x, ID.y)
    , .keep = "unused" # Remove temporary columns ended with .x and .y
  )

# Merge all data for accepted names, synonyms, and children names
alldata.goodmatch1 <- alldata.accepted.names %>% 
  bind_rows(alldata.synonyms.children)

sum(is.na(alldata.goodmatch$ID))
sum(is.na(alldata.accepted.names$ID))
sum(is.na(alldata.synonyms.children$ID))

library(ggplot2)
alldata.goodmatch %>% 
  ggplot(aes(x = ID)) +
  geom_histogram()





occurrences_for_goodmatch[3143,]
checki <- goodmatch %>% filter(taxonKey == 2769247)

occurrences_for_goodmatch[3143,]
checkit <- synonyms.children %>% filter(taxonKey == 3921842)
alldata.goodmatch %>% filter(ID == 2454)
# --------

# 'Badmatch' names (higherrank)
alldata.badmatch <- occurrences_for_badmatch %>% 
  select(-scientificName,
         -verbatimScientificName) %>% 
  left_join(badmatch, by = "ID") %>% 
  select(-c(
    # scientificName,
            status,
            matchType))

# Merge all occurrence and conservation data together
alldata <- alldata.goodmatch %>% bind_rows(alldata.badmatch) %>% 
  mutate_at("ID", as.integer) %>% 
  mutate_at(c(
    "verbatimScientificName",
    "kingdom",
    "nameUk"), as.factor)

alldata %>% glimpse()

# Pack it into a named list and write to the disk
gbif_dump_dataset <- list(alldata, gbif_dataset_metadata)
names(gbif_dump_dataset) <- c("data", "metadata")
save(gbif_dump_dataset, file = "gbif_dump_dataset.Rdata")

# One can address data as
# gbif_dump_dataset$data

# And metadata as
# gbif_dump_dataset$metadata


# Delete temporary files if it exist ####
filestodelete <- c("higherrank.csv",
                   "matches.Rdata",
                   "metadata.Rdata",
                   "occurrences_for_goodmatch.Rdata",
                   "occurrences_for_badmatch.Rdata",
                   "synonyms_children.Rdata")

for (i in 1:length(filestodelete)) {
  if (file.exists(filestodelete[i])) {
    file.remove(filestodelete[i])
    cat("File deleted")
  } else {
    cat("No file found")
  }
}

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

# End of script ####


# Citations
gbif_citation(x='0ec3229f-2b53-484e-817a-de8ceb1fce2b')
