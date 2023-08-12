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

# Taxa with bad match to GBIF Backbone Taxonomy ####
# Should be searched by verbatinScientificName term

# Load names that should be searched by verbatim scientific names
speciesnames <- read.csv("higherrank_nameVariants.csv") %>% 
  select(-X, -nameStringConcatenated)
# Resulting dataframe internal ID, which helps us later to merge their occurrence 
# data with occ. data for "goodmatch", and possible spellings for verbatim 
# scientific names for each taxon.


# Create an empty list
search_results <- list()

# Get existing occurrences for each name ID from the 'badmatch' 
for (i in 1:nrow(speciesnames)) {
  # Look up for occurrences
  # Returns tibble with occurrences if any, NULL if no occs found for the particular name
  df <- occ_search(verbatimScientificName = str_c(speciesnames[i, 2:ncol(speciesnames)], 
                                                  collapse = ";"),
                   hasCoordinate = TRUE,
                   country = "UA",
                   fields = "all",
                   limit = 100000)$data
  
  # If the tibble in not empty, create ID column according to
  # previously defined name ID, and append it to the list.
  
  if (!is.null(df)) {
    df %>% 
      mutate(ID = speciesnames[i, 1]) %>%
      relocate(ID) -> search_results[[i]]
    print(paste0(paste0("Success for  the scientificName ", speciesnames[i, 2], 
                        ", ID ", speciesnames[i, 1])))
    
  } else {
    print(paste0("No data for the scientificName ", speciesnames[i, 2], 
                 ", ID ", speciesnames[i, 1]))
  }
}


# Output of the previous step is a list of many, where each sub-list contains 
# occurrence IDs for the particular taxon.


# Merge results to get occurrence IDs associated with name IDs 
occ.badmatch <- bind_rows(search_results) %>% 
         select(ID,
                gbifID,
                datasetKey,
                scientificName,
                taxonKey,
                year,
                eventDate,
                decimalLatitude,
                decimalLongitude,
                coordinateUncertaintyInMeters)
# Results - occurrences for each name ID in the 'badmatch' dataframe

# Check data visually
occ.badmatch %>% glimpse()

gc() # Clean up unused memory

# Taxa with good match to GBIF Backbone Taxonomy ####
# Should be searched by scientificName term

# Look up for occurrences for successfully matched names
# Create an empty list
search_results <- list()

goodmatch$scientificName <- as.character(goodmatch$scientificName)
i = 1
# Get existing occurrenceID for each name ID from goodmatch
for (i in 1:nrow(goodmatch)) {
  # Look up for occurrences
  # Returns tibble with occurrences if any, NULL if no occurrences found 
  # for the particular name
  df <- occ_search(scientificName = goodmatch[i, 3],
                   hasCoordinate = TRUE,
                   country = "UA",
                   fields = "all",
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
occ.goodmatch <- bind_rows(search_results) %>% 
select(ID,
         gbifID,
         datasetKey,
         scientificName,
         taxonKey,
         year,
         eventDate,
         decimalLatitude,
         decimalLongitude,
         coordinateUncertaintyInMeters)
# Results - occurrences for each name ID in the 'goodmatch' dataframe


# Merge occurrenceIDs for the 'good' and 'bad' matches
occurrences.all <- occ.goodmatch %>% bind_rows(occ.badmatch)

save(occurrences.all, file = "occurrences_all.Rdata")
gc()
# rm(list = ls()) # Reset R`s brain
rm(occ.goodmatch, occ.badmatch, search_results)


load(file = "occurrences_all.Rdata")
# Match data with attributes ####
full.data <- goodmatch %>% 
  bind_rows(badmatch) %>% 
  arrange(ID) %>% 
  right_join(occurrences.all, by = "ID") 
# %>% 
#   select(-c(scientificName,
#            status,
#            matchType))

# Export data to the local drive
save(full.data, file = "dump.Rdata")

# Get metadata for all Ukrainian GBIF occurrences to date ####
# preforms the query
response <- occ_download(
  pred("hasCoordinate", TRUE),
  pred("occurrenceStatus","PRESENT"), 
  pred_not(pred_in("basisOfRecord",c("FOSSIL_SPECIMEN","LIVING_SPECIMEN"))),
  pred("country", "UA"),  # TODO: Move "UA" to Config.R
  format = "DWCA"
  # user = gbif_user,
  # pwd = gbif_pwd,
  # email = gbif_email
)

# Retrieve download's metadata
gbif_dataset_metadata <- occ_download_meta(response)

save(gbif_dataset_metadata,
     file = "metadata.Rdata")


# desiccate metadata
print(paste0("DOI: ", gbif_dataset_metadata$doi))
print(paste0("https://doi.org/", gbif_dataset_metadata$doi)) # Example: https://doi.org/10.15468/dl.qpx7ya
print(paste0("Dataset key: ", gbif_dataset_metadata$key))
print(paste0("Download link: ", gbif_dataset_metadata$downloadLink))


# Write output as text file
sink(file = "MoransI_residuals.txt")
morans_spdep_kern_df
sink(file = NULL)



# Clean-up the session ####
rm(list = ls()) # Reset R`s brain
gc()

# End of script ####
