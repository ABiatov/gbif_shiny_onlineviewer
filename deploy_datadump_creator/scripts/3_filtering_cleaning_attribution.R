# Environment preparation ####
rm(list = ls()) # Reset R`s brain

# Load libraries
library(tidyr)
library(dplyr)
library(sf)

## Import some variables
# source( "./scripts/config.R") 
source( paste0(Sys.getenv("APP_DIR"), "/config.R") )

# Make a custom operator "not in" for `dplyr` filtering
`%notin%` <- Negate(`%in%`)

# load country polygon
# country_polygon <- st_read("./data/shp/country.shp")
country_polygon <- st_read( paste0(Sys.getenv("INPUT_DATA_DIR"), "/shp/country.shp") )

# Load data saved at step #1
# load(file = "./temp/matches.Rdata")
load(file = paste0(Sys.getenv("TEMP_DATA_DIR"), "/matches.Rdata") )

list2env(matches, .GlobalEnv)
rm(matches)

# Restore original input data, but with internal IDs
attributes <- goodmatch %>% bind_rows(badmatch) %>% 
  arrange(ID)

# Drop unused objects
rm(goodmatch, badmatch)

# Load GBIF data prepared at the second step
# load(file = "./temp/gbif_data.Rdata")
load(file = paste0(Sys.getenv("TEMP_DATA_DIR"), "/gbif_data.Rdata") )

# Transform data
gbif_sf_dataset <- gbif.dump %>% 
  # rename columns
  rename(Latitude = decimalLatitude, Longitude = decimalLongitude) %>%
  # make new columns for occurrence and dataset Keys
  mutate(URL_record = paste0("https://www.gbif.org/occurrence/", as.character(gbifID) )) %>%
  mutate(URL_dataset = paste0("https://www.gbif.org/dataset/", datasetKey)) %>% 
  # drop records from grid datasets
  filter(datasetKey %notin% dropped_datasets) %>% 
  # drop occs w/ high georeference uncertainty
  filter(coordinateUncertaintyInMeters <= coordUncert.threshold | is.na(coordinateUncertaintyInMeters)) %>%
  filter(coordinatePrecision < coordinatePrec.threshold | is.na(coordinatePrecision)) %>% 
  # merge occurrences with attributes (local names, conservation lists)
  left_join(attributes, by = "ID") %>% 
  # drop redundant columns
  select(-c(ID,
            occurrenceID,
            gbifID,
            datasetKey,
            taxonKey.x,
            scientificName.x,
            verbatimScientificName.x,
            taxonKey.y,
            scientificName.y,
			      kingdom.y,		  
            status,
            matchType)) %>% 
  # leave only one scientific name column, with the name as in the input data
  rename(scientificName = verbatimScientificName.y) %>% 
  # leave only one kingdom column, with values as in GBIF
  rename(kingdom = kingdom.x) %>%								  
  # # coalesce IUCN Red List castegories across all sources category) and manually filled (IUCN))
  unite("iucnRListCat", c(iucnRedListCategory, IUCN), sep = "", na.rm = TRUE, remove = FALSE) %>% 
  select(-c(iucnRedListCategory, IUCN)) %>% 
  rename(iucnRedListCategory = iucnRListCat) %>%
    # drop IUCN Red List status for the species officially claimed as invasive in Ukraine
  mutate(iucnRedListCategory = replace(iucnRedListCategory,
                                       Invasive == "yes",
                                       NA)) %>% 
 
  # convert to `simple features` spatial object
  st_as_sf(dim = "XY", remove = FALSE, na.fail = F, 
                      coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs") %>% 
  # clip by polygon and drop unused column
  filter(st_intersects(geometry, country_polygon, sparse = FALSE))


# Load occurrence data for names not included to the input data, but have 
# IUCN RL category (except LC)
# load(file = "./temp/iucn_omitted.Rdata") 
load(file = paste0(Sys.getenv("TEMP_DATA_DIR"), "/iucn_omitted.Rdata") )

# Occurrences from IUCN Red List for species not included into the input data

gbif_iucn_sf_extradata <- iucn_omitted %>% 
  # rename columns
  rename(Latitude = decimalLatitude, Longitude = decimalLongitude) %>%
  # make new columns for occurrence and dataset Keys
  mutate(URL_record = paste0("https://www.gbif.org/occurrence/", as.character(gbifID) )) %>%
  mutate(URL_dataset = paste0("https://www.gbif.org/dataset/", datasetKey)) %>% 
  # drop records from grid datasets
  filter(datasetKey %notin% dropped_datasets) %>% 
  # drop occs w/ high georeference uncertainty
  filter(coordinateUncertaintyInMeters <= coordUncert.threshold | is.na(coordinateUncertaintyInMeters)) %>%
  filter(coordinatePrecision < coordinatePrec.threshold | is.na(coordinatePrecision)) %>% 
  # drop redundant columns
  select(-c(ID,
            occurrenceID,
            gbifID,
            datasetKey,
            taxonKey,
            verbatimScientificName)) %>% 
  # convert to `simple features` spatial object
  st_as_sf(dim = "XY", remove = FALSE, na.fail = F, 
           coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs") %>% 
  # clip by polygon and drop unused column
  filter(st_intersects(geometry, country_polygon, sparse = FALSE))

# Merge occurrence data
gbif_sf_dataset <- gbif_sf_dataset %>% 
  bind_rows(gbif_iucn_sf_extradata) %>%
  select(all_of(colnames_set0) ) 

# # Preview result
# library(ggplot2)
# ggplot() +
#   geom_sf(data = country_polygon, fill = "transparent", colour = "gray") +
#   geom_sf(data = gbif_sf_dataset, colour = "red") +
#   theme_bw()

# Save GBIF points to local drive as Robject
# save(gbif_sf_dataset, file = "./gbif_data/gbif_sf_dataset.Rdata")
save(gbif_sf_dataset, file = paste0(Sys.getenv("GBIF_DATA_DIR"), "/gbif_sf_dataset.Rdata") )

# write.csv(gbif_sf_dataset, file = "./gbif_data/gbif_sf_dataset.csv", row.names=TRUE)

# load(file = "./gbif_data/gbif_sf_dataset.Rdata")

# # Delete temporary files if they exist ####
# filestodelete <- list.files(path = "./temp")
# 
# for (i in 1:length(filestodelete)) {
#   if (file.exists(filestodelete[i])) {
#     file.remove(filestodelete[i])
#     cat("File deleted")
#   } else {
#     cat("No file found")
#   }
# }

# Clean the session
rm(list = ls())
gc()
