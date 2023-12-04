# -----------------------
# This script is fully automated and can be run in the background.
# The script takes occurrence data, obtained on the previous step, and merge it
# with the attributes from the original data. As a result, we obtain a `simple 
# features` spatial data frame, containing georeferenced occurrences unambiguously
# associated with the conservation status of the taxa they belong to.
# 
# Input:
# 1) country.shp - ESRI shapefile containing the boundaries of the target country
#   (Ukraine by default)
# 
# 2) matches.Rdata - list of two with the result of name matching.
#   [[1]] - "goodmatch" - Names whose matching is satisfactory - we will then look 
#   for occurrences for these names using taxon keys.
#   [[2]] - "badmatch" - Names whose matching is supposed to be wrong/unsatisfactory.
# 
# 3) gbif_data.Rdata - all existing occurrences, associated with the names (IDs)
#   from the original data.
# 
# 4) iucn_omitted.Rdata - all existing occurrences, don't associated with the 
#   names (IDs) from the original data set, but with species that have IUCN Red 
#   List category (except 'Least Concern', LC).
# 
# Outputs:
# 1) gbif_sf_dataset.Rdata - all occurrences associated with the scientific names
#   both from the original data and those assigned to the IUCN Red List categories. 
#   Сonservation status according to the involved conservation lists are included.
# 
# 2) datapoints_.png - image file with all spatial points hexplot, for illustrative
#   purposes
# 
# The final part of the script automatically erase "./temp" folder, where all 
# intermediate files are stored, to prevent you running out of storage. But, if 
# you work locally, or modify the code, it's recommended to keep it in case you 
# will need to import the data during further R sessions and you don't want to 
# perform the (quite large) query again.




# Environment preparation ####
rm(list = ls()) # Reset R`s brain

# Load libraries
library(tidyr)
library(dplyr)
library(sf)
library(rgbif)
## Import some variables
# source( "./scripts/config.R") 
source( paste0(Sys.getenv("APP_DIR"), "/config.R") )

# Make a custom operator "not in" for `dplyr` filtering
`%notin%` <- Negate(`%in%`)

# load country polygon
# country_polygon <- st_read("./data/shp/country.shp")
country_polygon <- st_read( paste0(Sys.getenv("INPUT_DATA_DIR"), "/shp/country.shp") )


# Load data saved at step 1 ####
# load(file = "./temp_1/matches.Rdata")
load(file = paste0(Sys.getenv("TEMP_1_DATA_DIR"), "/matches.Rdata") )

list2env(matches, .GlobalEnv)
rm(matches)

# Restore original input data, but with internal IDs
attributes <- goodmatch %>% bind_rows(badmatch) %>% 
  arrange(ID)

# Drop unused objects
rm(goodmatch, badmatch)

# Load GBIF data prepared at the second step
# load(file = "./temp_2/gbif_data.Rdata")
load(file = paste0(Sys.getenv("TEMP_2_DATA_DIR"), "/gbif_data.Rdata") )


# Transform data into the `sf` ####
gbif_sf_dataset <- gbif.dump %>% 
  # rename columns
  rename(Latitude = decimalLatitude, Longitude = decimalLongitude) %>%
  # make new columns for occurrence and dataset Keys
  mutate(URL_record = paste0("https://www.gbif.org/occurrence/", gbifID)) %>%
  # mutate(URL_record = paste0("https://www.gbif.org/occurrence/", as.character(gbifID) )) %>%
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
  # # coalesce IUCN Red List categories across all sources category) and manually filled (IUCN))
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
# load(file = "./temp_2/iucn_omitted.Rdata")
load(file = paste0(Sys.getenv("TEMP_2_DATA_DIR"), "/iucn_omitted.Rdata") )														

# Occurrences from IUCN Red List for species not included into the input data
gbif_iucn_sf_extradata <- iucn_omitted %>% 
  # rename columns
  rename(Latitude = decimalLatitude, Longitude = decimalLongitude) %>%
  # make new columns for occurrence and dataset Keys
  mutate(URL_record = paste0("https://www.gbif.org/occurrence/", gbifID)) %>%
  # mutate(URL_record = paste0("https://www.gbif.org/occurrence/", as.character(gbifID) )) %>%
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
           coords = c("Longitude", "Latitude"), 
           crs = "+proj=longlat +datum=WGS84 +no_defs") %>% 
  # clip by polygon and drop unused column
  filter(st_intersects(geometry, country_polygon, sparse = FALSE))

# Since gbif_iucn_sf_extdata lacks data on higher taxonomy, we use GBIF species
# lookup tool to retrieve it

# Extract only taxon names and kingdom
names_iucn_extdata <- gbif_iucn_sf_extradata %>% 
  select(scientificName, kingdom) %>% 
  rename(name = scientificName) %>% 
  st_drop_geometry()

# Matching names against GBIF Backbone Taxonomy and tidying data
gbif_iucn_sf_extradata <- name_backbone_checklist(names_iucn_extdata) %>%
  select(c(phylum,
           class,
           family)) %>% 
  # Add higher taxonomy to the `gbif_iucn_sf_extradata`
  bind_cols(gbif_iucn_sf_extradata)

# Combine occurrence data together ####
gbif_sf_dataset <- gbif_sf_dataset %>% 
  bind_rows(gbif_iucn_sf_extradata) %>% 
  # Check for possible duplicates
  distinct(URL_record, .keep_all = TRUE)

# Save GBIF points to local drive as Robject
# save(gbif_sf_dataset, file = "./gbif_data/gbif_sf_dataset.Rdata")
save(gbif_sf_dataset, file = paste0(Sys.getenv("GBIF_DATA_DIR"), "/gbif_sf_dataset.Rdata") )

# Save GBIF points to local drive as FlatGeobuf
# write_sf(gbif_sf_dataset, 
#          dsn="./gbif_data/gbif_sf_dataset.fgb",
#          driver="FlatGeobuf",
#          delete_dsn= TRUE)

write_sf(gbif_sf_dataset, 
         dsn=paste0(Sys.getenv("GBIF_DATA_DIR"), "/gbif_sf_dataset.fgb"),
         driver="FlatGeobuf",
         delete_dsn= TRUE)


# load(file = "./gbif_data/gbif_sf_dataset.Rdata")

# # Preview the result ####
# library(ggplot2)
# library(hexbin)
# ggplot() +
#   geom_sf(data = country_polygon, fill = "transparent", colour = "gray") +
#   # geom_sf(data = gbif_sf_dataset, colour = "red") +
#   geom_hex(data = gbif_sf_dataset, aes(x = Longitude, y = Latitude),
#            bins = 800) +
#   scale_fill_viridis_c() +
#   theme_bw()
# 
# # Save the figure
# ggsave(paste0("./gbif_data/datapoints_", Sys.Date(), ".png"),
#        width = 16, height = 12, units = "cm", dpi = 300)



# # Delete temporary files and clean the session ####
# pathtothetempfolder <- "./temp_2/"
# filestodelete <- list.files(path = "./temp_2")

# for (i in 1:length(filestodelete)) {
#   if (file.exists(paste0(pathtothetempfolder, filestodelete[i]))) {
#     file.remove(paste0(pathtothetempfolder, filestodelete[i]))
#     cat("File deleted")
#   } else {
#     cat("No file found")
#   }
# }

# Clean the session
rm(list = ls())
gc()

# End of script ####