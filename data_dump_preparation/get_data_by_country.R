# Get GBIF data by specieses list from table in Country

# library(readxl)

# library(rvest)
library(httr)
library(tictoc)

library(rgbif)
library(dplyr)
library(sf)
# require(ggplot2)
# library(basemapR)

# Paths and variables ####
## Paths for load data
path_country_vector_layer <- "data_dump_preparation/shp/country.shp" # Country polygon, CRS WGS84 (EPSG: 4326)
path_gbif_credentials <- "data_dump_preparation/gbif_ini.R"
path_species_name_dataset <- "name_lookup/matches.Rdata"

## Paths for export result
path_gbif_response <- "data/gbif_response.Rdata"
path_gbif_dataset_metadata <- "data/gbif_dataset_metadata.Rdata"
path_gbif_sf_dataset <- "data/gbif_sf_dataset.Rdata"

## variables
country_code <- "UA"
taxon_id_field <- "key"

# Import needed support datasets ####

## Import GBIF credentials
source(path_gbif_credentials)

## Import species name dataset
load(file = path_species_name_dataset)
goodmatch <- matches[["goodmatch"]]
badmatch <- matches[["badmatch"]]

df_combined_specieses_status <- rbind(goodmatch, badmatch)

## import spatial data ####
country_polygon <- st_read(path_country_vector_layer)
class(country_polygon)

# Download data ####

## create vector of TaxonID
vector_specieses_taxonId = df_combined_specieses_status[[taxon_id_field]]

## preforms the query
response = occ_download(
  pred_in("taxonKey", vector_specieses_taxonId),
  pred_in("hasCoordinate", TRUE),
  pred_in("country", country_code),  
  user = gbif_user,
  pwd = gbif_pwd, 
  email = gbif_email
)

## get query metadata 
gbif_dataset_metadata <- occ_download_meta(response)

print(paste0("DOI: ", gbif_dataset_metadata$doi))
print(paste0("https://doi.org/", gbif_dataset_metadata$doi)) # Example: https://doi.org/10.15468/dl.qpx7ya
print(paste0("Dataset key: ", gbif_dataset_metadata$key))
print(paste0("Download link: ", gbif_dataset_metadata$downloadLink))

## save response and GBIF dataset metadata
save(response, file = path_gbif_response)
save(gbif_dataset_metadata, file = path_gbif_dataset_metadata)

# You need to wait for 20-25 minutes until the dataset is ready for the server.
# When dataset show by doi link you can continue script

# Checking if the GBIF dataset is ready ####
# URL for checking
url <- paste0("https://doi.org/", gbif_dataset_metadata$doi)


# Function to check if a page exists
check_page <- function(url) {
  response <- GET(url)
  return(status_code(response))
}

# Variable to keep track of the number of checks
attempts <- 0

# Loop to repeat checks
while (attempts < 10) {
  attempts <- attempts + 1
  
  # Page Existence Check
  status <- check_page(url)
  
  if (status == 200) {
    # If the page is present, download GBIF dataset
    damp_dataset <- occ_download_get(gbif_dataset_metadata$key)
    break
  } else {
    # If the page is missing, wait 5 minutes
    Sys.sleep(300)
  }
}

# If after 10 checks the page still does not appear
if (attempts == 10) {
  print("The page is taking too long to wait. Check if the address is correct.")
}

# Import downloaded dataset ####

df_dataset <-   occ_download_import(damp_dataset) %>%
  rename(Latitude = decimalLatitude, Longitude = decimalLongitude)

class(df_dataset)




