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
taxon_id_gbif <- "speciesKey"

colnames_set0 <- c(
  "gbifID",
  # "bibliographicCitation", "identifier", "license", 
  "publisher", 
  # "references",  "rightsHolder", "type", "institutionID", "collectionID", "datasetID", 
  # "institutionCode", "collectionCode", "datasetName", "ownerInstitutionCode", 
  "basisOfRecord", 
  # "informationWithheld",
  # "occurrenceID", "catalogNumber", "recordNumber", "recordedBy", "recordedByID", "individualCount",
  # "organismQuantity", "organismQuantityType", "sex", "lifeStage", "reproductiveCondition", "behavior",
  # "establishmentMeans", "degreeOfEstablishment", "georeferenceVerificationStatus", "occurrenceStatus",
  # "preparations", "disposition", "associatedReferences", "associatedSequences", "otherCatalogNumbers",
  # "occurrenceRemarks", "organismID", "organismScope", "materialSampleID", "eventID", "parentEventID",
  # "eventDate", "eventTime", 
  "year", 
  # "month", "day", 
  "verbatimEventDate", 
  # "habitat", "samplingProtocol",
  # "sampleSizeValue", "sampleSizeUnit", "samplingEffort", "fieldNotes", "eventRemarks", "locationID",
  # "waterBody", "locality", 
  "verbatimLocality", 
  "Latitude", "Longitude", "coordinateUncertaintyInMeters", "issue",
  # "footprintWKT", "identifiedBy", "dateIdentified", 
  "taxonID", "acceptedNameUsageID",
  # "parentNameUsageID", 
  "scientificName", "kingdom", "phylum", "class", "order", "family", "genus",
  "genericName", "infragenericEpithet", "specificEpithet", "infraspecificEpithet", "taxonRank",
  "vernacularName", "taxonomicStatus", "datasetKey", "publishingCountry",
  "lastInterpreted", # "issue",
  # "mediaType",
  "taxonKey", "acceptedTaxonKey", "kingdomKey", "phylumKey", "classKey", "orderKey",
  "familyKey", "genusKey", "speciesKey", "species", "acceptedScientificName", "verbatimScientificName",
  "typifiedName", 
  "iucnRedListCategory",
  "nameUk", "BernAppendix2", "BernAppendix3", "Bonn", "AEWA", 
  # "IUCN", 
  "BernResolution6", "ЧКУ", 
  "BernAppendix1", "CITES", "EUROBATS",      
  "ACCOBAMS", "BirdsDirective", "HabitatsDirective",
  "Invasive", "ЧС_Полтавська", "ЧС_Чернівецька", 
  "ЧС_Житомирська", "ЧС_Вінницька", "ЧС_Харківська", 
  "ЧС_Чернігівська", "ЧС_Черкаська", "ЧС_Івано_Франківська",
  "ЧС_Рівненська", "ЧС_Одеська", "ЧС_Сумська",    
  "ЧС_Закарпатська", "ЧС_Львівська", "ЧС_Миколаївська", 
  "ЧС_Донецька", "ЧС_Херсонська", "ЧС_Севастополь", 
  "ЧС_Тернопільська", "ЧС_Київ", "ЧС_Волинська",  
  "ЧС_Хмельницька", "ЧС_Запорізька", "ЧС_Кіровоградська",
  "ЧС_Луганська", "ЧС_Київська", "ЧС_Дніпропетровська",
  "matchType", "confidence", "status", "rank"
)

# Import needed support datasets ####

## Import GBIF credentials
source(path_gbif_credentials)

## Import species name dataset
load(file = path_species_name_dataset)
goodmatch <- matches[["goodmatch"]]
badmatch <- matches[["badmatch"]]

# df_combined_specieses_status <- rbind(goodmatch, badmatch)
df_combined_specieses_status <- goodmatch


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
  # format = "SIMPLE_CSV",
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

## load sawed response
# load(file = path_gbif_response)
# load(file = path_gbif_dataset_metadata)


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

# Join downloaded dataset and dataframe species statuses ####

## rename field "key" to "speciesKey"
colnames(df_combined_specieses_status)[colnames(df_combined_specieses_status) == taxon_id_field] <- taxon_id_gbif

# rename fields: scientificName verbatimScientificName kingdom phylum class in df_dataset for correct join
colnames(df_dataset)[colnames(df_dataset) == "scientificName"] <- "scientificName_gbif"
colnames(df_dataset)[colnames(df_dataset) == "verbatimScientificName"] <- "verbatimScientificName_gbif"
colnames(df_dataset)[colnames(df_dataset) == "kingdom"] <- "kingdom_gbif"
colnames(df_dataset)[colnames(df_dataset) == "phylum"] <- "phylum_gbif"
colnames(df_dataset)[colnames(df_dataset) == "class"] <- "class_gbif"


# rm(df_joined_dataset)

## Join dataframes
df_joined_dataset <- inner_join(df_dataset, df_combined_specieses_status, by = taxon_id_gbif) %>% 
  dplyr::select(all_of(colnames_set0)) 


# df_selected_dataset <- df_joined_dataset %>% dplyr::select(all_of(colnames_set0)) 

class(df_joined_dataset)

## temp backup
# save(df_dataset, file = "temp/df_dataset.Rdata") #temp for test
# save(df_joined_dataset, file = "temp/df_joined_dataset.Rdata") #temp for test
# 
# write.csv(df_dataset, "temp/df_dataset.csv")  #temp for test
# write.csv(df_joined_dataset, "temp/df_joined_dataset.csv")  #temp for test

# Convert to SF object
sf_points <- st_as_sf(df_joined_dataset, dim = "XY", remove = FALSE, na.fail = F, 
                      coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs")

class(sf_points)

# preview geometry column
head(sf_points[tail(colnames(sf_points))])

# clip by extent
sf_points_croped_extent <- st_crop(sf_points, st_bbox(country_polygon)) # предварительная образка до экстента, похоже ускоряет обрезку до конечного полигона.

# clip by polygon

gbif_sf_dataset <- st_intersection(sf_points_croped_extent, country_polygon)


# Save GBIF points as Robject


# save(gbif_sf_dataset, file = "temp/gbif_sf_dataset.Rdata") #temp for test

save(gbif_sf_dataset, file = path_gbif_sf_dataset)

write.csv(gbif_sf_dataset, "temp/gbif_sf_dataset.csv")

# Preview result
require(ggplot2)
library(basemapR)

ggplot()+
  base_map(bbox = st_bbox(gbif_sf_dataset), 
           basemap = 'mapnik', 
           increase_zoom = 2) +
  geom_sf(data=gbif_sf_dataset, aes(color="red"),size=2)+
  # scale_colour_manual(values = kingdom_colors, name=NULL ) +
  theme_minimal()+
  theme(axis.text = element_blank())+
  theme(legend.position = "bottom",
        legend.margin=margin())+
  labs(caption = "Basemap attribution: © OpenStreetMap contributors")



