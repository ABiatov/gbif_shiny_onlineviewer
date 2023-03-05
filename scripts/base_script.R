# Environment preparation ####
rm(list = ls()) # Reset R`s brain

# Set working directory - NO NEED 
setwd("C:/Users/admin/Documents/GitHub/gbif_shiny_onlineviewer/")


## Library import ####
library(sf)
#require(raster)
require(rgeos)
require(rgbif)
library(dplyr)
library(data.table)
require(writexl)

# Calling custom function
source("scripts/config.R")
source("functions/import_dictionaries.R")

## INPUTS ####
# Import a polygon shapefile
aoi <- st_read("data_temp/aoi.shp") # in production read polygons from Rdata

buff_radius <- 10000

specieses_list = c("Plecotus auritus", "Nyctalus leisleri", 
              "Coronella austriaca", "Columba oenas", 
              "Morchella steppicola", "Fritillaria meleagroides", 
              "Tulipa quercetorum", "Utricularia minor")

## BUFFERIN AOI ####

# todo add bufferisation on CRS 

# extent_aoi <- raster::extent(aoi)
# extent_aoi <- as(extent_aoi, "SpatialPolygons")
# ex_WKT <- rgeos::writeWKT(extent_aoi)

# aoi_geometry <- st_geometry(aoi) # TODO изучить варианты работы с несколькими полигонами или мультиполигоном
aoi_geometry <- st_geometry(aoi[1,])

aoi_buffered <- st_transform(aoi_geometry, CRS_used_in_calculations) %>%
  st_buffer(dist = buff_radius, nQuadSegs = 4) %>%
  st_transform(4326)

# plot(aoi_buffered,  border = 'blue')
# plot(aoi_geometry, add = TRUE,border = 'red')

aoi_WKT <- st_as_text(aoi_buffered)
# aoi_WKT <- as(aoi_WKT, "SpatialPolygons")
#aoi_WKT <- rgeos::writeWKT(aoi_WKT)

## Query to GBIF ####

RECIEVED_DATA <- occ_search(scientificName = specieses_list, 
                            return = "data", hasCoordinate = T, 
                            geometry = aoi_WKT, 
                            limit = query_limit
                            )

DF_RECIEVED_DATA<- rbindlist(lapply(RECIEVED_DATA, function(x) x$data), fill = TRUE, use.names = TRUE)

# write.csv(DF_RECIEVED_DATA, "outputs/DF_RECIEVED_DATA.csv")
#colnames(DF_RECIEVED_DATA)

## Create DF_PREVIEW dataframe ####

DF_PREVIEW <- dplyr::select(DF_RECIEVED_DATA, all_of(fields_list_to_DF_PREVIEW)) %>%
  mutate(across(fields_as_factor, as.factor)) %>%
  left_join(dict_name_ua) %>%
  rename(Latitude = decimalLatitude, Longitude = decimalLongitude) %>%
  mutate(URL_record = paste0("https://www.gbif.org/occurrence/", key)) %>%
  mutate(URL_species = paste0("https://www.gbif.org/species/", taxonKey)) %>%
  mutate(URL_kingdom = paste0("https://www.gbif.org/species/", kingdomKey)) %>%
  mutate(URL_class = paste0("https://www.gbif.org/species/", classKey)) %>%
  mutate(URL_dataset = paste0("https://www.gbif.org/dataset/", datasetKey)) %>%
  mutate(to_report = TRUE) %>%
  dplyr::select(all_of(refactor_fields_list_to_DF_PREVIEW))

# write.csv(DF_PREVIEW, "outputs/DF_PREVIEW.csv")

# levels(DF_PREVIEW$scientificName)

# add fields:
# class_name
# dataset_name

## Create DF_REPORT dataframe ####


DF_REPORT <- DF_PREVIEW %>%
  left_join(red_book_ua) %>%
  left_join(dict_iucn_category) %>%
  dplyr::select(all_of(refactor_fields_list_to_DF_REPORT))

## Export dataframe DF_REPORT to XLSX  ####

write_xlsx(DF_REPORT, "outputs/DF_REPORT.xlsx")







