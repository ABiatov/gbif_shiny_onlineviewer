# Environment preparation ####
rm(list = ls()) # Reset R`s brain

# Set working directory - NO NEED 
setwd("C:/Users/admin/Documents/GitHub/gbif_shiny_onlineviewer/")


## Library import ####
library(sf)
library(sp)
#require(raster)
require(rgeos)
require(rgbif)
library(dplyr)
require(ggplot2)
library(basemapR)
library(data.table)
require(writexl)
library(magrittr)
library(officer)

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


## Create DF_PREPRINT dataframe ####

DF_PREPRINT <- DF_REPORT %>%
  dplyr::select(all_of(fields_list_to_DF_PREPRINT)) %>%
  group_by(scientificName,  # настроить корректно групбай чтоб не удаляло лишние поля или джойнить их DF_REPORT
           NameUA,          # TODO to config
           RedBookUA,
           IUCN_Red_List) %>%
  summarise(Amount = n()) %>%
  na.omit()

## Export dataframe DF_PREPRINT to XLSX  ####

# write_xlsx(DF_PREPRINT, "outputs/DF_PREPRINT.xlsx")
  
# DOCX generation ####

## Generate map for print ####

sf_points <- st_as_sf(DF_PREVIEW, dim = "XY", remove = FALSE, na.fail = F, 
                   coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs")

cols <- c("Plantae" = "#4daf4a", "Fungi" = "#377eb8", "Animalia" = "#ff7f00")

# TODO generate Simple map wiz extent aoi_buffered +- 5%

src <- tempfile(fileext = ".png")
png(filename = src, width = 5, height = 5, units = 'in', res = 300)

ggplot(data=sf_points)+
  base_map(bbox = st_bbox(aoi_buffered), 
           basemap = 'mapnik', 
           increase_zoom = 2) +
  geom_sf(aes(color=kingdom),size=2)+
  geom_sf(data = aoi_buffered, colour = "blue", fill=NA)+
  geom_sf(data = aoi_geometry, colour = "red", fill = NA)+
  scale_colour_manual(
    values = cols, name=NULL ) +
  theme_minimal()+
  theme(axis.text = element_blank())+
  theme(legend.position = "bottom")+
  labs(caption = "Basemap attribution: © OpenStreetMap contributors")

dev.off()

## Generate document

my_doc <- read_docx() 
styles_info(my_doc)

my_doc <- my_doc %>% 
  body_add_par(txt_report_header, style = "heading 1") %>% 
  body_add_par("", style = "Normal") %>% # blank paragraph
  body_add_par(txt_about_gbif_viewer, style = "Normal") %>% 
  body_add_img(src = src, width = 5, height = 5, style = "centered") %>%
  body_add_par("", style = "Normal") %>% # blank paragraph
  body_add_par("Перелік видів", style = "heading 2") %>% 
  body_add_table(DF_PREPRINT, style = "table_template")

print(my_doc, target = "outputs/report.docx")

# Cleaning workspace ####
# rm(list = ls()) # Reset R`s brain
