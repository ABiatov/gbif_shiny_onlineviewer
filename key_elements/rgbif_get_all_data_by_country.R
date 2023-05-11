# Get GBIF data by specieses list from table in Country

library(readxl)
library(rgbif)
library(dplyr)
library(sf)
require(ggplot2)
library(basemapR)
# Import GBIF credentials
source("scripts/gbif_ini.R")

curent_country <- "UA"
puth_country_vector_layer <- "regions/gadm41_UKR_0.shp"


xl_file_path <- "dictionaries/rdb_ua.xlsx"
taxon_id_field <- "key"

# Read XLSX tab
read_xlsx(xl_file_path)

df_specieses_status <- read_xlsx(xl_file_path) %>%
  as.data.frame()

vector_specieses_taxonId = df_specieses_status[[taxon_id_field]]

# preforms the query
response = occ_download(
  pred_in("taxonKey", vector_specieses_taxonId),
  pred_in("hasCoordinate", TRUE),
  pred_in("country", curent_country),  # TODO: Move "UA" to Config.R
  user = gbif_user,
  pwd = gbif_pwd, 
  email = gbif_email
)

gbif_dataset_metadata <- occ_download_meta(response)

print(paste0("DOI: ", gbif_dataset_metadata$doi))
print(paste0("https://doi.org/", gbif_dataset_metadata$doi)) # Example: https://doi.org/10.15468/dl.qpx7ya
print(paste0("Dataset key: ", gbif_dataset_metadata$key))
print(paste0("Download link: ", gbif_dataset_metadata$downloadLink))


# You need to wait for 20-25 minutes until the dataset is ready for the server.
# Whan dataset show by doi link you can continue script

# rm(df_dataset)

# damp_dataset <- occ_download_get(response, overwrite = TRUE)

damp_dataset <- occ_download_get(gbif_dataset_metadata$key)


df_dataset <-   occ_download_import(damp_dataset) %>%
  rename(Latitude = decimalLatitude, Longitude = decimalLongitude)

class(df_dataset)
# str(df_dataset)

# TODO add select necessary fields

# Convert to SF object
sf_points <- st_as_sf(df_dataset, dim = "XY", remove = FALSE, na.fail = F, 
                      coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs")

class(sf_points)

# preview geometry column
head(sf_points[tail(colnames(sf_points))])

cst_geometry(sf_points)


# TODO add clip by country polygon
## import spatial data ####
country_polygon <- st_read(puth_country_vector_layer)
class(country_polygon)

# st_crop - crop an sf object to a specific rectangle
# st_intersection
# st_intersection.sf
# st_overlaps

# clip by extent
sf_points_croped_extent <- st_crop(sf_points, st_bbox(country_polygon)) # предварительная образка до экстента, похоже ускоряет обрезку до конечного полигона.

# clip by polygon

gbif_sf_dataset <- st_intersection(sf_points_croped_extent, country_polygon)


# Preview result


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

# Save GBIF points as Robject

save(response, file = "data/gbif_response.Rdata")

save(gbif_dataset_metadata, file = "data/gbif_dataset_metadata.Rdata")

save(gbif_sf_dataset, file = "data/gbif_sf_dataset.Rdata")


# Cleaning workspace ####
# rm(list = ls()) # Reset R`s brain

# Load files
# load(file = "data/gbif_sf_dataset.Rdata")
# 
# load(file = "data/gbif_dataset_metadata.Rdata")




