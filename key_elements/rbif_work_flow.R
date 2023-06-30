# Source: https://github.com/mcglinnlab/soar

library(rgbif)
library(sf)
require(ggplot2)
library(basemapR)

# Import GBIF credentials
source("scripts/gbif_ini.R")

# Get Taxon Key by Species name
sp_key <- name_suggest(q = 'Sus scrofa', rank = 'species')$data$key[1]


sp_keys <- c('2440940', '7705930')

# preforms the query
response = occ_download(
  # pred_in("taxonKey", sp_keys),
  pred_in("hasCoordinate", TRUE),
  pred_in("country", 'UA'),  # TODO: Move "UA" to Config.R
  # pred_within("POLYGON ((35.45321 50.36227, 35.66187 50.88181, 36.76558 50.81935, 36.61732 49.47762, 36.00781 49.45976, 35.45321 50.36227))"), # MAKING Mistake
  user = gbif_user,
  pwd = gbif_pwd, 
  email = gbif_email
  )

dataset_metadata <- occ_download_meta(response)
dataset_doi <- dataset_metadata$doi

print(paste0("DOI: ", dataset_doi))

# rm(df_dataset)

df_dataset <- occ_download_get(response, overwrite = TRUE) %>%
  occ_download_import() %>%
  rename(Latitude = decimalLatitude, Longitude = decimalLongitude)

class(df_dataset)
# str(df_dataset)


#

# Preview result
sf_points <- st_as_sf(df_dataset, dim = "XY", remove = FALSE, na.fail = F, 
                      coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs")

ggplot()+
  base_map(bbox = st_bbox(sf_points), 
           basemap = 'mapnik', 
           increase_zoom = 2) +
  geom_sf(data=sf_points, aes(color=kingdom),size=2)+
  scale_colour_manual(values = kingdom_colors, name=NULL ) +
  theme_minimal()+
  theme(axis.text = element_blank())+
  theme(legend.position = "bottom",
        legend.margin=margin())+
  labs(caption = "Basemap attribution: © OpenStreetMap contributors")






