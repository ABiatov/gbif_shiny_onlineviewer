# join_gbifdata_conservationsatatus

# Cleaning workspace ####
rm(list = ls()) # Reset R`s brain

# Import libraries ####
library(readxl)
library(dplyr)

# Load files ####
# Load damp gbif_sf_dataset.Rdata
load(file = "data/gbif_sf_dataset.Rdata")

class(gbif_sf_dataset)

# Load df_specieses_status
xl_file_path <- "dictionaries/rdb_ua.xlsx"
taxon_id_xlsx <- "key"
taxon_id_gbif <- "speciesKey"
## Read XLSX tab
df_specieses_status <- read_xlsx(xl_file_path) %>%
  as.data.frame()

# rename field "key" to "speciesKey"
colnames(df_specieses_status)[colnames(df_specieses_status) == taxon_id_xlsx] <- taxon_id_gbif

class(df_specieses_status)

# Join dataframes

joined_df <- inner_join(gbif_sf_dataset, df_specieses_status, by = taxon_id_gbif)

class(joined_df)


# TODO Add selector necessary columns  




library(sf)
require(ggplot2)
library(basemapR)

ggplot()+
  base_map(bbox = st_bbox(joined_df), 
           basemap = 'mapnik', 
           increase_zoom = 2) +
  geom_sf(data=joined_df, aes(color="red"),size=2)+
  # scale_colour_manual(values = kingdom_colors, name=NULL ) +
  theme_minimal()+
  theme(axis.text = element_blank())+
  theme(legend.position = "bottom",
        legend.margin=margin())+
  labs(caption = "Basemap attribution: © OpenStreetMap contributors")



