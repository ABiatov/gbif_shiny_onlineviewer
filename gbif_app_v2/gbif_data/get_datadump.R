# gbif_sf_dataset to FlatGeobuf format

library(sf)

# setwd("<YOU_FOLDER_PATH>")

# For example
# setwd("C:/Users/admin/Documents/GitHub/gbif_shiny_onlineviewer/gbif_app_v2/gbif_data/")

# link_gbif_sf_dataset <- "https://github.com/olehprylutskyi/GBIF_occurence_download/raw/main/outputs/gbif_sf_dataset.Rdata"
# link_metadata <- "https://github.com/olehprylutskyi/GBIF_occurence_download/raw/main/outputs/metadata.Rdata"
# Ссылки ^поменялись, нужно вручную скачивать, или обновить ссылки.

link_gbif_sf_dataset <- "https://github.com/olehprylutskyi/GBIF_occurence_download/raw/refs/heads/main/outputs/gbif_sf_dataset.Rdata"
link_metadata <- "https://github.com/olehprylutskyi/GBIF_occurence_download/raw/refs/heads/main/outputs/metadata.Rdata"

# For run in RStudio
# file_name_gbif_sf_dataset <- "gbif_sf_dataset.Rdata"
# file_name_metadata <- "metadata.Rdata"
# output_fgb_file <- "gbif_sf_dataset.fgb"

# For run in VS Code 
file_name_gbif_sf_dataset <- "gbif_app_v2/gbif_data/gbif_sf_dataset.Rdata"
file_name_metadata <- "gbif_app_v2/gbif_data/metadata.Rdata"
output_fgb_file <- "gbif_app_v2/gbif_data/gbif_sf_dataset.fgb"

download.file(link_metadata, file_name_metadata, mode = "wb")
download.file(link_gbif_sf_dataset, file_name_gbif_sf_dataset, mode = "wb")

load(file = file_name_metadata)

load(file = file_name_gbif_sf_dataset)

write_sf(gbif_sf_dataset, 
         dsn=output_fgb_file,
         driver="FlatGeobuf",
         delete_dsn= TRUE)

rm(list = ls()) # Reset R`s brain
gc()            # Free unused R's memory


