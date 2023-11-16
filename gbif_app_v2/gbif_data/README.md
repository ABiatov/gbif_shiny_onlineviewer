# gbif_sf_dataset to FlatGeobuf format

Open terminal or Windows PowerShell

Go to directory with files

```shel

cd 

```

"C:/Program%20Files/R/R-4.3.2/bin/R.exe" "C:/Users/admin/Documents/GitHub/gbif_shiny_onlineviewer/gbif_app_v2/gbif_data/gbif_sf_dataset_to_FlatGeobuf.R"


```r

setwd("<YOU_FOLDER_PATH>")
# For example
# setwd("C:/temp/gbif_shiny_onlineviewer/gbif_app_v2/gbif_data/")

library(sf)

load(file = "gbif_sf_dataset.Rdata")

write_sf(gbif_sf_dataset, 
         dsn="gbif_sf_dataset.fgb",
         driver="FlatGeobuf",
         delete_dsn= TRUE)

# or

st_write(gbif_sf_dataset, "gbif_sf_dataset.fgb", delete_dsn= TRUE)

rm(list = ls()) # Reset R`s brain
gc()            # Free unused R's memory

```

