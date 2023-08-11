rm(list = ls())
gc()

# CONFIG Start ####

url_datadump <- "https://github.com/ABiatov/gbif_shiny_onlineviewer/raw/main/data/gbif_sf_dataset.Rdata"

# url_datadump <- "https://github.com/ABiatov/gbif_shiny_onlineviewer/raw/main/key_elements/Liaflet_filters/data.Rdata"

# url_datadump <- "https://fra1.digitaloceanspaces.com/abspatial/WD/gbif_shiny_onlineviewer/test/data.Rdata"


# CONFIG End

# primary_wd <- getwd() # write current work directory
primary_wd <- setwd(tempdir()) # write current work directory
on.exit(setwd(primary_wd))

# print(primary_wd)
# print(getwd())


# custom functions start ####

# custom functions end

# lockal_puth_to_datadump_file <- "C:/Users/admin/Documents/GitHub/gbif_shiny_onlineviewer/temp/gbif_sf_dataset.Rdata"

# lockal_puth_to_datadump_file <- "C:/Temp/data.Rdata"
lockal_puth_to_datadump_file <- "temp_data.Rdata"

download.file(url_datadump, lockal_puth_to_datadump_file,
              method="libcurl",
              mode = "wb",
              )

load(lockal_puth_to_datadump_file)

# rm(data)
setwd(primary_wd)

print(getwd())

load(lockal_puth_to_datadump_file)

# OR ####

load(url(url_datadump))


