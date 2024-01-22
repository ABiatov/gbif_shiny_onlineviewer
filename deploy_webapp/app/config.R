CRS_used_in_calculations <- 3537 # WGS 84 / North Pole LAEA Europe
# query_limit = 50

# url_metadata_datadump <- "https://github.com/ABiatov/gbif_shiny_onlineviewer/raw/main/name_lookup/outputs/metadata.Rdata"
# url_datadump <- "https://github.com/ABiatov/gbif_shiny_onlineviewer/raw/main/name_lookup/outputs/gbif_sf_dataset.Rdata"

# url_metadata_datadump <- "https://fra1.digitaloceanspaces.com/abspatial/WD/gbif_shiny_onlineviewer/metadata.Rdata"
# url_datadump <- "https://fra1.digitaloceanspaces.com/abspatial/WD/gbif_shiny_onlineviewer/gbif_sf_dataset.Rdata"
# url_datadump_fgb <- "https://fra1.digitaloceanspaces.com/abspatial/WD/gbif_shiny_onlineviewer/gbif_sf_dataset.fgb"

# url_metadata_datadump <- "https://abspatial.com/data/gbif_viewer/metadata.Rdata"
# url_datadump <- "https://abspatial.com/data/gbif_viewer/gbif_sf_dataset.Rdata"
# url_datadump_fgb <- "https://abspatial.com/data/gbif_viewer/gbif_sf_dataset.fgb"

path_metadata_datadump <- "gbif_data/metadata.Rdata"
path_datadump <- "gbif_data/gbif_sf_dataset.Rdata"
path_datadump_fgb <- "gbif_data/gbif_sf_dataset.fgb"


# Lists of columns
## for full table (excel)
colnames_set1 <- c(
  "individualCount", "organismQuantity", "organismQuantityType",
  "eventDate", "year", "Latitude", "Longitude", "coordinateUncertaintyInMeters", "coordinatePrecision", "verbatimLocality",
  "nameUk", "scientificName", "kingdom",
  # "phylum",
  "class", 
  # "order", 
  "family",
  # "genus",
  "ЧКУ",
  "iucnRedListCategory",
  "BernAppendix1", 
  "BernAppendix2", 
  "BernAppendix3", 
  "BernResolution6",
  "Bonn",
  "AEWA",
  "EUROBATS",
  "ACCOBAMS",
  "BirdsDirectiveAnnex_I", 
  "BirdsDirectiveAnnex_IІ",
  "HabitatsDirectiveAnnex_II", 
  "HabitatsDirectiveAnnex_IV", 
  "HabitatsDirectiveAnnex_V",
  "Invasive",
  "ЧС_Вінницька",
  "ЧС_Волинська",
  "ЧС_Дніпропетровська",
  "ЧС_Донецька",
  "ЧС_Житомирська",
  "ЧС_Закарпатська",
  "ЧС_Запорізька",
  "ЧС_Івано_Франківська",
  "ЧС_Київська",
  "ЧС_Кіровоградська",
  "ЧС_Луганська",
  "ЧС_Львівська",
  "ЧС_Миколаївська",
  "ЧС_Одеська",
  "ЧС_Полтавська",
  "ЧС_Рівненська",
  "ЧС_Сумська",
  "ЧС_Тернопільська",
  "ЧС_Черкаська",
  "ЧС_Чернівецька",
  "ЧС_Чернігівська",
  "ЧС_Харківська",
  "ЧС_Херсонська",
  "ЧС_Хмельницька", 
  "ЧС_Київ",
  "ЧС_Севастополь",
  "URL_record", "URL_dataset",
  "license"
)

## for reduced table (to show the output in the application)
colnames_set2 <- c(
  "nameUk", "scientificName",
  "year", "Latitude", "Longitude", "kingdom" ,
  "class", "family",
  "URL_record"
  # "phylum", "order", "family" 
)

## for reduced table (to instal to DOCX report)
colnames_set3 <- c("scientificName", "nameUk", "class", "family", "kingdom") 

# International agreements and conventions to filter
vector_conventions <- c(
  "Bern Appendix 1",
  "Bern Appendix 2",
  "Bern Appendix 3",
  "Bern Resolution 6",
  "Bonn",
  "AEWA",
  "EUROBATS",
  "ACCOBAMS",
  "Birds Directive Annex I", 
  "Birds Directive Annex IІ",
  "Habitats Directive Annex II", 
  "Habitats Directive Annex IV", 
  "Habitats Directive Annex V"
)

iucn_category <- c("EX", "EW", "CR", "EN", "VU", "NT", "LC", "DD", "NE")

iucn_category_selected <- c("EX", "EW", "CR", "EN", "VU", "NT", "DD")


# draw_new_shape_options
draw_new_shape_options <- drawShapeOptions(
  # clickable = TRUE,
  weight = 1,
  opacity = 0.8,
  fillOpacity = 0.3,
  color = '#ff0000',
  fillColor = 'blue')

polygon_aoi_options <- pathOptions(color = "#ff0000", weight = 2, opacity = 0.9, fill = FALSE, zIndexOffset = 12)

buffered_polygon_options <- pathOptions(color = "#03F", weight = 2, opacity = 0.9, fill = FALSE, zIndexOffset = 10) 


# pallet points on map
kingdom_colors <- c("Plantae" = "#4daf4a", "Fungi" = "#377eb8", "Animalia" = "#ff7f00")



