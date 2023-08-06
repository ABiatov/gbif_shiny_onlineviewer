CRS_used_in_calculations <- 3537 # WGS 84 / North Pole LAEA Europe
query_limit = 50

# Lists of columns
## for full table (excel)
colnames_set1 <- c(
  
  # "bibliographicCitation", "identifier", "license", "publisher", 
  # "references",  "rightsHolder", "type", "institutionID", "collectionID", "datasetID", 
  # "institutionCode", "collectionCode", "datasetName", "ownerInstitutionCode", 
  # "basisOfRecord",  "informationWithheld",
  # "occurrenceID", "catalogNumber", "recordNumber", "recordedBy", "recordedByID", "individualCount",
  # "organismQuantity", "organismQuantityType", "sex", "lifeStage", "reproductiveCondition", "behavior",
  # "establishmentMeans", "degreeOfEstablishment", "georeferenceVerificationStatus", "occurrenceStatus",
  # "preparations", "disposition", "associatedReferences", "associatedSequences", "otherCatalogNumbers",
  # "occurrenceRemarks", "organismID", "organismScope", "materialSampleID", "eventID", "parentEventID",
  "eventDate",
  # "eventTime", 
  "year", 
  # "month", "day", "verbatimEventDate", 
  # "habitat", "samplingProtocol", "sampleSizeValue", "sampleSizeUnit", "samplingEffort", "fieldNotes", "eventRemarks", 
  # "locationID", "waterBody", "locality", 
  # "verbatimLocality", # commented for test
  "Latitude", "Longitude", "coordinateUncertaintyInMeters", 
  # "issue", "footprintWKT", "identifiedBy", "dateIdentified",
  # "taxonID", "acceptedNameUsageID", "parentNameUsageID",
  "nameUk", "scientificName", "kingdom", "phylum", "class", "order", "family", "genus",
  #   "genericName", "infragenericEpithet", "specificEpithet", "infraspecificEpithet", "taxonRank",
  # "vernacularName", "taxonomicStatus", 
  # "publishingCountry", "lastInterpreted", "issue", "mediaType",
  # "taxonKey", "acceptedTaxonKey", "kingdomKey", "phylumKey", "classKey", "orderKey",
  # "familyKey", "genusKey", "speciesKey", "species", "acceptedScientificName", "verbatimScientificName",
  # "typifiedName", 
  "iucnRedListCategory",
  "BernAppendix2", "BernAppendix3", "Bonn", "AEWA", 
  # "IUCN", 
  "BernResolution6", "ЧКУ", 
  "BernAppendix1", 
  # "CITES", 
  "EUROBATS",      
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
  # "gbifID", "datasetKey"
  "URL_record", "URL_dataset"
  # "matchType", "confidence", "status", "rank"
)

## for reduced table (to show the output in the application)
colnames_set2 <- c(
  "nameUk", "scientificName",
  "year", "Latitude", "Longitude", "kingdom",
  "phylum", "class", "order", "family" 
)

## for reduced table (to instal to DOCX report)
colnames_set3 <- c("scientificName", "nameUk", "kingdom") 

# Internetional agriments and conventions to filter
vector_conventions <- c(
  "Bern Appendix 1",
  "Bern Appendix 2",
  "Bern Appendix 3",
  "Bern Resolution 6",
  "Bonn",
  "AEWA",
  # "CITES",
  "EUROBATS",
  "ACCOBAMS",
  "Birds Directive",
  "Habitats Directive"
)

vect_region_redlist <- c(
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
  "ЧС_Севастополь"
)


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



# # Don't use in last version
# fields_dict_red_book_ua <- c("scientificName", "RedBookUA") 

# # Don't use in last version
# fields_list_to_DF_PREVIEW <- c("key", "scientificName",  
#                                "decimalLatitude", "decimalLongitude",
#                                "kingdom", "classKey", "kingdomKey",
#                                "eventDate", "datasetKey",
#                                "iucnRedListCategory", "taxonKey",
#                                "identifier.1", "georeferencedBy", "georeferenceProtocol", "associatedReferences",
#                                "verbatimLocality", "occurrenceID", "coordinateUncertaintyInMeters"
#                                )

# # Don't use in last version
# fields_as_factor <- c("scientificName", "kingdom", "classKey", 
#                       "iucnRedListCategory", "taxonKey", "kingdomKey"
#                       )

# # Don't use in last version
# refactor_fields_list_to_DF_PREVIEW <- c("scientificName", "NameUA",
#                                         "Latitude", "Longitude",
#                                         "kingdom", "classKey",
#                                         "eventDate", "datasetKey",
#                                         "URL_record", "URL_species",
#                                         "URL_kingdom", "URL_class",
#                                         "URL_dataset",
#                                         "iucnRedListCategory",
#                                         "to_report"
#                                         )

# # Don't use in last version
# refactor_fields_list_to_DF_REPORT <- c("scientificName", "NameUA",
#                                         "Latitude", "Longitude",
#                                         "kingdom", "classKey", "eventDate",
#                                         "RedBookUA",
#                                         "IUCN_Red_List",
#                                         "datasetKey",
#                                         "URL_record", "URL_species",
#                                         "URL_kingdom", "URL_class",
#                                         "URL_dataset"
#                                        )

# # Don't use in last version
# fields_list_to_DF_PREPRINT <- c("scientificName", 
#                                 "kingdom" #,
#                           #      "NameUA",
#                          #     "PotectedStatus",
#                          #     "RedBookUA",
#                          #     "IUCN_Red_List",
#                          #     "URL_species"
#                               )

# DOCX text blocks ####

date <- format(Sys.Date(), "%Y-%m-%d")
text_with_date <- paste("Звіт згенеровано", date)  # in report.Rmd

# pallet points on map
kingdom_colors <- c("Plantae" = "#4daf4a", "Fungi" = "#377eb8", "Animalia" = "#ff7f00")




# # don't use it in last version 2.3
# txt_report_header <- "Зведені відомості про реєстрації рідкісних та таких, що перебувають під охороною, видів"
# 
# 
# # don't use it in last version 2.3
# txt_about_gbif_viewer <- "GBIF Viewer: an open web-based biodiversity conservation decision-making tool for policy and governance. Спільний проєкт The Habitat Foundation та Української Природоохоронної Групи, за підтримки NLBIF: The Netherlands Biodiversity Information Facility, nlbif2022.014"






