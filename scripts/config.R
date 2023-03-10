CRS_used_in_calculations <- 3537 # WGS 84 / North Pole LAEA Europe
query_limit = 1000

fields_dict_red_book_ua <- c("scientificName", "RedBookUA")

fields_list_to_DF_PREVIEW <- c("key", "scientificName", 
                               "decimalLatitude", "decimalLongitude",
                               "kingdom", "classKey",
                               "eventDate", "datasetKey",
                               "iucnRedListCategory", "taxonKey",
                               "kingdomKey"
                               )

fields_as_factor <- c("scientificName", "kingdom", "classKey", 
                      "iucnRedListCategory", "taxonKey", "kingdomKey"
                      )


refactor_fields_list_to_DF_PREVIEW <- c("scientificName", "NameUA",
                                        "Latitude", "Longitude",
                                        "kingdom", "classKey",
                                        "eventDate", "datasetKey",
                                        "URL_record", "URL_species",
                                        "URL_kingdom", "URL_class",
                                        "URL_dataset",
                                        "iucnRedListCategory",
                                        "to_report"
                                        )

refactor_fields_list_to_DF_REPORT <- c("scientificName", "NameUA",
                                        "Latitude", "Longitude",
                                        "kingdom", "classKey", "eventDate",
                                        "RedBookUA",
                                        "IUCN_Red_List",
                                        "datasetKey",
                                        "URL_record", "URL_species",
                                        "URL_kingdom", "URL_class",
                                        "URL_dataset"
                                       )

fields_list_to_DF_PREPRINT <- c("scientificName", "NameUA",
                         #     "PotectedStatus",
                              "RedBookUA",
                              "IUCN_Red_List",
                              "URL_species"
                              )

# DOCX text blocks ####

txt_report_header <- "?????????????? ?????????????????? ?????? ???????????????????? ?????????????????? ???? ??????????, ???? ?????????????????????? ?????? ????????????????, ??????????"

txt_about_gbif_viewer <- "GBIF Viewer: an open web-based biodiversity conservation decision-making tool for policy and governance. ???????????????? ???????????? The Habitat Foundation ???? ?????????????????????? ???????????????????????????????? ??????????, ???? ?????????????????? NLBIF: The Netherlands Biodiversity Information Facility"


# pallet points on map
kingdom_colors <- c("Plantae" = "#4daf4a", "Fungi" = "#377eb8", "Animalia" = "#ff7f00")



