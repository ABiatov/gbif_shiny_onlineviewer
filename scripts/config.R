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
