
# Text blocks used in the shiny app interface ####
## "Map" tab
txt_interface_tabs_map_title <- "Map"

txt_interface_tabs_map_choose_region <- "Choose the region"

txt_interface_tabs_map_choose_subregion <- "Choose the district"

txt_interface_tabs_map_choose_commune <- "Choose the territorial community"

txt_interface_tabs_map_upload_custom_file <- "Upload your own kml/kmz polygon"

txt_interface_tabs_map_draw_buffer <- "Buffer zone around the area of interest"

txt_interface_tabs_map_getdata_button <- "Get GBIF data"

txt_about_gbif_viewer_noFormat <- "Biodiversity Viewer: an open web-based biodiversity conservation decision-making tool for policy and governance. Joint project of The Habitat Foundation and Ukrainian Nature Conservation Group, supported by NLBIF: The Netherlands Biodiversity Information Facility, nlbif2022.014"

## "Filtering" tab
txt_interface_tabs_filter_title <- "Filter data"

txt_interface_tabs_filter_comment <- "Define the search criteria and click the <Aplly filters> button"

txt_interface_tabs_filter_reddatabookofukraine <- "Red Data Book of Ukraine"

txt_interface_tabs_filter_iucnrl <- "IUCN Red List"

iucn_category_full_names <- c("Extinct, EX",
                              "Extinct in the Wild, EW",
                              "Critically Endangered, CR",
                              "Endangered, EN",
                              "Vulnerable, VU",
                              "Near Threatened, NT",
                              # "Least Concern, LC",
                              "Data Deficient, DD"
                              #, "Not Evaluated, NE"
)

txt_interface_tabs_filter_iucnrl_choices <- as.list(iucn_category_selected)

names(txt_interface_tabs_filter_iucnrl_choices) <- iucn_category_full_names

txt_interface_tabs_filter_international <- "International conventions and agreements"

txt_interface_tabs_filter_international_choices <- as.list(vector_conventions)

vector_conventions_names <- c("Bern Convention. Appendix 1",
                              "Bern Convention. Appendix 2",
                              "Bern Convention. Appendix 3",
                              "Bern Convention. Resolution 6",
                              "Convention on the Conservation of Migratory Species of Wild Animals (Bonn Convention)",
                              "Agreement on the Conservation of African-Eurasian Migratory Waterbirds (AEWA)",
                              "Agreement on the Conservation of Populations of European Bats (EUROBATS)",
                              # "Agreement on the Conservation of Cetaceans of the Black Sea, Mediterranean Sea and contiguous Atlantic Area (ACCOBAMS)",
                              "Agreement on the Conservation of Cetaceans of the Black Sea, Mediterranean Sea and contiguous Atlantic Area (ACCOBAMS)",
                              "Birds Directive. Annex I",
                              "Birds Directive. Annex IІ",
                              "Habitats Directive. Annex IІ",
                              "Habitats Directive. Annex IV",
                              "Habitats Directive. Annex V")

names(txt_interface_tabs_filter_international_choices) <- vector_conventions_names

txt_interface_tabs_filter_regionalrl <- "Regional Red Lists"

vect_region_redlist <- c(
  "RL_Vinnytsia",
  "RL_Volyn",
  "RL_Dnipropetrovsk",
  "RL_Donetsk",
  "RL_Zhytomyr",
  "RL_Zakarpattia",
  "RL_Zaporizhzhia",
  "RL_Ivano_Frankivsk",
  "RL_Kyiv",
  "RL_Kirovohrad",
  "RL_Luhansk",
  "RL_Lviv",
  "RL_Mykolaiv",
  "RL_Odesa",
  "RL_Poltava",
  "RL_Rivne",
  "RL_Sumy",
  "RL_Ternopil",
  "RL_Cherkasy",
  "RL_Chernivtsi",
  "RL_Chernihiv",
  "RL_Kharkiv",
  "RL_Kherson",
  "RL_Khmelnytskyi", 
  "RL_Kyiv",
  "RL_Sevastopol"
)

txt_interface_tabs_filter_regionalrl_choices <- as.list(vect_region_redlist)

names(txt_interface_tabs_filter_regionalrl_choices) <- c("Vinnytsia reg.",
                                                         "Volyn reg.",
                                                         "Dnipropetrovsk reg.",
                                                         "Donetsk reg.",
                                                         "Zhytomyr reg.",
                                                         "Zakarpattia reg.",
                                                         "Zaporizhzhia reg.",
                                                         "Ivano-Frankivsk reg.",
                                                         "Kyiv reg.",
                                                         "Kirovohrad reg.",
                                                         "Luhansk reg.",
                                                         "Lviv reg.",
                                                         "Mykolaiv reg.",
                                                         "Odesa reg.",
                                                         "Poltava reg.",
                                                         "Rivne reg.",
                                                         "Sumy reg.",
                                                         "Ternopil reg.",
                                                         "Cherkasy reg.",
                                                         "Chernivtsi reg.",
                                                         "Chernihiv reg.",
                                                         "Kharkiv reg.",
                                                         "Kherson reg.",
                                                         "Khmelnytskyi reg.", 
                                                         "Kyiv",
                                                         "Sevastopol")


txt_interface_tabs_filter_kingdom <- "Kingdom"

kingdom_names <- c("Animalia",
                    "Plantae",
                    "Fungi",
                    "Chromista",
                    "Protozoa") # could be other kingdoms in future

kingdom_selected <- c("Animalia",
                      "Plantae",
                      "Fungi",
                      "Chromista",
                      "Protozoa") # could be other kingdoms in future

txt_interface_tabs_filter_kingdom_choices <- as.list(kingdom_selected)

names(txt_interface_tabs_filter_kingdom_choices) <- kingdom_names

invasive_alien_species <- "Invasive and alien species"

txt_clear_filters_button <- "Clear filters"

## "Preview" tab

txt_interface_tabs_preview_title <- "Datasheet preview"

## "Reports" tab

txt_interface_tabs_reports_title <- "Generating of reports"

txt_interface_tabs_reports_h2 <- "Summary statistics"

## "About" tab

txt_interface_tabs_about_title <- "About"

txt_interface_tabs_about_link <- "about_en.md"
# txt_interface_tabs_about_link <- "about_en.html"

## Controls and buttons labels ####

txt_apply_filters_button <- "Apply filters"

txt_interface_tabs_preview_downloadCSV_button <- "Download CSV"

txt_interface_tabs_preview_downloadXLSX_button <- "Download XLSX"

txt_interface_tabs_reports_docFormats_button <- "Document format"


# Back end

txt_backend_no_polygon <- "No polygon selected"

txt_backend_number_of_records <- "Total number of records: "

txt_backend_species_list <- "General species list"

txt_backend_species_rdbuk <- "Species, listed in Red Data Book of Ukraine"

txt_backend_species_iucnrl <- "Species, listed in IUCN Red List"

txt_backend_species_bern1 <- "Species, listed in Appendix I of the Bern Convention"

txt_backend_species_bern2 <- "Species, listed in Appendix II of the Bern Convention"

txt_backend_species_bern3 <- "Species, listed in Appendix III of the Bern Convention"

txt_backend_species_BernRes_6 <- "Species, listed in Resolution 6 of the Bern Convention"

txt_backend_species_Bonn <- "Species, listed in Convention on the Conservation of Migratory Species of Wild Animals (Bonn Convention)"

txt_backend_species_AEWA <- "Species, protected in terms of the Agreement on the Conservation of African-Eurasian Migratory Waterbirds (AEWA)"

txt_backend_report_total <- "Total, according to the search criteria"

txt_backend_report_eurobats <- "Species, protected in terms of the Agreement on the Conservation of Populations of European Bats (EUROBATS)"

txt_backend_report_accobams <- "Species, protected in terms of the Agreement on the Conservation of Cetaceans of the Black Sea, Mediterranean Sea and contiguous Atlantic Area (ACCOBAMS)"

txt_backend_report_birdDirectiveApp1 <- "Species, listed in Annex I of the Birds Directive"

txt_backend_report_birdDirectiveApp2 <- "Species, listed in Annex II of the Birds Directive"

txt_backend_report_habitatDirectiveApp2 <- "Species, listed in Annex II of the Habitats Directive"

txt_backend_report_habitatDirectiveApp4 <- "Species, listed in Annex IV of the Habitats Directive"

txt_backend_report_habitatDirectiveApp5 <- "Species, listed in Annex V of the Habitats Directive"

txt_backend_report_invasive_alien_species <- "Invasive and alien species"




# Text blocks used in the report template file (report.Rmd)
# This file is used for generation Word report

txt_report_title <- "GBIF data"

txt_report_date_created <- paste("Report created",
                                 format(Sys.Date(), "%Y-%m-%d") # current date
)

txt_report_header <- "Summarized information on the records of rare and protected species"

txt_report_citation_instruction <- "Please use the following citation format when mentioning the report in any publications:"

txt_report_statistics <- "Summary statistics"

txt_chku_species <- "Species, listed in the Red Data Book of Ukraine"

txt_iucn_species <- "Species, listed in IUCN Red List"

txt_BernApp_1_species <- "Species, listed in Appendix I of the Bern Convention"

txt_BernApp_2_species <- "Species, listed in Appendix II of the Bern Convention"

txt_BernApp_3_species <- "Species, listed in Appendix III of the Bern Convention"

txt_BernRes_6_species <- "Species, listed in Resolution 6 of the Bern Convention"

txt_Bonn_species <- "Species, listed in Convention on the Conservation of Migratory Species of Wild Animals (Bonn Convention)"

txt_AEWA_species <- "Species, protected in terms of the Agreement on the Conservation of African-Eurasian Migratory Waterbirds (AEWA)"

txt_EUROBATS_species <- "Species, protected in terms of the Agreement on the Conservation of Populations of European Bats (EUROBATS)"

txt_ACCOBAMS_species <- "Species, protected in terms of the Agreement on the Conservation of Cetaceans of the Black Sea, Mediterranean Sea and contiguous Atlantic Area (ACCOBAMS)"

txt_BirdDirAnn_I_species <- "Species, listed in Annex I of the Birds Directive"

txt_BirdDirAnn_II_species <- "Species, listed in Annex II of the Birds Directive"

txt_HabitatsDirAnn_II_species <- "Species, listed in Annex II of the Habitats Directive"

txt_HabitatsDirAnn_IV_species <- "Species, listed in Annex IV of the Habitats Directive"

txt_HabitatsDirAnn_V_species <- "Species, listed in Annex V of the Habitats Directive"

txt_Invasive_species <- "Invasive and alien species"

txt_all_species <- "General species list"

txt_about_gbif_viewer <- "**Biodiversity Viewer**: an open web-based biodiversity conservation decision-making tool for policy and governance. Joint project of The [Habitat Foundation](https://thehabitatfoundation.org/) and [Ukrainian Nature Conservation Group](https://uncg.org.ua/), supported by [NLBIF: The Netherlands Biodiversity Information Facility](https://www.nlbif.nl/), nlbif2022.014"



# Options for used-defined buffer, in meters
buffer_choices <- c(
  "Without buffer" = 0,
  "1 km" = 1000,
  "5 km" = 5000,
  "10 km" = 10000,
  "20 km" = 20000
)


txt_accobams_fullname <- "Agreement on the Conservation of Cetaceans of the Black Sea, Mediterranean Sea and contiguous Atlantic Area (ACCOBAMS)"

chku_category <- c("vulnerable", "rare", "endangered", "unvalued", "insufficiently known", "extinct in the wild")



# DOCX text blocks ####

rename_chku_fields <- c("Kingdom" = "kingdom", "Class" = "class", "Family" = "family", "Vernacular_name" = "nameUk", "Latin_name" = "scientificName", "RBU_Category" = "ЧКУ" )
rename_iucn_fields <- c("Kingdom" = "kingdom", "Class" = "class", "Family" = "family", "Vernacular_name" = "nameUk", "Latin_name" = "scientificName", "IUCN_Category" = "iucnRedListCategory" )
rename_convention_fields <- c("Kingdom" = "kingdom", "Class" = "class", "Family" = "family", "Vernacular_name" = "nameUk", "Latin_name" = "scientificName" )

rename_species_field <- c("Kingdom" = "kingdom", "Class" = "class", "Family" = "family", "Number_of_records" = "Amount", "Vernacular_name" = "nameUk", "Latin_name" = "scientificName" )

