CRS_used_in_calculations <- 3537 # WGS 84 / North Pole LAEA Europe
# query_limit = 50

# url_metadata_datadump <- "https://github.com/ABiatov/gbif_shiny_onlineviewer/raw/main/name_lookup/outputs/metadata.Rdata"
# url_datadump <- "https://github.com/ABiatov/gbif_shiny_onlineviewer/raw/main/name_lookup/outputs/gbif_sf_dataset.Rdata"

url_metadata_datadump <- "https://fra1.digitaloceanspaces.com/abspatial/WD/gbif_shiny_onlineviewer/metadata.Rdata"
url_datadump <- "https://fra1.digitaloceanspaces.com/abspatial/WD/gbif_shiny_onlineviewer/gbif_sf_dataset.Rdata"




# Lists of columns
## for full table (excel)
colnames_set1 <- c(
  "individualCount", "organismQuantity", "organismQuantityType",
  "eventDate", "year", "Latitude", "Longitude", "coordinateUncertaintyInMeters", "coordinatePrecision", "verbatimLocality",
  "nameUk", "scientificName", "kingdom",
  # "phylum",
  "class", "order", "family",
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

# Internetional agriments and conventions to filter
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

# Options for used-defined buffer, in meters
buffer_choices <- c(
  "немає" = 0,
  "1 км" = 1000,
  "5 км" = 5000,
  "10 км" = 10000,
  "20 км" = 20000
)

chku_category <- c("вразливий", "рідкісний", "зникаючий", "неоцінений", "недостатньо відомий", "зниклий у природі")

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



# DOCX text blocks ####

rename_chku_fields <- c("Царство" = "kingdom", "Клас" = "class", "Родина" = "family", "Українська_назва" = "nameUk", "Латинська_назва" = "scientificName", "Категорія_ЧКУ" = "ЧКУ" )
rename_iucn_fields <- c("Царство" = "kingdom", "Клас" = "class", "Родина" = "family", "Українська_назва" = "nameUk", "Латинська_назва" = "scientificName", "Категорія_IUCN" = "iucnRedListCategory" )
rename_convention_fields <- c("Царство" = "kingdom", "Клас" = "class", "Родина" = "family", "Українська_назва" = "nameUk", "Латинська_назва" = "scientificName" )

rename_species_field <- c("Царство" = "kingdom", "Клас" = "class", "Родина" = "family", "Кількість_знахідок" = "Amount", "Українська_назва" = "nameUk", "Латинська_назва" = "scientificName" )


# pallet points on map
kingdom_colors <- c("Plantae" = "#4daf4a", "Fungi" = "#377eb8", "Animalia" = "#ff7f00")






# Text blocks used in the report template file (report.Rmd)
# This file is used for generation Word report

txt_report_title <- "Дані GBIF"

txt_report_date_created <- paste("Звіт згенеровано",
                                 format(Sys.Date(), "%Y-%m-%d") # current date
                                 )

txt_report_header <- "Зведені відомості про реєстрації рідкісних та таких, що перебувають під охороною, видів"

txt_report_citation_instruction <- "Будь ласка використовуйте таку цитату в публікаціях:"

txt_report_statistics <- "Зведена статистика"

txt_chku_species <- "Види, занесені до Червоної книги України"

txt_iucn_species <- "Види, занесені до Червоного списку IUCN"

txt_BernApp_1_species <- "Види, занесені до Додатку 1 Бернської конвенції"

txt_BernApp_2_species <- "Види, занесені до Додатку 2 Бернської конвенції"

txt_BernApp_3_species <- "Види, занесені до Додатку 3 Бернської конвенції"

txt_BernRes_6_species <- "Резолюції 6 Бернської конвенції"

txt_Bonn_species <- "Види, занесені до Конвенції про збереження мігруючих видів диких тварин (Боннська конвенція)"

txt_AEWA_species <- "Види, що охороняються в рамках Угоди про збереження афро-євразійських мігруючих водно-болотних птахів (AEWA)"

txt_EUROBATS_species <- "Види, що охороняються в рамках Угоди про збереження популяцій європейських кажанів (EUROBATS)"

txt_ACCOBAMS_species <- "Види, що охороняються в рамках Угоди про збереження китоподібних Чорного моря, Середземного моря та прилеглої акваторії Атлантичного океану (ACCOBAMS)"

txt_BirdDirAnn_I_species <- "Види, занесені до Додатку I Пташиної директиви ЄС"

txt_BirdDirAnn_II_species <- "Види, занесені до Додатку II Пташиної директиви ЄС"

txt_HabitatsDirAnn_II_species <- "Види, занесені до Додатку II Оселищної директиви ЄС"

txt_HabitatsDirAnn_IV_species <- "Види, занесені до Додатку IV Оселищної директиви ЄС"

txt_HabitatsDirAnn_V_species <- "Види, занесені до Додатку V Оселищної директиви ЄС"

txt_Invasive_species <- "Інвазійні та чужорідні види"

txt_all_species <- "Загальний перелік видів"

txt_about_gbif_viewer <- "**GBIF Viewer**: an open web-based biodiversity conservation decision-making tool for policy and governance. Спільний проєкт The [Habitat Foundation](https://thehabitatfoundation.org/) та [Української Природоохоронної Групи](https://uncg.org.ua/), за підтримки [NLBIF: The Netherlands Biodiversity Information Facility](https://www.nlbif.nl/), nlbif2022.014"
