
# Country code to download GBIF data
country <- "UA"

# How much get existing occurrenceID for each name ID from goodmatch and badmatch
occ_search_limit = 100000 

iucnRL_category = c("EX", "EW", "CR", "EN", "VU", "NT", "DD")

# Vector of datasetKeys for the dataset occurrences from which we 
# deliberately drop from the data
dropped_datasets <- c("c779b049-28f3-4daf-bbf4-0a40830819b6") # EBCC Atlas of European Breeding Birds

# Fields list for export
colnames_set0 <- c(
  "individualCount", "organismQuantity", "organismQuantityType",
  "eventDate", "year", "Latitude", "Longitude", "coordinateUncertaintyInMeters", "coordinatePrecision", "verbatimLocality",
  "nameUk", "scientificName", "kingdom",
  #"phylum",
  "class", "order", "family",
  #"genus",
  "ЧКУ",
  "iucnRedListCategory",
  "BernAppendix1", "BernAppendix2", "BernAppendix3", "BernResolution6",
  "Bonn",
  "AEWA",
  "EUROBATS",
  "ACCOBAMS",
  "BirdsDirectiveAnnex_I", "BirdsDirectiveAnnex_IІ",
  "HabitatsDirectiveAnnex_II", "HabitatsDirectiveAnnex_IV", "HabitatsDirectiveAnnex_V",
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

# Limit for coordinate uncertainty in meters (occurrences with uncertainty above 
# the threshold will be dropped)
coordUncert.threshold <- 500

# Minimum coordinate precision (occurrences with precision above the threshold will be dropped)
# https://dwc.tdwg.org/terms/#dwc:coordinatePrecision
coordinatePrec.threshold <- 0.001
