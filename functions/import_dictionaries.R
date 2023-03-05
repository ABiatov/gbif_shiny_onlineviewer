# import dictionaries ####

dict_name_ua <- read.table("dictionaries/dict_name_ua.csv", sep=',', header = TRUE) %>%
  mutate(across(c(scientificName, NameUA), as.factor))

dict_iucn_category <- read.table("dictionaries/dict_iucn_category.csv", sep=',', header = TRUE) %>%
  mutate(across(c(iucnRedListCategory, IUCN_Red_List), as.factor))

red_book_ua <- read.table("dictionaries/red_book_ua.csv", sep=',', header = TRUE) %>%
  mutate(across(fields_dict_red_book_ua, as.factor)) %>%
  dplyr::select(all_of(fields_dict_red_book_ua))
