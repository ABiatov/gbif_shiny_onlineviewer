# Environment preparation ####
rm(list = ls()) # Reset R`s brain

# Load libraries
library(tidyr)
library(dplyr)
library(rgbif)

# Load data saved at step #1
load(file = "./name_lookup/matches.Rdata")
list2env(matches, .GlobalEnv)
rm(matches)

accepted_doubtful <- goodmatch %>% 
  filter(matchType != "HIGHERRANK" 
         & status != "SYNONYM"
  )



# ----------
# This requires the newest version of rgbif
name_list <- c(
  "Cirsium arvense (L.) Scop.", 
  "Calopteryx splendens", 
  "Puma concolor (Linnaeus, 1771)", 
  "Ceylonosticta alwisi", 
  "Fake species (John Waller 2021)", 
  "Calopteryx")

name_backbone_checklist(name_list)

name_data <- data.frame(
  scientificName = c(
    "Cirsium arvense (L.) Scop.", # a plant
    "Calopteryx splendens (Harris, 1780)", # an insect
    "Puma concolor (Linnaeus, 1771)", # a big cat
    "Ceylonosticta alwisi (Priyadarshana & Wijewardhane, 2016)", # newly discovered insect 
    "Puma concuolor (Linnaeus, 1771)", # a mis-spelled big cat
    "Fake species (John Waller 2021)", # a fake species
    "Calopteryx" # Just a Genus   
  ), 
  kingdom = c(
    "Plantae",
    "Animalia",
    "Animalia",
    "Animalia",
    "Animalia",
    "Johnlia",
    "Animalia"
  ))

name_backbone_checklist(name_data)
# To return more than just the 'best' results, run
name_backbone_checklist(name_data, verbose=TRUE)


name_backbone("Leucoagaricus nympharum (Kalchbr.) Bon", verbose = TRUE)



?name_usage


# All names from the dataset
name_usage(datasetKey="bca0cf23-b459-4164-a552-9b90825ee255")

# Leucoagaricus nympharum (Kalchbr.) Bon
# Look up all synonyms according to GBIF Backbone Taxonomy
df <- name_usage(key = 6159223,
                 data = "synonyms"
                 )$data %>% 
  select(key)

# Look up all children taxa according to GBIF Backbone Taxonomy
df <- name_usage(key = 6159223,
                 data = "children"
)$data %>% 
  select(key)


View(df)
# look up iucn red list category 
name_usage(key = 2535788, data = 'iucnRedListCategory')
