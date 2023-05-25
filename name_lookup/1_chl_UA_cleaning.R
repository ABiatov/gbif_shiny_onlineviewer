rm(list = ls()) # Reset R`s brain
setwd("~/git/gbif_shiny_omlineviewer/name_lookup")

library(tidyr)
library(dplyr)

df.fullredlist <- read.csv("checklist-protected-species-UA-template-v24-cleaned_20230522.csv")
df.normalized <- read.csv("normalized.csv")

# Quick lookup
str(df.fullredlist)
str(df.normalized)

df.normalized <- df.normalized |>
  select(2:9) |>
  mutate_if(is.character, as.factor)

head(df.normalized)
levels(df.normalized$matchType)

# Exact matching
exact_accepted <- df.normalized |> 
  filter(matchType == "EXACT",
         status == "ACCEPTED")

exact_synonym <- df.normalized |> 
  filter(matchType == "EXACT",
         status == "SYNONYM")

exact_doubtful <- df.normalized |> 
  filter(matchType == "EXACT",
         status == "DOUBTFUL")


# Merge with full checklist
df.merged <- df.fullredlist |>
  left_join(df.normalized, by = join_by(verbatimScientificName)) |>
  mutate(ID = 1:nrow(df.normalized)) |> # Assign internal IDs
  relocate(ID) |>
  relocate(key, .after = ID) |>
  relocate(scientificName, .after = verbatimScientificName) |> 
  select(-kingdom.y) |> rename(kingdom = kingdom.x)



# Splitting by matchType
goodmatch <- df.merged |> 
  filter(matchType != "HIGHERRANK")

badmatch <- df.merged |> 
  filter(matchType == "HIGHERRANK")

# Save for separate occurrence search
matches <- list(goodmatch, badmatch)
names(matches) <- c("goodmatch", "badmatch")


save(matches, file = "matches.Rdata")

# Not exact matching (need manual review)
badmatch |> 
  select(ID, verbatimScientificName) |> 
  write.csv("higherrank.csv")

# End of script
