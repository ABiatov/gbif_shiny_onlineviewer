library(readxl)

xl_protected_status <- read_xlsx("C:/Users/admin/Documents/GitHub/gbif_shiny_onlineviewer/dictionaries/checklist_protected_species_UA.xlsx")

df_protected_status <- as.data.frame(xl_protected_status)

str(df_protected_status)

head(df_protected_status)


red_book_vrazliviy <- subset(df_protected_status, ЧКУ = "вразливий")

