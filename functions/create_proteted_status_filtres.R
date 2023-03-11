library(readxl)

xl_protected_status <- read_xlsx("C:/Users/admin/Documents/GitHub/gbif_shiny_onlineviewer/dictionaries/checklist_protected_species_UA.xlsx")

df_protected_status <- as.data.frame(xl_protected_status)

name_df_protected_status <- names(df_protected_status)

for (i in name_df_protected_status){
#  print(i)
  df_protected_status[,i] <- as.factor(df_protected_status[,i])
}

red_book_vrazliviy <- subset(df_protected_status, ЧКУ == "вразливий")[['scientificName']]

levels(df_protected_status$ЧКУ)


