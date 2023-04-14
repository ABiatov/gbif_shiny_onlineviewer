# Preparing list of rare species and invazive species as vectors for filters and extort them to folder "dictionaries"
# from file dictionaries/checklist_protected_species_UA.xlsx

# Environment preparation ####
rm(list = ls()) # Reset R`s brain
library(readxl)
library(dplyr)

# Inputs ####
work_path <- "C:/Users/admin/Documents/GitHub/gbif_shiny_onlineviewer/dictionaries"
xl_file_name <- "checklist_protected_species_UA.xlsx"
redbook_status_field <- "ЧКУ"

species_name <- 'base_name' 

# Flow ####

xl_filepath <- paste(work_path, xl_file_name, sep ="/")

df_protected_status <- read_xlsx(xl_filepath) %>%
  as.data.frame()

## add base_name field ####
# Base name it is 2 firsts worlds of full species name
df_protected_status[[species_name]] <- sub("(\\w+\\s+\\w+).*", "\\1", df_protected_status$scientificName)

# str(df_protected_status)
# head(df_protected_status)

## convert all fields to factor ####
# name_df_protected_status <- names(df_protected_status)
# for (i in name_df_protected_status){
# #  print(i)
#   df_protected_status[,i] <- as.factor(df_protected_status[,i])
# }

# levels(df_protected_status[[redbook_status_field]])

# Create vectors for filters ####

# "вразливий"
red_book_vrazlyvyi <- subset(df_protected_status, df_protected_status[[redbook_status_field]] == "вразливий")[[species_name]]

# "рідкісний" 
red_book_ridkisnyi <- subset(df_protected_status, df_protected_status[[redbook_status_field]] == "рідкісний")[[species_name]]

# "зникаючий"
red_book_znykaiuchyi <- subset(df_protected_status, df_protected_status[[redbook_status_field]] == "зникаючий")[[species_name]]


# "зниклий"
red_book_znyklyi <- subset(df_protected_status, df_protected_status[[redbook_status_field]] == "зниклий")[[species_name]]


# "зниклий в природі"
red_book_znyklyi_v_pryrodi <- subset(df_protected_status, df_protected_status[[redbook_status_field]] == "зниклий в природі")[[species_name]]


# "недостатньо відомий"
red_book_nedostatno_vidomyi <- subset(df_protected_status, df_protected_status[[redbook_status_field]] == "недостатньо відомий")[[species_name]]


# "неоцінений"
red_book_neotsinenyi <- subset(df_protected_status, df_protected_status[[redbook_status_field]] == "неоцінений")[[species_name]]

# BernAppendix2
bern_appendix_2 <- subset(df_protected_status, BernAppendix2 == "yes")[[species_name]]

# BernAppendix3
bern_appendix_3 <- subset(df_protected_status, BernAppendix3 == "yes")[[species_name]]

# BernResolution6
bern_resolution_6 <- subset(df_protected_status, BernResolution6 == "yes")[[species_name]]

# ЧС_Полтавська
redlist_poltavska <- subset(df_protected_status, ЧС_Полтавська == "yes")[[species_name]]

# ЧС_Чернівецька
redlist_chernivetska <- subset(df_protected_status, ЧС_Чернівецька == "yes")[[species_name]]

# Invasive
invasive_specieses <- subset(df_protected_status, Invasive == "yes")[[species_name]]


# Export Protected status info as Rdata ####

save(df_protected_status, file = "dictionaries/df_protected_status.Rdata")

save(red_book_vrazlyvyi, file = "dictionaries/red_book_vrazlyvyi.Rdata")
save(red_book_ridkisnyi, file = "dictionaries/red_book_ridkisnyi.Rdata")
save(red_book_znykaiuchyi, file = "dictionaries/red_book_znykaiuchyi.Rdata")
save(red_book_znyklyi, file = "dictionaries/red_book_znyklyi.Rdata")
save(red_book_znyklyi_v_pryrodi, file = "dictionaries/red_book_znyklyi_v_pryrodi.Rdata")
save(red_book_nedostatno_vidomyi, file = "dictionaries/red_book_nedostatno_vidomyi.Rdata")
save(red_book_neotsinenyi, file = "dictionaries/red_book_neotsinenyi.Rdata")

save(bern_appendix_2, file = "dictionaries/bern_appendix_2.Rdata")
save(bern_appendix_3, file = "dictionaries/bern_appendix_3.Rdata")
save(bern_resolution_6, file = "dictionaries/bern_resolution_6.Rdata")
save(redlist_poltavska, file = "dictionaries/redlist_poltavska.Rdata")
save(redlist_chernivetska, file = "dictionaries/redlist_chernivetska.Rdata")
save(invasive_specieses, file = "dictionaries/invasive_specieses.Rdata")


# Load files
# load(file = "dictionaries/red_book_vrazlyvyi.Rdata")
# load(file = "dictionaries/red_book_ridkisnyi.Rdata")
# load(file = "dictionaries/red_book_znykaiuchyi.Rdata")
# load(file = "dictionaries/red_book_znyklyi.Rdata")
# load(file = "dictionaries/red_book_znyklyi_v_pryrodi.Rdata")
# load(file = "dictionaries/red_book_nedostatno_vidomyi.Rdata")
# load(file = "dictionaries/red_book_neotsinenyi.Rdata")
# load(file = "dictionaries/df_protected_status.Rdata")
# load(file = "dictionaries/bern_appendix_2.Rdata")
# load(file = "dictionaries/bern_appendix_3.Rdata")
# load(file = "dictionaries/bern_resolution_6.Rdata")
# load(file = "dictionaries/redlist_poltavska.Rdata")
# load(file = "dictionaries/redlist_chernivetska.Rdata")
# load(file = "dictionaries/invasive_specieses.Rdata")


# Environment preparation ####
rm(list = ls()) # Reset R`s brain
