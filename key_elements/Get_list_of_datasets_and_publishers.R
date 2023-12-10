# Get list of datasets and publishers

library(rgbif)

publ_r <- "ca2fd897-6108-4361-91f8-b39dc8d12d13"

d_set <- "ffc0834b-f129-4821-b626-0f23e18be6b8"

datasets(uuid=d_set)$data$title

load("C:/Temp/GBIF_occurence_download/temp/gbif_data.Rdata")

all_ccurrences <- occ_download_import(as.download("C:/Temp/GBIF_occurence_download/temp/0061781-231002084531237.zip"))


# Get list of datasets ####

length(unique(gbif.dump$datasetKey))

f_datasetKey <- unique(gbif.dump$datasetKey)

l_dataset_titles <- datasets(uuid=f_datasetKey)$data$title

l_dataset_titles <- c()

for (i in 1:length(f_datasetKey)){
  l_dataset_titles <- c(l_dataset_titles, datasets(uuid=f_datasetKey[i])$data$title)
  print(datasets(uuid=f_datasetKey[i])$data$title)
}

length(f_datasetKey)
length(l_dataset_titles)

df_datasets <- data.frame(num = 1:length(f_datasetKey), dataset_id = f_datasetKey, title = l_dataset_titles)

df_datasets <- df_datasets[ order(df_datasets$title), ]

df_datasets$num <- c(1:length(f_datasetKey))

write.csv(df_datasets, file = "C:/Temp/GBIF_occurence_download/temp/datasets.csv", fileEncoding = "UTF-8")

# Get list of publishers ####

l_publishers <- unique(all_ccurrences$publisher)

length(l_publishers)

df_publishers <- data.frame(num = 1:length(l_publishers), publisher=l_publishers)

df_publishers <- df_publishers[ order(df_publishers$publisher), ]

df_publishers$num <- c(1:length(l_publishers))

write.csv(df_publishers, file = "C:/Temp/GBIF_occurence_download/temp/publishers.csv", fileEncoding = "UTF-8")


# Get list of basisOfRecord ####

l_basisOfRecord <- unique(all_ccurrences$basisOfRecord)

length(l_basisOfRecord)


