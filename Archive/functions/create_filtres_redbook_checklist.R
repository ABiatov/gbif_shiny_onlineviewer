# Get Red Data Book of Ukraine Cheсklist https://www.gbif.org/dataset/bca0cf23-b459-4164-a552-9b90825ee255 ####

# import libs ####

library(rgbif)      
library(dplyr)
library(purrr)

# TODO fix function
colums_as_factor <- function(df_x){
  names_df_x <- names(df_x)
  for (i in names_df_x){
    #  print(i)
    df_x[,i] <- as.factor(df_x[,i])
  }
  return(df_x)
}


# Getting checklist ####

dataset_key_red_book_animals_ua <- "bca0cf23-b459-4164-a552-9b90825ee255"

startTime_getting_checklist <- Sys.time()

response_red_book_animals_ua <- name_usage(datasetKey = dataset_key_red_book_animals_ua, limit = 10000)

# df_red_list_full_checklist  <- as.data.frame(response_red_list$data)
# df_species_red_list <- subset(df_red_list_full_checklist, rank == "SPECIES")

tibble_red_book_animals_ua_list_core <- response_red_book_animals_ua$data %>% filter(origin == "SOURCE") 

df_red_book_animals_ua_list_core <- as.data.frame(tibble_red_book_animals_ua_list_core) 

# str(tibble_red_book_animals_ua_list_core)
# str(df_red_book_animals_ua_list_core)

endTime_getting_checklist <- Sys.time()



### convert all columns to factors ####
name_df_red_book_animals_ua_list_core <- names(df_red_book_animals_ua_list_core)
for (i in name_df_red_book_animals_ua_list_core){
  #  print(i)
  df_red_book_animals_ua_list_core[,i] <- as.factor(df_red_book_animals_ua_list_core[,i])
}



## Get distribution extension ####

# distribution <- name_usage(datasetKey = dataset_key_red_book_animals_ua, data = "distribution")

startTime_distribution_preporation <- Sys.time()

df_distribution_red_book_animals_ua <- 
  tibble_red_book_animals_ua_list_core %>% 
  pull(key) %>%
  map_dfr(., function(x) {
    distribution_details <-name_usage(x, data = "distribution")
    return(distribution_details$data)
  }
  ) %>% as.data.frame()

endTime_distribution_preporation <- Sys.time()

# str(df_distribution_red_book_animals_ua)

## Get vernacularNames extension ####

startTime_vernacularname_preporation <- Sys.time()

df_vernacularname_red_book_animals_ua <- 
  tibble_red_book_animals_ua_list_core %>% 
  pull(key) %>%
  map_dfr(., function(x) {
    vernacularname_details <-name_usage(x, data = "vernacularNames")
    return(vernacularname_details$data)
  }
  ) %>% as.data.frame()

endTime_vernacularname_preporation <- Sys.time()


# str(df_vernacularname_red_book_animals_ua)

print(paste("getting checklist: ", endTime_getting_checklist - startTime_getting_checklist))
print(paste("distribution preporation: ", endTime_distribution_preporation - startTime_distribution_preporation))
print(paste("vernacularname preporation: ", endTime_vernacularname_preporation - startTime_vernacularname_preporation))
print(paste("general time: ", endTime_vernacularname_preporation - startTime_getting_checklist))

