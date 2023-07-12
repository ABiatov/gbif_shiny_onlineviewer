# Check when GBIF dataset is ready.

library(rvest)
library(httr)
library(tictoc)


path_gbif_dataset_metadata <- "data/gbif_dataset_metadata.Rdata"
load(file = path_gbif_dataset_metadata)

# URL for checking
url <- paste0("https://doi.org/", gbif_dataset_metadata$doi)


# Function to check if a page exists
check_page <- function(url) {
  response <- GET(url)
  return(status_code(response))
}

# Variable to keep track of the number of checks
attempts <- 0

# Loop to repeat checks
while (attempts < 10) {
  attempts <- attempts + 1
  
  # Page Existence Check
  status <- check_page(url)
  
  if (status == 200) {
    # If the page is present, print its content
    page <- read_html(url)
    content <- html_text(page)
    print(content)
    break
  } else {
    # If the page is missing, wait 5 minutes
    Sys.sleep(300)
  }
}

# If after 10 checks the page still does not appear
if (attempts == 10) {
  print("The page is taking too long to wait. Check if the address is correct.")
}


