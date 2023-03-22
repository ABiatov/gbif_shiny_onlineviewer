library(digest)
library(htmltools)
library(mime)
library(shiny) #Display
library(shinydashboard)  #Display
library(ggplot2) #for the plots
library(DT)  #For the tables
library(leaflet)  #Maps
library(jsonlite)  
library(rgbif)  #GBIF data
library(raster)  #for working with rasters
library(shinycssloaders)  #This makes the loading symbols
library(CoordinateCleaner)  #Cleans the Data
library(rnaturalearth)  #Maps for leaflet
#library(mapview) #for downloading leaflet map

choice <- function(input_num) {
  Gbif_fields <- read.csv("../data/gbif_fields.csv", as.is = TRUE)
  Country_codes <- read.csv("../data/countryCodes.csv", as.is = TRUE)
  # Note to Self -- [row,col] cols: colName, MetaData, Minimal, Default, Custom
  #Sets lis to a list of the row names selected and sets selected to a list of values in the column
  if (input_num ==1){
    lis<-Gbif_fields$Column.Name[Gbif_fields$Minimal == 1]
    selected <- Gbif_fields$Minimal
  }
  else if (input_num == 2) {
    lis<-Gbif_fields$Column.Name[Gbif_fields$Default == 1]
    selected <- Gbif_fields$Default
  }
  else if (input_num == 3){
    lis<-Gbif_fields$Column.Name[Gbif_fields$Custom == 1]
    selected <- Gbif_fields$Custom
  }
  else {
    lis <- Country_codes$CountryCodes
    selected <- Country_codes$CountryCodes
    lis2 <- Country_codes$CountryCodes
    final_list <- list()
    for (i in 1:304){
      final_list[[lis[i]]] <- lis2 [i]
    }
    return(final_list)
  }
  lis2 <- c()
  #Sets lis2 to a list of each row number that corresponds to a value in lis
  for (i in 1:length(Gbif_fields$Column.Name)){
    if (selected[i] == 1) {
      lis2 <- c(lis2,i)
    }
  }
  final_list <- list()
  #combines lis and lis2 into a format that can be used by the choices variabe in checkboxGroupInput()
  for (i in 1:length(lis)) {
    final_list[[lis[i]]] <- as.double(lis2 [i])
  }
  return(final_list)
}
data_ID <- "" #holds the citation info
data_meta <- "" #holds data for getting citation
clean_data_exists <- FALSE #holds a boolean for telling if cleaned data has been created
clean_bias_data <- "" #holds the cleaned data once created
#create a user interface
ui <- dashboardPage(
  #App title
  dashboardHeader(title = 'SOAR'),
  
  #sidebar with input fields
  dashboardSidebar(
    #get species name
    textInput("species_name", "Latin name", "Caretta caretta"),
    selectInput("rank", "Taxonomic rank", 
                choices = list("Species" = "Species","Genus" = "Genus", "Family" = "Family","Order" = "Order", 
                               "Class" = "Class", "Phylum" = "Phylum", "Kingdom" = "Kingdom"), selected = "Species"),
    
    #get login information
    textInput("gbif_username", "Gbif Username", "Ex: GbifUser1313"),
    passwordInput("gbif_pass", "Gbif Password", "Ex: GbifPass2424"),
    textInput("gbif_email", "Gbif Email", "Ex: JaneSmith@generic.com"),
    
    #input latlong bounding box
    checkboxInput("lat_long_check_box", label = "Input Geographical Boundaries with Latitude and Longitude", 
                  value = FALSE),
    conditionalPanel (
      condition = "input.lat_long_check_box == true && input.input_file_checkbox == false && input.down_key_checkbox == false",
      numericInput("Lat_low", "Latitude lower range", "-90", min = -90, max = 90),
      numericInput("Lat_high", "Latitude upper range", "90", min = -90, max = 90),
      numericInput("Long_low", "Longitude lower range", "-180", min = -180, max = 180),
      numericInput("Long_high", "Longitude upper range", "180", min = -180, max = 180)
    ),
    
    #input bounding box with political boundaries
    checkboxInput("political_checkbox", label = "Input Geographical boundaries by country"),
    conditionalPanel(
      condition ="input.political_checkbox == true && input.input_file_checkbox == false && input.down_key_checkbox == false",
      selectInput("political_boundary", "Country Name", choices = choice(4), selected = "US")
      ),
   
     #input date bounding information for gbif
    checkboxInput("date_checkbox", label = "Sort by date", value = FALSE),
    conditionalPanel (
      condition = "input.date_checkbox == true && input.input_file_checkbox == false && input.down_key_checkbox == false",
      dateInput("from_date", "Show results from this date:"),
      dateInput("to_date", "to this date:")
    ),
  
    #input specific download key
    checkboxInput("down_key_checkbox", label = "Specify Download Key", value = FALSE),
    conditionalPanel (
      condition = "input.down_key_checkbox == true && input.input_file_checkbox == false",
      textInput("down_key", "Download Key:", "" )
    ),
    
    #input file name to import
    checkboxInput("input_file_checkbox", label = "Import Gbif Data File", value = FALSE),
    conditionalPanel(
      condition = "input.input_file_checkbox == true",
      textInput("file_name", "Gbif Data File(format: file_name.zip):" ,"")
    ),
      
    actionButton("do", "Submit")
  ),
   
  #show results here
  dashboardBody(
    tabsetPanel(
      tabPanel("Map",
               downloadButton('download_dataset_params', label = "Download Parameters used to find this data"), 
               withSpinner(leafletOutput("world_map"))
               #, actionButton('map_do', "Download This Map")
               ),
      #now also gives the ID for citing the data
      tabPanel("Raw Data", DT::dataTableOutput("raw_data")),
      tabPanel("Download Raw Data", textOutput("data_ID_return"), h4(""), h4("Citation:"), 
               textOutput("data_citation_return"), actionButton("do_ID", "Refresh Data ID for citations"),
               radioButtons("table_cols", label = "Columns in Downloadable table",
                                  choices = list("Minimal" = 1, "Default" = 2, "All Columns" = 3, "Custom" = 4), 
                                  selected = 1),
               downloadButton('download_data', label = "Download Table"),
               conditionalPanel(condition = "input.table_cols == 1", 
                                checkboxGroupInput("table_min", label = h4("Minimal Options:"), choices =
                                                   choice(1), 
                                                   selected = c(133, 134, 175, 220, 229))),
               conditionalPanel(condition = "input.table_cols == 2",
                                checkboxGroupInput("table_def", label= h4("Default Options:"), choices = choice(2),
                                                   selected = c(43,47,48,60,65,69,75,76,106,121,133:135,175,183,191:200,207,219,229,230))
                                ),
               conditionalPanel(condition = "input.table_cols == 3",
                                h4("Table will contain all columns.")),
               conditionalPanel(condition = "input.table_cols == 4",
                                #checkboxes with 235 options
                                checkboxGroupInput("table_cus", label= h4("Select Options (Defaults Options Selected):"), choices = choice(3),
                              selected = c(43,47,48,60,65,69,75,76,106,121,133:135,175,183,191:200,207,219,229,230))
                                )
               ),
              
      tabPanel("Clean Data", 
               h4("Flag possibly erronious data based on:"),
               checkboxInput("cap_prox_box", label = "Proximity to capitals", value = FALSE),
               checkboxInput("cen_prox_box", label = "Proximity to country centroids", value = FALSE),
               checkboxInput("lat_long_check", label = "Actual coordinate location vs. country specified by the data (It is recomended that this not be selected if oceanic species are involved)", value = FALSE),
               checkboxInput("rec_dup", label = "Duplications of records", value = FALSE), 
               checkboxInput("iden_lat_long", label = "Records with identical coordinates", value = FALSE),
               checkboxInput("gbif_prox", label = "Proximity to Gbif headquarters", value = FALSE),
               checkboxInput("bio_prox", label = "Proximity to biodiversity institutions", value = FALSE),
               checkboxInput("out_box", label = "Outliers", value = FALSE),
               checkboxInput("sea_box", label = "Locaton relative to the oceans (check if records include only terrestrial organisms)", value = FALSE),
               checkboxInput("equ_zero_zero", label = "Equal latitude and longitude, plain zeros, and proximity to point 0/0", value = FALSE),
              
               
              #These are conditional, only available if their corresponding box is checked
               conditionalPanel(condition = "input.cap_prox_box == true",
                                numericInput("cap_rad", "Radius around capitals (degrees)", 0.1)
                                ),
               conditionalPanel(condition = "input.cen_prox_box == true",
                                numericInput("cen_rad", "Side length of rectangle around country centroids (degrees)", 0.01),
                                selectInput("cen_detail", "Test around country centroids, province centroids, or both", 
                                            choices = list("Country" = 1, "Province" = 2, "Both" = 3), selected = 3)
                                ),
               conditionalPanel(condition = "input.bio_prox == true | input.gbif_prox == true",
                                numericInput("inst_rad", "Radius around biodiversity institutions, including GBIF (degrees)", 0.001)
                                ),
               conditionalPanel(condition = "input.out_box == true", 
                                selectInput("out_type", "What outlier test would you like to use?", 
                                            choices = list("Boxplot distance of records from each other" = 1, 
                                                           "Mean absolute deviation of the distance of records from each other" = 2,
                                                           "Minimum distance to next record is greater than selected" = 3)
                                            #1 = "outlier", 2 = "mad", 3 = "distance"
                                            ),
                                conditionalPanel(condition = "input.out_type == 2", 
                                                 numericInput("out_mtp","Multiplier for interquartile range",3) ),
                                conditionalPanel(condition = "input.out_type == 3",
                                                 numericInput("out_td","Minimum distance of a record to all other records to be identified as an outlier (km)",1000) ),
                                numericInput("out_size", "Minimum number of records to run the taxon-specific outlier test", 7)
                                ),
               conditionalPanel(condition = "input.equ_zero_zero == true",
                                numericInput("zero_rad", "Radius around 0/0 (degrees)", 0.5)
               ),
               #provide these (do not require as input):
                #capitals.ref -- works fine on its own
                #centroids.ref -- works fine on its own
                #country.ref -- Works fine with rnaturalearth added in
                #inst.ref -- works fine on its own
                #seas.ref -- Works fine on its own
                #urban.ref
               selectInput("value_input", "What kind of output would you like?", 
                           choices = list("Spatial Valid - A separate document with what data was flagged and for what test" = 1, "Clean - the flagged data is automatically removed" = 2), selected = 1),
               #verbose = False
              actionButton("do_clean", "Submit")
               ),
      tabPanel("Detect Bias",
               checkboxInput("cleaned_bias", label = "Use cleaned data in this tab", value = FALSE),
               textOutput("cleaned_bias_boolean"),
               h4("Temporal Distribution", h5("Information can become less accurate depending on when it was collected. These percentages are for refrence in determining the accuracy of your dataset."),
                  h6(textOutput("pre1950"),"% of the dataset was collected before 1950"),
                  h6(textOutput("pre1970"),"% of the dataset was collected before 1970"),
                  h6(textOutput("pre1990"),"% of the dataset was collected before 1990")),
                  h5(), #For a new line
                  h5("The plot below shows the number of observtions in the current dataset per year. Take this into consideration when comparing
                     the number of observations from one year to another.", withSpinner(plotOutput("temporal_bias_plot"))),
               h4("Spatial Bias"),
                  h5("The map below shows the occurrences in the selected dataset (black) overlaid with the dataset 
                      specified in the boxes above the map(red). Take this into consideration when 
                     comparing how many occurrences are reported in one area rather than another.",
                     radioButtons("spat_bias_comp_data", label = "Spatial Bias Comparison Dataset",
                                  choices = list("Full GBIF Database" = 1, "Specific Dataset" = 2), 
                                  selected = 2),
                     conditionalPanel( condition = "input.spat_bias_comp_data == 2",
                     textInput("bias_name", "Latin name", "Caretta caretta"),
                     selectInput("bias_rank", "Taxonomic rank", 
                                 choices = list("Species" = "Species","Genus" = "Genus", "Family" = "Family","Order" = "Order", 
                                                "Class" = "Class", "Phylum" = "Phylum", "Kingdom" = "Kingdom"), selected = "Species")
                     ),
                     actionButton("spatial_do", "Use this dataset for spatial bias comparison"), h4(),
                     withSpinner(leafletOutput("spatial_bias_map")) ),
                  h5("The plot below shows the correlation between the percent of total observations
                     per map pixel for the selected dataset and the percent of total observations per
                     map pixel for the comparison dataset selected above. Take this into consideration when 
                     comparing how many occurrences are reported in one area rather than another.",
                     withSpinner(plotOutput("spatial_bias_plot")))),
      tabPanel("Download Cleaned Data", 
               h4("'True' means the data passed the tests indicated, 'False' means it failed"),
               downloadButton('download_clean_data_params', label = "Download text document with parameters used to create this table"),
               downloadButton('download_clean_data', label = "Download Table"),
               withSpinner(DT::dataTableOutput("clean_data")))
    )
  )
)
#server- logic of the app
server <- function(input, output) {
  
  output$data_ID_return <- renderText({paste("Your ID for finding this data again:", retrieve_data_ID())})
  retrieve_data_ID <- eventReactive(input$do_ID, {return(data_ID)})
  retrieve_data_citation <- eventReactive(input$do_ID, {
    if (typeof(data_meta) == "logical"){
      return("A citation cannot be created from an uploaded file")
    } 
    else {
      data_meta <<- occ_download_meta(data_ID)
    return(gbif_citation(data_meta)$download)
    }
    })
  output$data_citation_return <- renderText({retrieve_data_citation()})
  
  gbif_data <- eventReactive(input$do, {
    #import file
    if (input$input_file_checkbox){
    data_file_name = input$file_name
    }
    else { #to make sure this doesn't run when a file is uploaded (to work w/o internet)
    #filter by name
    sp_key <- name_suggest(q =input$species_name, rank = input$rank)$key[1]
    }
    #filter by country
    if (input$political_checkbox){ country <- input$political_boundary}
    #filter by date
    if (input$date_checkbox){
      to_m <- substr(input$to_date, 6,7)
      from_m<- substr(input$from_date, 6,7)
      to_y <- substr(input$to_date, 1, 4)
      from_y <- substr(input$from_date, 1, 4)
    }
    else{
      to_m <- substr(Sys.Date(), 6,7)
      from_m<- "1"
      to_y <- substr(Sys.Date(), 1, 4)
      from_y <- "1600"
    }
    #filter by lat/long
    if (input$lat_long_check_box){
      if (is.null(input$Lat_low ) ||  is.null(input$Lat_high) ||  is.null(input$Long_low) || is.null(input$Long_high)){
        low_lat <- -90
        high_lat <- 90
        low_long <- -180
        high_long <- 180
      }
      else{
      low_lat <- input$Lat_low
      high_lat <- input$Lat_high
      low_long <- input$Long_low
      high_long <- input$Long_high
    }
    }
    else {
      low_lat <- -90
      high_lat <- 90
      low_long <- -180
      high_long <- 180
    }
    
    if (input$input_file_checkbox) {
      #Works, but says file does not exist
      file = substring(data_file_name,1,23)
      data_ID <<- "A key cannot be determined from an uploaded file" #update the citation info
      dat <- occ_download_import(key = file)
      data_meta <<- FALSE
      return(dat)
    }
    else if (input$down_key_checkbox){
      continue = FALSE
      data_ID <<- toString(input$down_key) #update the citation info
      dat <- occ_download_get(key = toString(input$down_key), overwrite = TRUE)# %>% occ_download_import()
      data_meta <<- dat
      dat <- occ_download_import(dat)
      return(dat)
    }
    else{
      continue = TRUE
      if (input$political_checkbox){
        res = occ_download(paste("decimalLatitude >=", low_lat), paste("decimalLatitude <=", high_lat),
                           paste("decimalLongitude >=", low_long), paste("decimalLongitude <=", high_long),
                           paste("year >=", from_y), paste("year <=", to_y), paste("month >=", from_m),
                           paste("month <=", to_m), paste("country =", country), 
                           paste("taxonKey =", sp_key), 'hasCoordinate = TRUE',
                           user = input$gbif_username, pwd = input$gbif_pass, 
                           email = input$gbif_email)
      }
      else{
        res = occ_download(paste("decimalLatitude >=", low_lat), paste("decimalLatitude <=", high_lat),
                           paste("decimalLongitude >=", low_long), paste("decimalLongitude <=", high_long),
                           paste("year >=", from_y), paste("year <=", to_y), paste("month >=", from_m),
                           paste("month <=", to_m), 
                           paste("taxonKey =", sp_key), 'hasCoordinate = TRUE',
                           user = input$gbif_username, pwd = input$gbif_pass, 
                           email = input$gbif_email)
      }
    }
    
    #updates the data_ID
    data_ID <<- toString(res)
#loops so that it checks every 30 seconds to see if meta$status is "SUCCEEDED" or "KILLED"
    while(continue){
      meta = occ_download_meta(res)
       if (meta$status == "SUCCEEDED"){
        continue = FALSE
        dat <- occ_download_get(res[1], overwrite = TRUE) %>%
          occ_download_import()
        return(dat)
       }     
      if (meta$status == "KILLED"){
        continue = FALSE
        #Returns: ERROR:Bad Request(HTTP 400)
      }
      #30 second delay --  Extend?
      Sys.sleep(30)
    }
      
  })
  
#Displays Entire Dataset
  #-Changed gbif_data() to download_info() to only show cols selected by user
  output$raw_data <- DT::renderDataTable(expr = download_info(),options=list(autoWidth = TRUE,scrollX=TRUE))
  
#Display Cleaned Dataset based on criteria  
  output$clean_data <- DT::renderDataTable(expr = clean_info(),options=list(autoWidth = TRUE,scrollX=TRUE))
  
  #Clean Data based on input
  clean_info <- eventReactive(input$do_clean, {
    #call Coordinate Cleaner
    selected_tests = NULL
    if (input$cap_prox_box){ selected_tests = c(selected_tests, "capitals")}
    if (input$cen_prox_box){selected_tests = c(selected_tests, "centroids")}
    if (input$lat_long_check){selected_tests = c(selected_tests, "countries")}
    if (input$rec_dup){selected_tests = c(selected_tests, "duplicates")}
    if (input$iden_lat_long){selected_tests = c(selected_tests, "equal")}
    if (input$gbif_prox){selected_tests = c(selected_tests, "gbif")}
    if (input$bio_prox){selected_tests = c(selected_tests, "institutions")}
    if (input$out_box){selected_tests = c(selected_tests, "outliers")}
    if (input$sea_box){selected_tests = c(selected_tests, "seas")}
    if (input$equ_zero_zero){selected_tests = c(selected_tests, "zeros")}
    
    
    dat = clean_coordinates(x = gbif_data(), lon = "decimalLongitude", lat = "decimalLatitude",
                            species = "species",
                            countries = "countryCode",
                            tests = selected_tests,
                            capitals_rad = input$cap_rad, 
                            centroids_rad = input$cen_rad,
                            centroids_detail = cen_detail_val(), 
                            inst_rad = input$inst_rad,
                            outliers_method = out_type_val(), 
                            outliers_mtp = input$out_mtp, 
                            outliers_td = input$out_td,
                            outliers_size = input$out_size, 
                            #range_rad = 0, #not using
                            zeros_rad = input$zero_rad,
                            #capitals_ref = NULL, 
                            #centroids_ref = NULL, 
                            #country_ref = NULL,
                            #inst_ref = NULL, 
                            #range_ref = NULL, #no
                            #seas_ref = NULL,
                            #seas_scale = 50, #not gonna happen
                            #urban_ref = NULL, 
                            value = value_input_val(),
                            verbose = FALSE, 
                            report = FALSE)
    return(dat)
  })
  
  #tells the user if cleaned data is not available
  output$cleaned_bias_boolean <- renderText({cleaned_boolean_check()})
  
  #resets cleaned data boolean variable if the input$do button is pressed again
  observeEvent(input$do, {
    clean_data_exists <<- FALSE
    })
  
  #updates clean_data boolean when cleaned data is created and stores the info
  observeEvent( input$do_clean, {
    selected_tests = NULL
    if (input$cap_prox_box){ selected_tests = c(selected_tests, "capitals")}
    if (input$cen_prox_box){selected_tests = c(selected_tests, "centroids")}
    if (input$lat_long_check){selected_tests = c(selected_tests, "countries")}
    if (input$rec_dup){selected_tests = c(selected_tests, "duplicates")}
    if (input$iden_lat_long){selected_tests = c(selected_tests, "equal")}
    if (input$gbif_prox){selected_tests = c(selected_tests, "gbif")}
    if (input$bio_prox){selected_tests = c(selected_tests, "institutions")}
    if (input$out_box){selected_tests = c(selected_tests, "outliers")}
    if (input$sea_box){selected_tests = c(selected_tests, "seas")}
    if (input$equ_zero_zero){selected_tests = c(selected_tests, "zeros")}
    
    
    dat = clean_coordinates(x = gbif_data(), lon = "decimalLongitude", lat = "decimalLatitude",
                            species = "species",
                            countries = "countryCode",
                            tests = selected_tests,
                            capitals_rad = input$cap_rad, 
                            centroids_rad = input$cen_rad,
                            centroids_detail = cen_detail_val(), 
                            inst_rad = input$inst_rad,
                            outliers_method = out_type_val(), 
                            outliers_mtp = input$out_mtp, 
                            outliers_td = input$out_td,
                            outliers_size = input$out_size, 
                            #range_rad = 0, #not using
                            zeros_rad = input$zero_rad,
                            #capitals_ref = NULL, 
                            #centroids_ref = NULL, 
                            #country_ref = NULL,
                            #inst_ref = NULL, 
                            #range_ref = NULL,
                            #seas_ref = NULL,
                            #seas_scale = 50, 
                            #urban_ref = NULL, 
                            value = value_input_val(),
                            verbose = FALSE, 
                            report = FALSE)
    clean_data_exists <<- TRUE
    clean_bias_data <<- dat
    })
  
  #tells user if clean data exists when they try to use it for the bias tab
  cleaned_boolean_check <- eventReactive(input$cleaned_bias == TRUE, {
    if (clean_data_exists && input$cleaned_bias == TRUE){
      return("")
    }
    if (clean_data_exists == FALSE && input$cleaned_bias == TRUE){
      return("Cleaned dataset has not yet been created, see Clean Data tab and Download Cleaned Data Tab")
      }
    
    })
 
  #Help determine meaningful input for clean_info 
  cen_detail_val <- function(){
    if (input$cen_detail == 1){
      return("country")
    }
    else if(input$cen_detail == 2){
      return("provinces")
    }
    else{
      return("both")
    }
  }
  value_input_val <- function(){
    if (input$value_input == 1){
      return("spatialvalid")
    }
    else {
      return("clean")
    }
  }
  out_type_val <- function(){
    if (input$out_type == 1){
      return("quantile")
    }
    else if (input$out_type == 2){
      return("mad")
    }
    else{
      return("distance")
    }
  }
  
  #Determine meaninful input for clean_info when creating the file
  out_type_val_file <- function(){
    if (input$out_type == 1){
      return("Boxplot distance")
    }
    else if (input$out_type == 2){
      return("Mean absolute deviation")
    }
    else{
      return("Minimum distance")
    }
    }
  
  #gives the proper data to the bias functions
  bias_data <- function(){
    if (clean_data_exists && input$cleaned_bias == TRUE){
      dat <- clean_bias_data
      return(dat)
      }
    else{
      dat <- gbif_data()
      return(dat)
      }
    }
  
  #Determine percentages of the dataset that was collected prior to 1990,1970, and 1950
  temporal_bias <- function(num){
    dat = bias_data()
    #narrow dat down so that it only contains the years
    year = dat$year
    pre1990 = 0
    pre1970 = 0
    pre1950 = 0
    for (i in seq_along(year)) {
      if (year[i] < 1950){
        pre1950 = pre1950 + 1
        pre1970 = pre1970 + 1
        pre1990 = pre1990 + 1
      }
      else if (year[i] < 1970) {
        pre1970 = pre1970 + 1
        pre1990 = pre1990 + 1
      }
      else if (year[i] < 1990) {
        pre1990 = pre1990 + 1
      }
    }
    
    if (num == 1){
    return((pre1990/(length(year)))*100)
    }
    else if (num == 2) {
    (pre1970/(length(year)))*100
    }
    else {
    (pre1950/(length(year)))*100
    }
  }
 
  #actually pass back the data
  output$pre1950 <- renderText(
    temporal_bias(3)
  )
  output$pre1970 <- renderText(
    return(temporal_bias(2))
  )
  output$pre1990 <- renderText(
    return(temporal_bias(1))
  )
  
  #gets data for spatial bias plot
  # 1 == full gbif database
  # 2 == specified database
  spatial_bias_data <- eventReactive(input$spatial_do,{
    if (input$spat_bias_comp_data == 2){
      latin_name <- input$bias_name
      rank <- input$bias_rank
      sp_key <- name_suggest(q =latin_name, rank = rank)$key[1]
      #This line is supposed to retrieve the actual data, 
      spec_data_raster <- map_fetch(taxonKey = sp_key, srs = "EPSG:3857")
      fullDat <- occ_download_import(key = "0019660-190415153152247") #so the code doesn't crash
      return(fullDat) # should return spec_data_raster eventually
      #return(spec_data_raster)
    }
    else {
      #Full_Data_Raster <- map_fetch(srs = "EPSG:3857")
      #writeRaster(x = Full_Data_Raster, filename = "full_GBIF_mapfetch.grd", format = "raster")
      full_data_raster <- raster("full_GBIF_mapfetch.grd")
      fullDat <- occ_download_import(key = "0019660-190415153152247") #so the code doesn't crash
      return(fullDat) #should be changed  to full_data_raster eventually
      #return(full_data_raster)
    }
  })
  
  #Currenlty just gets the data for the graph ready
  # 1- list of years 
  # 2- species of interest observations per year
  # 3- full dataset observations per year
  temporal_bias_data <-function(num){
    dat <- bias_data()$year
    currYear <- as.numeric(substring(Sys.Date(),1,4));
    #FILL IN FOR FULL DATA LATER
    if (num == 3){
      #Doing this rather than the whole download sequence allows it to read the 
      #file (of the same name as the key) rather than downloading the dataset again.
      #This also speeds up the code significantly
    fullDat <- occ_download_import(key = "0019660-190415153152247") 
    fullDat <- fullDat$year
    fullDatYear <- list()
    
    for (i in 1:(currYear-1599)) {fullDatYear[i] = 0}
    
    for (i in 1:length(fullDat)){
      curr = as.numeric(fullDat[i])
      fullDatYear[curr - 1599] = (as.numeric(fullDatYear[curr-1599])) + 1
    }
    }
    datYear <- list()
    years <- list()
    
    for (i in 1:(currYear-1599)) {datYear[i] = 0}
    for (i in 1:(currYear-1599)) {years[i] = i + 1599}
    for (i in 1:length(dat)){
      curr = as.numeric(dat[i])
      datYear[curr - 1599] = (as.numeric(datYear[curr-1599])) + 1
    }

    if (num == 1) {return(years)}
    if (num == 2) {return(datYear)}
    if (num == 3) {return(fullDatYear)}
  }
  
  output$temporal_bias_plot <- renderPlot({
    Year <-temporal_bias_data(1)
    Number_Of_Observations <-  temporal_bias_data(2)
    plot(Year, Number_Of_Observations , type = "l")
  })
  
  #Output a plot that shows the range of where all Gbif data is from Vs. where 
  #only the selected data is from
  output$spatial_bias_map <- renderLeaflet({
    #Full Data -> spatial_bias_data() -- red
    #Sample Data -> bias_data() -- black
    selected_data <- bias_data()
    leaflet(spatial_bias_data()) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
      
      addCircleMarkers(lng = ~ decimalLongitude, lat = ~ decimalLatitude,
                       radius = 3,
                       color = 'black',
                       stroke = FALSE, fillOpacity = 0.5
      ) %>%
      addCircleMarkers(data = selected_data, lng = ~ decimalLongitude, lat = ~ decimalLatitude,
                       radius = 3,
                       color = "red",
                       stroke = FALSE, 
                       fillOpacity = 0.5)
  })
  
  output$spatial_bias_plot <- renderPlot({
    interest <- spatial_bias_raster("selected_sp")
    Total <- spatial_bias_raster("all_sp")
    plot(Total,interest, pch=1, cex=1,
         xlab = "Percent of Comparison Dataset Occurrences Per Cell",
        ylab = "Percent of Species of Interest occurrences per cell")
    dat <- getValues(interest)
    fullDat <- getValues(Total)
    corr = cor(fullDat,dat, use = "complete.obs")
    legend("topleft", paste("r = ", corr),bty = "n")
    abline(a=0,b=1)
  })
  
  #Create a rasta file that can be used in a comparison graph for how many 
  #results are in a cell per gbif vs. how many results are in a cell for 
  #the collected dataset
  spatial_bias_raster <- function(input = c("selected_sp", "all_sp")){
    #create and return the raster files depending on input
    # 1 = downloaded dataset
    # 2 = Full dataset
    if (input == "selected_sp"){
      dat = bias_data();
      crds = cbind(dat$decimalLongitude, dat$decimalLatitude)
      crds_sp = SpatialPoints(crds, proj4string = CRS("+proj=longlat +ellps=WGS84"))
      r = raster(nrows=116, ncols=364, xmn=-20037.51, xmx=20002.49, ymn=-6396.115,
                 ymx=6363.885, crs = CRS("+proj=cea +units=km +ellps=WGS84"))
      crds_cea = spTransform(crds_sp, CRSobj = CRS(proj4string(r)))
      ct = rasterize(crds_cea, r, fun='count')
      #comment out this line later
      return(ct)
      
      #this will turn the full numbers in to percentages of the max to match the values
      #from the map_fetch() functions ( values on a scale of 0 to 1)
      #max <- 0
      #for (i in 1:42224){
      #  if (!is.na(ct@data@values[i])){
      #    if (ct@data@values[i] > max){
      #      max <- ct@data@values[i]
      #    }
      #  }
      #}
      #
      #
      #for (i in 1:42224){
      #  if (is.na(ct@data@values[i])){
      #    ct@data@values[i] = 0
      #  }
      #  if (!is.na(ct@data@values[i])){
      #    ct@data@values[i] = (ct@data@values[i] / max)
      #  }
      #}
      #return(ct)
      
    }
    else {
      dat = spatial_bias_data();
      crds = cbind(dat$decimalLongitude, dat$decimalLatitude)
      crds_sp = SpatialPoints(crds, proj4string = CRS("+proj=longlat +ellps=WGS84"))
      r = raster(nrows=116, ncols=364, xmn=-20037.51, xmx=20002.49, ymn=-6396.115,
                 ymx=6363.885, crs = CRS("+proj=cea +units=km +ellps=WGS84"))
      crds_cea = spTransform(crds_sp, CRSobj = CRS(proj4string(r)))
      ct = rasterize(crds_cea, r, fun='count')
      return(ct)
    }
  }
  
#Default Cols? [,c(43,47,48,60,65,69,75,76,106,121,133:135,175,183,191:200,207,219,229,230)]
#Makes A Downloadable Table for only the columns selected
  download_info <- function(){
    gDat <- gbif_data()
    if(input$table_cols == 1){
      return(gDat[ , as.numeric(input$table_min)])
    }
    else if(input$table_cols == 2){
      return(gDat[ , as.numeric(input$table_def)])
    }
    else if(input$table_cols == 3){
      return(gDat)
    }
    else{
      return(gDat[ , as.numeric(input$table_cus)])
    }
  }
  
  output$download_data <- downloadHandler(
    filename = function(){"gbifDat.csv"},
    content = function(file){
      write.csv(download_info(), file)
    }
  )
  
  #for saving parameters from cleaning data
  output$download_clean_data_params <- downloadHandler(
    filename = function(){"Clean_Data_Parameters.txt"},
    content = function(file){
      sink(file)
      cat("This file is to store the parameters used to clean the most recent dataset\n\n")
      currVar <- input$cap_prox_box
      cat(paste("Capital Proximity Check:", currVar, "\n"))
      currVar <- input$cen_prox_box
      cat(paste("Centroid Proximity Check :", currVar, "\n"))
      currVar <- input$lat_long_check
      cat(paste("Country Vs. Location Check :", currVar, "\n"))
      currVar <- input$rec_dup
      cat(paste("Duplicate Check :", currVar, "\n"))
      currVar <- input$iden_lat_long
      cat(paste("Identical Coordinate Check :", currVar, "\n"))
      currVar <- input$gbif_prox
      cat(paste("GBIF Headquarters Proximity Check :", currVar, "\n"))
      currVar <- input$bio_prox
      cat(paste("Biodiversity Institution Proximity Check :", currVar, "\n"))
      currVar <- input$out_box
      cat(paste("Outlier Check :", currVar, "\n"))
      currVar <- input$sea_box
      cat(paste("Ocean Check :", currVar, "\n"))
      currVar <- input$equ_zero_zero
      cat(paste("Equal Lat/Long, Plain Zero, and 0/0 Check  :", currVar, "\n"))
      currVar <- input$cap_rad
      cat(paste("Radius Around Capitals (if Capitals Check is chosen) :", currVar, "\n"))
      currVar <- input$cen_rad
      cat(paste("Radius Around Centroids (if Centroids Check is chosen) :", currVar, "\n"))
      currVar <- cen_detail_val()
      cat(paste("Centroid Specification (if Centroids Check is chosen) :", currVar, "\n"))
      currVar <- input$inst_rad
      cat(paste("Radius Around Institutions (if Institutions Check is chosen) :", currVar, "\n"))
      currVar <- out_type_val_file()
      cat(paste("Outlier Method (if Outlier Check is chosen) :", currVar, "\n"))
      currVar <- input$out_size
      cat(paste("Outlier Minimum Records (if Outlier Check is chosen) :", currVar, "\n"))
      currVar <- input$out_mtp
      cat(paste("Interquartile Range (if Outlier Mean Absolute Deviation Option is chosen) :", currVar, "\n"))
      currVar <- input$out_td
      cat(paste("Outlier Minimum Distance (if Min Distance Outlier Option is chosen) :", currVar, "\n"))
      currVar <-input$zero_rad
      cat(paste("Radius Around Point 0/0 (if Equal Lat/Long, Ploin Zero, and 0/0 Check is chosen) :", currVar, "\n"))
      currVar <- value_input_val()
      cat(paste("Output Type :", currVar, "\n"))
      sink()
      #file.show(clean_data_parameters_file) #just for debugging/checking accuracy
      }
    )
  
  output$download_dataset_params <- downloadHandler(
    filename = function(){"Dataset_Parameters.txt"},
    content = function(file){
      sink(file)
      cat(paste("Parameters used to create dataset", data_ID, "\n\n"))
      if (input$input_file_checkbox) {
        cat("this data was uploaded from a previously downloaded file, the parameters cannot be determined")
      }
      else if (input$down_key_checkbox){
        cat("this data was redownloaded, and the parameters of the origional search cannot be determined")
      }
      else{
        spName <- input$species_name
        cat(paste("Searched: ", spName,"as a",input$rank, "\n"))
        #filter by country
        if (input$political_checkbox){ 
        country <- input$political_boundary
        cat(paste("Country searched:", country, "\n"))
        }
        #filter by date
        if (input$date_checkbox){
        to_m <- substr(input$to_date, 6,7)
        from_m<- substr(input$from_date, 6,7)
        to_y <- substr(input$to_date, 1, 4)
        from_y <- substr(input$from_date, 1, 4)
        cat(paste("Date Constrainte:  From->", from_m, from_y, "To ->", to_m, to_y, "\n"))
      }
        #filter by lat/long
        if (input$lat_long_check_box){
          if (is.null(input$Lat_low ) ||  is.null(input$Lat_high) ||  is.null(input$Long_low) || is.null(input$Long_high)){
          }
          else{
            low_lat <- input$Lat_low
            high_lat <- input$Lat_high
            low_long <- input$Long_low
            high_long <- input$Long_high
            cat(paste("Longitudinal Constraints:", low_long, high_long, "\n"))
            cat(paste("Latitudinal Constraints:", low_lat, high_lat, "\n"))
          }
        }
      }
      sink()
      }
    )
  
  #currently saves it with the package files... not accessable to the user
  #How to make it accessable to user?
  #observeEvent( input$map_do,{
  #    map <- leaflet(gbif_data()) %>%
  #      addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
  #      
  #      addCircleMarkers(lng = ~ decimalLongitude, lat = ~ decimalLatitude,
  #                       radius = 3,
  #                       color = 'red',
  #                       stroke = FALSE, fillOpacity = 0.5
  #      )
  #      mapview::mapshot(map, file = "Occurrence_Map.png", selfcontained = FALSE )
  #  })

  output$download_clean_data <- downloadHandler(
    filename = function(){"CleanGbifDat.csv"},
    content = function(file){
      write.csv(clean_info() ,file)
    }
  )
  
  output$world_map <- renderLeaflet({
    leaflet(gbif_data()) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
      
      addCircleMarkers(lng = ~ decimalLongitude, lat = ~ decimalLatitude,
                       radius = 3,
                       color = 'red',
                       stroke = FALSE, fillOpacity = 0.5
      )
  })
}

shinyApp(ui, server)