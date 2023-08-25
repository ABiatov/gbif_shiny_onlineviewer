
# Create a global reactive value ####
## Create a global reactive value for AOI polygon
reaktive_aoi_polygon <- reactiveVal()

## Create a global reactive value for guffered polygon
reaktive_bufered_polygon <- reactiveVal() # Create a global reactive value for buffered polygon - now SF, not WKT!

# reaktive_dissolved_polygon <- reactiveVal()  # Create second reactive value for dissolved polygon - also SF


df_rare_lists <- reactiveVal() 


# Create reactiveVal by list ####

# List of variable names
# ChKU, IUCN, Bern Appendix 1,  Bern Appendix 2, Bern Appendix 3, Bern Resolution 6
# Bonn, AEWA, EUROBATS, ACCOBAMS
# Birds Directive Annex I, Birds Directive Annex IІ, 
# Habitats Directive Annex II, Habitats Directive Annex IV, Habitats Directive Annex V 

var_names <- c("report" ,"chku", "iucn", "BernApp_1", "BernApp_2", "BernApp_3", "BernRes_6",
               "Bonn", "AEWA", "EUROBATS", "ACCOBAMS",
               "BirdDirAnn_I", "BirdDirAnn_II",
               "HabitatsDirAnn_II", "HabitatsDirAnn_IV", "HabitatsDirAnn_V",
               "Invasive"
               )

# Loop over the list of variable names
for (name in var_names) {
  # Create the reactive variable for `tab_filtred`
  assign(paste0("tab_filtred_", name), reactiveVal())
  
  # Create the reactive variable for `nrow`
  assign(paste0("nrow_", name), reactiveVal())
}


# # Loop over the list of variable names
# for (name in var_names) {
#   # Create the reactive variable for `tab_filtred`
#   print(paste0("tab_filtred_", name))
#   
#   # Create the reactive variable for `nrow`
#   print(paste0("nrow_", name))
# }





