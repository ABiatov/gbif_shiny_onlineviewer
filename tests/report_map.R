library(sf)
library(tmap)
library(leaflet)


# require(ggplot2)
# library(ggspatial)
# 
# require(ggmap)
# require(basemaps)

# library(maptiles)
# 
# library(basemapR)

# load("~/GitHub/gbif_shiny_onlineviewer/gbif_app_v2/gbif_data/gbif_sf_dataset.Rdata")

path_datadump_fgb <- "~/GitHub/gbif_shiny_onlineviewer/gbif_app_v2/gbif_data/gbif_sf_dataset.fgb"


aoi <- st_zm(st_read("~/GitHub/gbif_shiny_onlineviewer/tests/Simple.kml")) 

aoi_geom <- aoi %>% st_geometry() %>% st_union()

clipped_data <- read_sf(path_datadump_fgb, wkt_filter = aoi_geom %>% st_as_text() )
        
kingdom_colors <- c("Plantae" = "#4daf4a", "Fungi" = "#377eb8", "Animalia" = "#ff7f00")
kingdoms <- c("Plantae", "Fungi", "Animalia")

tmap_mode("plot")
# tmap_mode("view")

# tm = 
tm_basemap(leaflet::providers$Esri.WorldTopoMap)+
  tm_layout(
    legend.text.size = 1,
    legend.outside = TRUE, 
    legend.frame = FALSE 
    ) +
  tm_graticules() +
  tm_shape(aoi_geom) + tm_borders(lwd = 3, col = 'blue') +
  tm_shape(clipped_data) + tm_dots(
    size=0.5, 
    col="kingdom", 
    palette = kingdom_colors
    ) # + 
  # tm_add_legend(
  #               labels = "Area",
  #               # type = "polygons",
  #               type = "symbol",
  #               pwd = 2,
  #               col = "red",
  #               # lwd = 3 #,
  #               # fill = FALSE 
  #               ) # + 
  # tm_add_legend(
  #   labels = "Buffered area",
  #   # type = "polygons",
  #   type = "symbol",
  #   col = "blue",
  #   lwd = 3,
  #   # fill = FALSE
  #   ) 

tm


tmap_save(tm, "my_tm_map.png", width = 1000, height = 650, dpi = 96)



current.mode <- tmap_mode("view")

tmap_mode("plot")

data(World, metro)

tm_basemap(leaflet::providers$Stamen.Watercolor) +
  tm_tiles(paste0("https://services.arcgisonline.com/ArcGIS/rest/services/",
                  "World_Topo_Map/MapServer/tile/{z}/{y}/{x}"), group = "ESRI World Topo") +
  tm_shape(metro, bbox = "India") + tm_dots(col = "red", group = "Metropolitan areas") 
  


# Use tmap options to set the basemap and overlay map permanently during the R session:
opts <- tmap_options(basemaps = c(Canvas = "Esri.WorldGrayCanvas", Imagery = "Esri.WorldImagery"),
                     overlays = c(Labels = paste0("http://services.arcgisonline.com/arcgis/rest/services/Canvas/",
                                                  "World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}")))

tm_shape(World) + tm_borders(col = "red") 

qtm(World, fill = "HPI", fill.palette = "RdYlGn")

# restore options
tmap_options(opts)

# restore current mode
tmap_mode(current.mode)

