# polygon_bufferisation

# polygon_bufferisation(input_polygon, radius)

polygon_bufferisation <- function(sf_input_polygon, radius){
  sf_polygon_buffered <- st_transform(sf_input_polygon, CRS_used_in_calculations) %>%
    st_buffer(dist = as.numeric(radius), nQuadSegs = 4) %>%
    st_transform(4326) %>% 
    st_union()
  
  return(sf_polygon_buffered)
}

# create sf polygon from drowed on leaflet

leaf_draw_sf_polyg <- function(geometry_coordinates){
  coords <- geometry_coordinates %>% unlist %>% matrix(nc = 2, byrow = T)
  curent_polygon <- sp::Polygon(coords) %>% list %>% sp::Polygons(ID=1) %>% list %>% sp::SpatialPolygons()
  sf_curent_polygon <- st_as_sfc(curent_polygon) %>% st_set_crs(4326) # it work
  
  return(sf_curent_polygon)
}

