# polygon_bufferisation

# polygon_bufferisation(input_polygon, radius)

polygon_bufferisation <- function(sf_input_polygon, radius){
  sf_polygon_buffered <- st_transform(sf_input_polygon, CRS_used_in_calculations) %>%
    st_buffer(dist = as.numeric(radius), nQuadSegs = 4) %>%
    st_transform(4326) %>% 
    st_union()
  
  return(sf_polygon_buffered)
}

