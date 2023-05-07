library(leaflet)

# curent_providers <- providers
# print(curent_providers)


x <- c(36.25, 36.36, 36.53)
y <- c(49.555, 49.69, 49.735)
name <- c("Gomilsha", "Zmiiv", "Mokhnach")
villages <- data.frame(x, y, name)
villages$population <- c(466, 13737, 231 )
  
map <- leaflet(
  options = leafletOptions(
    # dragging = FALSE, # ограничивает перемещение
    minZoom = 5,  # ограничивает zoom
    maxZoom = 14
    )
) %>%
  # addTiles() %>%
  # addProviderTiles("Esri.WorldImagery") %>%
  addProviderTiles("OpenTopoMap") %>%
  # setView(lng = 36.39, lat = 49.65, zoom = 10) %>% # set view by center point and zoom
  # fitBounds(lng1 = 36.1, lat1 = 50.5,  # set view by extent: p1 - top lext, p2 - bottom right 
  #           lng2 = 36.8, lat2 = 49.3) %>%
  setMaxBounds(lng1 = 35.5, lat1 = 50.5,   # максимально возможное перемещение карты
               lng2 = 37.1, lat2 = 49.1) %>%
  addMarkers(  # single marker
  # addPopups(  # create popup without marker
      lng = 36.3, lat = 49.58, popup = "NPP Gomilsha forest") %>% 
  # addMarkers(lng = c(36.25, 36.36, 36.53), # few markers as vector
  #            lat = c(49.555, 49.69, 49.735), 
  #            popup = c("Gomilsha", "Zmiiv", "Mokhnach"))   
  addCircleMarkers(data = villages, lng = ~x, lat = ~y, # add circle markers from dataframe
                   # radius = 10,  # static radius
                   radius = log10( villages$population ) ** 2,  # calculation radius
                   color = "red",
                   # popup = ~name # simple popup from field "name"
                   popup = ~paste0("<center>" ,"<b>", name, "</b>", "</center>", "<br>",   # popup with HTML 
                                   "Население: ", population, " чел." )
                   )

map

print("Map str: ")
print(str(getMapData(map)))

print("Map data: ")
print(getMapData(map))

