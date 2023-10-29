# gbif_shiny_onlineviewer

## Folders

** [gbif_app_v1](https://github.com/ABiatov/gbif_shiny_onlineviewer/tree/main/gbif_app_v1) ** - The first version application. Get GBIF data by list of species. Geting data by occ_search()

** [gbif_app_v2](https://github.com/ABiatov/gbif_shiny_onlineviewer/tree/main/gbif_app_v2) ** - The second version application. Get GBIF data from data damp.

** [name_lookup](https://github.com/ABiatov/gbif_shiny_onlineviewer/tree/main/name_lookup) ** - The code for data damp creation.


** [container_webapp](https://github.com/ABiatov/gbif_shiny_onlineviewer/tree/main/container_webapp) ** - The docker conteiner for build web application wich work with data damp.

** [container_datadump](https://github.com/ABiatov/gbif_shiny_onlineviewer/tree/main/container_datadump) ** - The Docker container for generating data dump. 





# Installing 

``` 
library(devtools)
install_github('Chrisjb/basemapR')
```

# GBIF data dump preparation

## Read protected status

''' {r}
load("~/GitHub/gbif_shiny_onlineviewer/name_lookup/matches.Rdata")
'''

