# Source: https://github.com/mcglinnlab/soar

library(rgbif)

# Import GBIF credentials
source("scripts/gbif_ini.R")

# Get Taxon Key by Species name
sp_key = name_suggest(q = 'Caretta caretta', rank = 'species')$data$key[1]

# preforms the query
res = occ_download(paste("taxonKey =", sp_key), 'hasCoordinate = TRUE',
             user = gbif_user, pwd = gbif_pwd, 
             email = gbif_email)

# checks on if complete
occ_download_meta(res)

# downloads and imports
dat <- occ_download_get(res[1], overwrite = TRUE) %>%
       occ_download_import()

class(dat)

