#!/usr/bin/sh

Rscript ${APP_DIR}/2_get_gbif_data.R
Rscript ${APP_DIR}/3_filtering_cleaning_attribution.R

