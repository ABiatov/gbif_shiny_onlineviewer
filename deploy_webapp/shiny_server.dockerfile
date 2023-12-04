FROM rocker/shiny-verse:4.3.2

LABEL org.opencontainers.image.title="GBIF Viewer. Module: Web Viewer - Shiny Server" \
      org.opencontainers.image.version="1.0" \
      org.opencontainers.image.created="2023-11-15" \
      org.opencontainers.image.authors="Anton Biatov (GitHub @ABiatov), Oleh Prylutskyi (GitHub @olehprylutskyi), Yehor Yatsiuk " \
      org.opencontainers.image.vendor="Ukrainian Nature Conservation Group (UNCG Ukraine) & Netherlands Biodiversity Information Facility (NLBIF)" \
      org.opencontainers.image.licenses="Creative Commons Attribution 4.0 International (CC-BY-4.0)" \
      org.opencontainers.image.url="https://github.com/ABiatov/gbif_shiny_onlineviewer/deploy_datadump_creator/" 

RUN apt-get update -qq \
    # && apt-get upgrade -y  \
    && apt-get install -y --no-install-recommends \
      libxml2-dev \
      libudunits2-dev \
      libgdal-dev \
      libgeos-dev \
      libproj-dev \
    && apt-get clean

RUN locale-gen uk_UA.UTF-8 

ENV LANG=uk_UA.UTF-8 \
    LANGUAGE=uk_UA:uk \
    LC_ALL=uk_UA.UTF-8

RUN install2.r --error --skipinstalled \
    shinyWidgets shinyalert shinyjs sp \
    DT openxlsx openxlsx2 rgbif sf \
    leaflet leaflet.extras leafem leaflet.esri

RUN installGithub.r Chrisjb/basemapR

COPY app /srv/shiny-server

EXPOSE 3838
