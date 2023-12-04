
FROM fedora:38

ENV FUNCTION_DIR="/app" 
ENV APP_DIR="${FUNCTION_DIR}/scripts" \
    INPUT_DATA_DIR="${FUNCTION_DIR}/data" \
    GBIF_DATA_DIR="${FUNCTION_DIR}/gbif_data" \
    TEMP_1_DATA_DIR="${FUNCTION_DIR}/temp_1" \
    TEMP_2_DATA_DIR="${FUNCTION_DIR}/temp_2" 

LABEL org.opencontainers.image.title="Biodiversity Viewer. Module: Data Dump Creator" \
      org.opencontainers.image.version="1.0" \
      org.opencontainers.image.created="2023-11-26" \
      org.opencontainers.image.authors="Anton Biatov (GitHub @ABiatov), Oleh Prylutskyi (GitHub @olehprylutskyi), Yehor Yatsiuk " \
      org.opencontainers.image.vendor="Ukrainian Nature Conservation Group (UNCG Ukraine) & Netherlands Biodiversity Information Facility (NLBIF)" \
      org.opencontainers.image.licenses="Creative Commons Attribution 4.0 International (CC-BY-4.0)" \
      org.opencontainers.image.url="https://github.com/ABiatov/gbif_shiny_onlineviewer/deploy_datadump_creator/" 

RUN dnf install -y R && \
    dnf install -y 'dnf-command(copr)' && \
    dnf copr -y enable iucar/cran &&  \
    dnf install -y R-CoprManager && \
    dnf install -y glibc-langpack-uk && \
    dnf clean dbcache && \
    dnf clean all && \
    rm -rf /var/cache/yum 

ENV LANG=uk_UA.UTF-8 \
    LANGUAGE=uk_UA:en \
    LC_ALL=uk_UA.UTF-8

RUN R -e "install.packages(pkgs=c('tidyverse', 'sf', 'rgbif', 'DataEditR'), repos='https://cran.rstudio.com/')" 

WORKDIR ${FUNCTION_DIR}

COPY . .

# RUN chmod +x ${APP_DIR}/execute.sh

# CMD ["sh", "-c", "${APP_DIR}/execute.sh"]

# CMD Rscript ${APP_DIR}/2_get_gbif_data.R && Rscript ${APP_DIR}/3_filtering_cleaning_attribution.R
