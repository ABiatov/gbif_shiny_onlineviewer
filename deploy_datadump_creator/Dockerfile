
FROM fedora:38

ENV FUNCTION_DIR="/app" 
ENV APP_DIR="${FUNCTION_DIR}/scripts" \
    INPUT_DATA_DIR="${FUNCTION_DIR}/data" \
    OUTPUT_DATA_DIR="${FUNCTION_DIR}/outputs" \
    TEMP_DATA_DIR="${FUNCTION_DIR}/temp" 

LABEL org.opencontainers.image.title="GBIF Viewer. Module: Data Dump Creator" \
      org.opencontainers.image.version="1.0" \
      org.opencontainers.image.created="2023-11-10" \
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

RUN R -e "install.packages(pkgs=c('tidyverse', 'sf', 'rgbif'), repos='https://cran.rstudio.com/')" 

WORKDIR ${FUNCTION_DIR}

COPY . .

# COPY temp/matches.Rdata ${TEMP_DATA_DIR} 
# COPY data/shp ${INPUT_DATA_DIR}
# COPY scripts/2_get_gbif_data.R ${APP_DIR}/2_get_gbif_data.R
# COPY scripts/3_filtering_cleaning_attribution.R ${APP_DIR}/3_filtering_cleaning_attribution.R
# COPY scripts/config.R ${APP_DIR}/config.R
# COPY scripts/gbif_ini.R ${APP_DIR}/gbif_ini.R
# COPY scripts/execute.sh ${APP_DIR}/execute.sh

CMD ["bash"]

# CMD ["sh", "-c", "${APP_DIR}/execute.sh"]
