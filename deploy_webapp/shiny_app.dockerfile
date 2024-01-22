FROM fedora:38

LABEL org.opencontainers.image.title="Biodiversity Viewer. Module: Web Viewer - Shiny App" \
      org.opencontainers.image.version="1.0" \
      org.opencontainers.image.created="2023-12-13" \
      org.opencontainers.image.authors="Anton Biatov (GitHub @ABiatov), Oleh Prylutskyi (GitHub @olehprylutskyi), Yehor Yatsiuk " \
      org.opencontainers.image.vendor="Ukrainian Nature Conservation Group (UNCG Ukraine) & Netherlands Biodiversity Information Facility (NLBIF)" \
      org.opencontainers.image.licenses="Creative Commons Attribution 4.0 International (CC-BY-4.0)" \
      org.opencontainers.image.url="https://github.com/ABiatov/gbif_shiny_onlineviewer/tree/main/deploy_webapp" 

# install R
RUN echo "install_weak_deps=False" >> /etc/dnf/dnf.conf \
    && dnf -y install R \
    && dnf -y clean all

# enable copr
RUN dnf -y install 'dnf-command(copr)' \
    && dnf -y copr enable iucar/cran \
    && dnf -y install R-CoprManager \
    && dnf clean dbcache  \
    && dnf clean all  \
    && rm -rf /var/cache/yum \
    && echo "options(repos='https://cloud.r-project.org')" > \
        /usr/lib64/R/etc/Rprofile.site.d/00-repos.site

# install littler scripts
RUN dnf -y install R-CRAN-{littler,remotes} \
    && dnf -y clean all \
    && ln -s /usr/local/lib/R/library/littler/examples/install.r \
        /usr/local/bin/install.r \
    && ln -s /usr/local/lib/R/library/littler/examples/install2.r \
        /usr/local/bin/install2.r \
    && ln -s /usr/local/lib/R/library/littler/examples/installGithub.r \
        /usr/local/bin/installGithub.r \
    && ln -s /usr/local/lib/R/library/littler/examples/testInstalled.r \
        /usr/local/bin/testInstalled.r

RUN dnf install -y glibc-langpack-uk \
    && dnf -y clean all

ENV LANG=uk_UA.UTF-8 \
    LANGUAGE=uk_UA:uk \
    LC_ALL=uk_UA.UTF-8

RUN R -e "install.packages(pkgs=c('tidyverse', 'shiny', \
    'shinyWidgets', 'shinyalert', 'shinyjs', 'sp', 'DT', \
    'data.table', 'openxlsx', 'openxlsx2', 'sf',  \
    'rmarkdown', 'tinytex' , 'knitr', 'leaflet', \
    'leaflet.extras', 'leafem', 'leaflet.esri', 'rgbif'),  \
    repos='https://cran.rstudio.com/')" 

# RUN install2.r --error --skipinstalled \
#     tidyverse shiny shinyWidgets \
#     shinyalert shinyjs sp DT \
#      ggplot2 openxlsx openxlsx2 sf \
#     leaflet leaflet.extras leafem rgbif

RUN R --quiet -e 'remotes::install_github("Chrisjb/basemapR")'
# RUN installGithub.r Chrisjb/basemapR

# RUN addgroup --system app && adduser --system --ingroup app app

# WORKDIR /home/app

COPY app /home/app

# RUN chown app:app -R /home/app

# USER app

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp(appDir='/home/app', host='0.0.0.0', port=3838)"]
