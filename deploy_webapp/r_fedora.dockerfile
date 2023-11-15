FROM fedora:38

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

# install langpack
# RUN dnf install -y glibc-langpack-uk

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

CMD ["bash"]

# RUN R -e "install.packages(pkgs=c('tidyverse', 'sf', 'rgbif'), repos='https://cran.rstudio.com/')" 

