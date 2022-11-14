FROM rocker/r-ver:4.2.2

## Install packages
RUN apt-get update \ 
  && apt-get install -y --no-install-recommends \ 
    libgdal-dev \ 
    libproj-dev \ 
    libgeos-dev \ 
    lbzip2 \ 
    libnetcdf-dev \ 
    libsqlite3-dev \ 
    libssl-dev \ 
    libudunits2-dev \ 
    netcdf-bin \ 
    sqlite3 \ 
    imagemagick \ 
    pandoc \ 
    pandoc-citeproc \ 
    make \ 
    ghostscript \ 
    poppler-utils \ 
    zip \ 
    wget \ 
    fonts-dejavu-extra \ 
    curl \ 
    tk \ 
    xtide \ 
    openjdk-11-jre \ 
  && rm -rf /var/lib/apt/lists/* 

## Install R package versions from MRAN (based on a date - YYYY-MM-DD)
RUN R -e "options(repos = \ 
    list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2022-10-04/'));\ 
    install.packages('tidyverse'); \ 
    install.packages('sf'); \ 
    install.packages('sp'); \ 
    install.packages('foreach'); \ 
    install.packages('testthat'); \ 
    install.packages('tidybayes'); \ 
    install.packages('jsonlite'); \ 
    install.packages('rlang'); \ 
    install.packages('bookdown'); \ 
    install.packages('rnaturalearth'); \ 
    install.packages('rnaturalearthdata'); \ 
    install.packages('patchwork'); \ 
    install.packages('ggnewscale'); \ 
    install.packages('tidybayes'); \ 
    install.packages('rgeos'); \ 
    install.packages('sn'); \ 
    install.packages('tidybayes'); \ 
    install.packages('inlabru'); \ 
    install.packages('cli'); \ 
    install.packages('Hmisc'); \ 
    install.packages('ncmeta'); \ 
    install.packages('stars'); \ 
    install.packages('geojsonR'); \ 
    install.packages('geojsonsf'); \ 
    install.packages('s2'); \ 
    install.packages('R.utils'); \ 
"

RUN R -e "options(repos = \ 
    list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2022-10-04/'));\ 
    install.packages('gratia'); \     
    install.packages('gbm'); \ 
"

## ## Install INLA
## RUN  wget https://inla.r-inla-download.org/R/stable/src/contrib/INLA_21.02.23.tar.gz \ 
##   && R CMD INSTALL --clean --no-multiarch --without-keep.source --byte-compile --resave-data --compact-docs --no-demo INLA_21.02.23.tar.gz \ 
##   && rm INLA_21.02.23.tar.gz 

## Create project directory in docker image 
RUN mkdir /home/Project

## Copy scripts and parameters (folders and contents) into docker image project directory
COPY scripts/ /home/Project/scripts/ 
COPY docs/ /home/Project/docs/ 
WORKDIR /home/Project/ 

## ENTRYPOINT ["make", "-i","all"]
