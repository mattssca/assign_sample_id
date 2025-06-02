FROM rocker/shiny:latest

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libicu-dev \
    libfontconfig1-dev \
    libcairo2-dev \
    libxt-dev \
    libgl1-mesa-dev \
    libpng-dev \
    libjpeg-dev \
    libtiff5-dev \
    libfreetype6-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libgomp1

RUN R -e "install.packages('pkgdown', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('devtools', 'shinyjs', 'bslib', 'shinyWidgets', 'DT'), repos='https://cloud.r-project.org/'); devtools::install_github('mattssca/assign_sample_id')"

COPY . /srv/shiny-server/

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]