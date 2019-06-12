FROM rocker/shiny:3.4.1
MAINTAINER Marine Institute
# install ssl
# and gdal
RUN sudo apt-get update && apt-get install -y libcurl4-openssl-dev libssl-dev libxml2-dev libudunits2-0 libudunits2-dev libproj-dev libgdal-dev && apt-get clean && rm -rf /var/lib/apt/lists/ && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
# install required packages
RUN Rscript -e "install.packages(c('plotly'), repos='https://cran.rstudio.com/')"
RUN Rscript -e "install.packages(c('tidyverse'), repos='https://cran.rstudio.com/')"
COPY server.R /srv/shiny-server/
COPY global.R /srv/shiny-server/
COPY ui.R /srv/shiny-server/
RUN mkdir /srv/shiny-server/data
#COPY *.rds /srv/shiny-server/
CMD ["/usr/bin/shiny-server.sh"]
# docker build -t mi/datrasqc:test .
# docker run --name datrasqc -d -p 3840:3838 mi/datrasqc:test