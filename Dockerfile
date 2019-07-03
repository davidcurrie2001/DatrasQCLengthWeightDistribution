FROM rocker/shiny:3.4.1
MAINTAINER Marine Institute
# install required Linux packages
RUN sudo apt-get update && apt-get install -y libcurl4-openssl-dev libssl-dev libxml2-dev libudunits2-0 libudunits2-dev libproj-dev libgdal-dev && apt-get clean && rm -rf /var/lib/apt/lists/ && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
# install required R packages
RUN Rscript -e "install.packages(c('plotly'), repos='https://cran.rstudio.com/')"
RUN Rscript -e "install.packages(c('tidyverse'), repos='https://cran.rstudio.com/')"
RUN Rscript -e "install.packages(c('shiny'), repos='https://cran.rstudio.com/')"
COPY server.R /srv/shiny-server/
COPY global.R /srv/shiny-server/
COPY ui.R /srv/shiny-server/
CMD ["/usr/bin/shiny-server.sh"]
# docker build -t mi/datrasqc:test .
# docker run --name datras -d -v $PWD/data:/srv/shiny-server/data -p 3845:3838 mi/datrasqc:test