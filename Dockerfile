FROM rocker/shiny-verse:4.1.0

RUN apt-get update -qq \
  && apt-get install -y \
    --no-install-recommends \
    git-core \
    libssl-dev \
    libcurl4-gnutls-dev \
#    curl \
#    libsodium-dev \
    libxml2-dev \
#    libicu-dev \
# Devtools, but probalbly use in other as well
    libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev libmagick++-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Plotly?
#   libudunits2-dev
#   librsvg2-dev

ENV _R_SHLIB_STRIP_=true
# COPY Rprofile.site /etc/R
RUN install2.r --error --skipinstalled --deps TRUE --repos https://cran.asia/  \
  devtools \
  cowplot \
  shinydashboard \
  shinyjs \
  data.table \
  DT
#     forecast \
#     jsonlite \
#     ggplot2 \
#     htmltools \
#     plotly

COPY ext_script/installBioc.R /usr/local/bin/
RUN chmod u+x /usr/local/bin/installBioc.R
RUN installBioc.R --error --skipinstalled --deps TRUE \
  phyloseq
# Install phyloseq I guess.

RUN rm -rf /srv/shiny-server/*
WORKDIR /srv/shiny-server/
RUN mkdir /srv/shiny-server/app
COPY app.R ./app
COPY internal ./app/internal

# Redundant, but serve as a reminder
USER shiny
ENV SHINY_LOG_STDERR=1
EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
