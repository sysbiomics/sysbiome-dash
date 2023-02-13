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
    libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev libmagick++-dev libglpk-dev \
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
  DT \
  matrixStats  # For DESEQs2 -> phyloseq
#     forecast \
#     jsonlite \
#     ggplot2 \
#     htmltools \
#     plotly

# phyloseqs need DESeqs which also need the following: igraph (glpk?), libmagick (?), matrixStats

COPY docker/ext_script/installBioc.R /usr/local/bin/
RUN chmod u+x /usr/local/bin/installBioc.R
RUN installBioc.R --error --skipinstalled --deps TRUE \
  phyloseq \
  microbiome
