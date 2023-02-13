FROM yumyai/sysmiome-dash_base:0.0.0-dev

RUN rm -rf /srv/shiny-server/*
WORKDIR /srv/shiny-server/
RUN mkdir /srv/shiny-server/app
COPY app.R ./app
COPY R ./app/R
COPY docker/internal ./app/internal

# Redundant, but serve as a reminder
USER shiny
ENV SHINY_LOG_STDERR=1
EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
