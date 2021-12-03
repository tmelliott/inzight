FROM rocker/r-bspm:20.04

LABEL maintainer="Tom Elliott <tom.elliott@auckland.ac.nz>"

RUN install.r remotes plumber duckdb

ADD . /srv/inzight/
WORKDIR /srv/inzight
RUN Rscript -e 'remotes::install_local("/srv/inzight")'

EXPOSE 4567

CMD ["./inst/server/run.sh"]
