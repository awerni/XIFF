FROM rocker/r-base

LABEL maintainer="Andreas Wernitznig"
LABEL email="awernitz@broadinstitute.org"

# system libraries of general use
RUN apt-get update && apt-get install -y \
libssl-dev \
libpq-dev \
libxml2-dev \
libcurl4-gnutls-dev \
libfontconfig1-dev \
libcairo2-dev

RUN R -e 'install.packages(c("remotes", "BiocManager"))'
RUN R -e 'remotes::install_github("Roche/ggtips")'
RUN R -e 'remotes::install_github("Boehringer-Ingelheim/FutureManager")'

RUN mkdir /install
COPY DESCRIPTION /install/DESCRIPTION
RUN R -e 'remotes::install_deps("/install", upgrade="never", repos=BiocManager::repositories(), dependencies=TRUE)'
RUN touch /tmp/openssl.cnf # workaround for phantomJS issue

COPY . /install
RUN R CMD INSTALL /install
RUN export OPENSSL_CONF="/tmp/openssl.cnf" && R -e 'testthat::test_local("/install", stop_on_failure=TRUE)'
RUN rm -rf /install

CMD ["R"]
