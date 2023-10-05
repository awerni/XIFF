FROM rocker/r-base:4.1.0

LABEL maintainer="Andreas Wernitznig"
LABEL email="awernitz@broadinstitute.org"

# system libraries of general use
RUN apt-get update && apt-get install -y \
libssl-dev \
libpq-dev \
libxml2-dev \
libcurl4-gnutls-dev \
libfontconfig1-dev \
libcairo2-dev \
&& rm -rf /var/lib/apt/lists/*

RUN install2.r -n -1 littler remotes BiocManager \
 shiny magrittr dplyr htmltools tidyr rlang \
 ggplot2 pheatmap tibble shinyjs ggrepel DT glue shinyWidgets \
 jsonlite Rtsne umap fpc purrr RPostgres forcats shinyBS readxl readr \
 scales tidyselect plotROC apcluster xfun Boruta neuralnet NeuralNetTools \
 systemfonts glmnet apcluster phateR Rcpp RcppArmadillo gridExtra svglite \
 stringi plyr promises future testthat formatR curl RCurl matrixStats \
 lambda.r futile.options RSQLite httr futile.logger BH snow bitops caret \
 logger highr showimage debugme tinytex knitr webdriver pingr parsedate \
 assertthat rmarkdown shinytest2 shades backports ggfittext config broom matrixTests \
 gggenes shinythemes \
 && rm -rf /tmp/downloaded_packages/* \
 && rm -rf /tmp/*.rds

RUN installGithub.r Roche/ggtips Boehringer-Ingelheim/FutureManager \
 && rm -rf /tmp/downloaded_packages/* \
 && rm -rf /tmp/*.rds
RUN installBioc.r DESeq2 limma GO.db graph edgeR gage \
 && rm -rf /tmp/downloaded_packages/* \
 && rm -rf /tmp/*.rds

RUN R -e 'webdriver::install_phantomjs()'
RUN apt-get update && \
 apt-get install -y python3-pip libpython3-dev && \
 python3 -m pip install --no-cache-dir phate && \
 apt-get remove -y python3-pip && \
 apt autoremove -y

RUN mkdir /install
COPY DESCRIPTION /install/DESCRIPTION
RUN R -e 'remotes::install_deps("/install", upgrade="never", repos=BiocManager::repositories(), dependencies=TRUE)'
RUN touch /tmp/openssl.cnf # workaround for phantomJS issue



COPY . /install
RUN R CMD INSTALL /install && \
 export OPENSSL_CONF="/tmp/openssl.cnf" && \
 #R -e 'testthat::test_local("/install", stop_on_failure=TRUE)' && \
 rm -rf /install

CMD ["R"]
