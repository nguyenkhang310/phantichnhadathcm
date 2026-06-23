FROM rocker/shiny:latest

ENV BDS_APP_HOST=0.0.0.0

RUN apt-get update && apt-get install -y --no-install-recommends \
    cmake \
    g++ \
    gfortran \
    git \
    libcairo2-dev \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libgdal-dev \
    libgeos-dev \
    libharfbuzz-dev \
    libjpeg-dev \
    libpng-dev \
    libproj-dev \
    libssl-dev \
    libtiff-dev \
    libxml2-dev \
    make \
    pandoc \
    && rm -rf /var/lib/apt/lists/*

RUN install2.r --error --skipinstalled --ncpus -1 \
    DBI \
    DT \
    RSQLite \
    chromote \
    dplyr \
    furrr \
    future \
    ggplot2 \
    htmltools \
    htmlwidgets \
    httr \
    jsonlite \
    leaflet \
    lubridate \
    plotly \
    purrr \
    randomForest \
    readr \
    rvest \
    scales \
    shiny \
    stringr \
    tibble \
    xml2 \
    xgboost

WORKDIR /srv/app

COPY . .

RUN Rscript -e 'model_files <- c("models/mo_hinh_gia_ban.rds", "models/mo_hinh_gia_thue.rds"); missing <- model_files[!file.exists(model_files)]; if (length(missing)) stop("Thieu model: ", paste(missing, collapse = ", ")); small <- model_files[file.info(model_files)$size < 1000000]; if (length(small)) stop("Model qua nho, co the Git LFS chua keo file that: ", paste(small, collapse = ", "))'

EXPOSE 3838

CMD ["Rscript", "app.R"]
