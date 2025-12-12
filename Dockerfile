FROM rocker/verse:4.5.0

# Install any additional system dependencies and clean up
RUN apt-get update && apt-get install -y \
    && rm -rf /var/lib/apt/lists/* \
    && rm -rf /tmp/*

# Configure R options
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/ \
    && echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site

# Set up build directory
RUN mkdir /build_zone
WORKDIR /build_zone

# Copy renv files first for better layer caching
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# Install renv and restore packages
RUN R -e 'install.packages("renv")' \
    && R -e 'renv::restore()' \
    && rm -rf /tmp/downloaded_packages/* \
    && rm -rf /tmp/Rtmp*

# Copy the rest of the application
COPY . .

# Install the local package and clean up build artifacts
RUN R -e 'renv::install(".", repos = getOption("repos"))' \
    && rm -rf /tmp/downloaded_packages/* \
    && rm -rf /tmp/Rtmp* \
    && rm -rf /build_zone/vignettes \
    && rm -rf /build_zone/tests \
    && rm -rf /build_zone/man \
    && rm -rf /build_zone/.git \
    && find /build_zone -name "*.md" -type f -delete \
    && find /build_zone -name "*.Rmd" -type f -delete

EXPOSE 80
CMD ["R", "-e", "options('shiny.port'=80,shiny.host='0.0.0.0');library(GlycoDash);GlycoDash::run_app()"]
