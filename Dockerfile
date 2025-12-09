FROM rocker/verse:4.5.0
RUN apt-get update && apt-get install -y   && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site

# Copy renv files first for better layer caching
RUN mkdir /build_zone
WORKDIR /build_zone
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# Install renv and restore packages
RUN R -e 'install.packages("renv")'
RUN R -e 'renv::restore()'

# Copy the rest of the application
COPY . .

# Install the local package
RUN R -e 'renv::install(".", repos = getOption("repos"))'

EXPOSE 80
CMD ["R", "-e", "options('shiny.port'=80,shiny.host='0.0.0.0');library(GlycoDash);GlycoDash::run_app()"]
