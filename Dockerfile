FROM rocker/verse:4.3.3
RUN apt-get update && apt-get install -y   && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("magrittr",upgrade="never", version = "2.0.3")'
RUN Rscript -e 'remotes::install_version("tibble",upgrade="never", version = "3.2.1")'
RUN Rscript -e 'remotes::install_version("rlang",upgrade="never", version = "1.1.3")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.7.0")'
RUN Rscript -e 'remotes::install_version("processx",upgrade="never", version = "3.8.4")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.8.1")'
RUN Rscript -e 'remotes::install_version("tidyselect",upgrade="never", version = "1.2.1")'
RUN Rscript -e 'remotes::install_version("rmarkdown",upgrade="never", version = "2.27")'
RUN Rscript -e 'remotes::install_version("knitr",upgrade="never", version = "1.47")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.8.1.1")'
RUN Rscript -e 'remotes::install_version("readxl",upgrade="never", version = "1.4.3")'
RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.5.1")'
RUN Rscript -e 'remotes::install_version("purrr",upgrade="never", version = "1.0.2")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.1.4")'
RUN Rscript -e 'remotes::install_version("RColorBrewer",upgrade="never", version = "1.1-3")'
RUN Rscript -e 'remotes::install_version("scales",upgrade="never", version = "1.3.0")'
RUN Rscript -e 'remotes::install_version("shinydashboard",upgrade="never", version = "0.7.2")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.33")'
RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.3.1")'
RUN Rscript -e 'remotes::install_version("ggplot2",upgrade="never", version = "3.5.1")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.2")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.2.1.1")'
RUN Rscript -e 'remotes::install_version("spelling",upgrade="never", version = "2.3.0")'
RUN Rscript -e 'remotes::install_version("writexl",upgrade="never", version = "1.5.0")'
RUN Rscript -e 'remotes::install_version("snakecase",upgrade="never", version = "0.11.1")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.8.6")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("shinyjqui",upgrade="never", version = "0.4.1")'
RUN Rscript -e 'remotes::install_version("shinyFeedback",upgrade="never", version = "0.4.0")'
RUN Rscript -e 'remotes::install_version("shinydashboardPlus",upgrade="never", version = "2.0.4")'
RUN Rscript -e 'remotes::install_version("shinycssloaders",upgrade="never", version = "1.0.0")'
RUN Rscript -e 'remotes::install_version("shinybusy",upgrade="never", version = "0.3.3")'
RUN Rscript -e 'remotes::install_version("shinyBS",upgrade="never", version = "0.61.1")'
RUN Rscript -e 'remotes::install_version("shinyalert",upgrade="never", version = "3.1.0")'
RUN Rscript -e 'remotes::install_version("RLumShiny",upgrade="never", version = "0.2.3")'
RUN Rscript -e 'remotes::install_version("plotly",upgrade="never", version = "4.10.4")'
RUN Rscript -e 'remotes::install_version("plater",upgrade="never", version = "1.0.4")'
RUN Rscript -e 'remotes::install_version("kableExtra",upgrade="never", version = "1.4.0")'
RUN Rscript -e 'remotes::install_version("InteractiveComplexHeatmap",upgrade="never", version = "1.10.0")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.4.1")'
RUN Rscript -e 'remotes::install_version("bsplus",upgrade="never", version = "0.1.4")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');library(GlycoDash);GlycoDash::run_app()"
