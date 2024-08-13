FROM rocker/verse:4.3.1

# Define date for CRAN snapshot (to ensure reproducible R package versions):
ARG WHEN='2023-09-15'

# Install packages (tidyverse packages are already in the container)
RUN R -e "options(repos = \
   list(CRAN = 'https://packagemanager.rstudio.com/cran/${WHEN}/')); \
   install.packages(c('data.table',\
                      'rmarkdown', \
                      'knitr', \
                      'mgcv', \
                      'gridExtra', \
                      'huxtable', \
                      'Epi', \
                      'popEpi', \
                      'gt', \
                      'gtsummary', \
                      'fst'))" 

# Create project directory within container:
WORKDIR /lastmeds