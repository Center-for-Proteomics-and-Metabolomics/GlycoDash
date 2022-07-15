# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package( "shinydashboard" )
usethis::use_package( "shinyjs" )
usethis::use_package( "stringr" )
usethis::use_package( "purrr" )
usethis::use_package( "plater" )
usethis::use_package( "tidyr" )
usethis::use_package( "dplyr" )
usethis::use_package( "snakecase" )
usethis::use_package( "shinyFeedback" )
usethis::use_package( "tools" )
usethis::use_package( "rlang" )
usethis::use_package( "tidyselect" )
usethis::use_package( "shinyalert" )
usethis::use_package( "glue" )
usethis::use_package( "writexl" )
usethis::use_package( "dashboardthemes" )
usethis::use_package( "ggplot2" )
usethis::use_package( "shinyWidgets" )
usethis::use_package( "bsplus" )
usethis::use_package( "kableExtra" )
usethis::use_package( "readxl" )
usethis::use_package( "RColorBrewer")
usethis::use_package( "htmltools" )
usethis::use_pipe()

## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "data_import" ) 
golem::add_module( name = "fileInput_with_info" )
golem::add_module( name = "read_lacytools" )
golem::add_module( name = "add_sample_ids" )
golem::add_module( name = "process_sample_list" )
golem::add_module( name = "add_sample_types" )
golem::add_module( name = "process_sample_type_file" )
golem::add_module( name = "add_metadata" )
golem::add_module( name = "clusters" )
golem::add_module( name = "spectra_curation" ) 
golem::add_module( name = "tab_cut_offs" )
golem::add_module( name = "analyte_curation" ) 
golem::add_module( name = "normalization" ) 
golem::add_module( name = "derived_traits" ) 
golem::add_module( name = "repeatability" ) 
golem::add_module( name = "data_exploration" ) 
golem::add_module( name = "information_box" )
golem::add_module( name = "tab_repeatability" )
golem::add_module( name = "export" )

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct( "data_import" ) 
golem::add_fct( "add_sample_ids" )
golem::add_fct( "add_sample_types" )
golem::add_fct( "add_metadata" )
golem::add_fct( "clusters" )
golem::add_fct( "spectra_curation" )
golem::add_fct( "analyte_curation" )
golem::add_fct( "normalization" )
golem::add_fct( "derived_traits" )
golem::add_fct( "repeatability" )
golem::add_utils( "general" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "LacyTools_summary", open = TRUE )
usethis::use_data_raw(name = "metadata_cleaned", open = TRUE)
usethis::use_data_raw(name = "example_data", open = TRUE)

## Tests ----
## Add one line by test you want to create
usethis::use_test( "spectra_curation" )
usethis::use_test( "data_import" )
usethis::use_test( "analyte_curation" )
usethis::use_test( "normalization" )

# Documentation

## Vignette ----
usethis::use_vignette("glycodash_data_import")
usethis::use_vignette("glycodash_spectra_curation")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
## 
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action() 
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release() 
usethis::use_github_action_check_standard() 
usethis::use_github_action_check_full() 
# Add action for PR
usethis::use_github_action_pr_commands()

# Travis CI
usethis::use_travis() 
usethis::use_travis_badge() 

# AppVeyor 
usethis::use_appveyor() 
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

