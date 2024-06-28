# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#

# #to re-load package
# devtools::load_all()
#
# # to open current app
# run_app()

golem::run_dev()

# bump version
usethis::use_version(which = 'patch')
usethis::use_version(which = 'dev')

# Save some data as demo data:
# This is created by running python cleaning within this app and saving .rds

#demo_data_1 <- readRDS("./data/data_cleaned_out_20230315_150121.rds")
#usethis::use_data(demo_data_1)


# RENV note from linux (speed)
# Package 'RcppTOML' wouldn't install, but running this fixed it:
# required for reticulate package
# withr::with_makevars( new = c(CXX17 = "g++", CXX17FLAGS = "-g -O2 $(LTO)",  CXX17PICFLAGS = "-fpic", CXX17STD = "-std=gnu++17"),  code = { install.packages("RcppTOML") })




###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.package('attachment') # if needed.
attachment::att_amend_desc()
# search for packages with ::
attachment::att_from_rscripts()
## Add modules ----
## Create a module infrastructure in R/
#golem::add_module(name = "name_of_module1", fct = "function_name", utils = "utility functions name", with_test = TRUE) # Name of the module

golem::add_module(name = "initial_summary", fct = "", with_test = TRUE)

golem::add_module(name = "by_bin", utils = "bin", with_test = TRUE)


golem::add_module(name = "by_animal", fct = "animal", with_test = TRUE)

golem::add_module(name = "files_in",  with_test = TRUE)

golem::add_module(name = 'final_summary', fct = "")

golem::add_module(name = 'welcome', fct = "")

golem::add_module(name = 'by_bin_clean', fct = "")

golem::add_module(name = 'uploads')

golem::add_module(name = 'by_animal_clean')

golem::add_module(name = 'by_animal_vis')


## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("helpers", with_test = TRUE)
golem::add_utils("helpers", with_test = TRUE)

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")
golem::add_sass_file("custom")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "my_dataset", open = FALSE)

## Tests ----
## Add one line by test you want to create
usethis::use_test("app")

# Documentation

## Vignette ----
usethis::use_vignette("IntakeInspectR")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
# covrpage::covrpage()

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



purrr::walk(list("DT", "MASS", "broom", "bslib", "dplyr", "dtplyr", "forcats",
         "ggiraph", "glue", "logr", "lubridate", "plotly", "purrr",
         "shinybusy", "shinycssloaders", "shinyjs", "stringr",
         "tibble", "tidyr", "tidyselect"), ~usethis::use_package(.x))

usethis::use_package('shinyTime')
