# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## Fill the DESCRIPTION ----
## Add meta data about your application
##
## /!\ Note: if you want to change the name of your app during development,
## either re-run this function, call golem::set_golem_name(), or don't forget
## to change the name in the app_sys() function in app_config.R /!\
##
golem::fill_desc(
  pkg_name = "IntakeInspectR", # The Name of the package containing the App
  pkg_title = "Data cleaning and visualisation for automatically collected feed intake data", # The Title of the package containing the App
  pkg_description = "A set of tools to help researchers clean their raw feed intake data by checking for common errors and visualise their data.", # The Description of the package containing the App
  author_first_name = "Dave", # Your First Name
  author_last_name = "Innes", # Your Last Name
  author_email = "innesd@uoguelph.ca", # Your Email
  repo_url = NULL # The URL of the GitHub Repo (optional)
)

## Set {golem} options ----
golem::set_golem_options()



## Create Common Files ----
## See ?usethis for more information
#usethis::use_mit_license("Golem User") # You can set another license here
usethis::use_readme_rmd(open = FALSE)
devtools::build_readme()
# Note that `contact` is required since usethis version 2.1.5
# If your {usethis} version is older, you can remove that param

#usethis::use_code_of_conduct(contact = "Golem User")
#usethis::use_lifecycle_badge("Experimental")
#usethis::use_news_md(open = FALSE)

## Use git ----
usethis::use_git()
usethis::use_github(organisation = "CNM-University-of-Guelph", private = FALSE)

## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

## Favicon ----
# If you want to change the favicon (default is golem's one)
golem::use_favicon() # path = "path/to/ico". Can be an online file.
golem::remove_favicon() # Uncomment to remove the default favicon

## Add helper functions ----
golem::use_utils_ui(with_test = TRUE)
golem::use_utils_server(with_test = TRUE)

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile("dev/02_dev.R")
