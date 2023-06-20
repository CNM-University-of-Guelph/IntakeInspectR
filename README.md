
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IntakeInspectR

<!-- badges: start -->
<!-- badges: end -->

## Dev notes:
This shiny packages uses golem, see: https://engineering-shiny.org/golem.html

To run the shiny app when developing use the function: `golem::run_dev()` (can also be run under the Addins button in RStudio)
This will execute the run_dev.R file, located in `/dev` folder. This script handles re-loading and documenting the package as required, as well as setting up some R shiny options (which can be changed as needed). The last line will run app, and takes 1 custom argument: `run_app(load_demo_data = FALSE)`. If `load_demo_data` is set to TRUE, then reticulate (and python) is not loaded, and the demo data located in `/data/` is loaded into the environment. The `files_in` module is still loaded, but doesn't allow any user-uploads. Use `load_demo_data = FALSE` to upload .DAT files. 

Each tab of the main page is it's own module, and follows the convention of naming the scripts `mod_*.R`. These modules contain a function for the server and a function for the UI, which are then called by the `/R/app_server.R` and `/R/app_UI.R` scripts. 

There are various differences to normal Shiny as this is a package. One of the main ones is that the package is documented using roxygen2, which also handles the NAMEPSPACE. See https://r-pkgs.org/man.html

This means that we cannot use calls to `library()`, and it is considered best practice to use the package name before each function used (like `dplyr::select()`), except in some cases where you might really want to import a function into the packages namespace (e.g. some ggplot functions). 

This also handles which functions are `@export` to our namespace. Currently the documentation is limited and needs a lot of work. The golem package uses a lot of `@noRd` which means that no documentation is generated. This might need changing to meet some checks. 




## Installation

You can install the development version of IntakeInspectR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("CNM-University-of-Guelph/IntakeInspectR")
```
or 
```r
remotes::install_github("CNM-University-of-Guelph/IntakeInspectR")
```
Won't work while private.
