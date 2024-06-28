#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    tags$head(
    # added js codes for handling window size changes
      tags$script(HTML('
          $(document).ready(function() {
            function adjustLayoutOnResize() {
              var navbarHeight = $(".navbar").outerHeight();
              $(".resized-tab-container").css("top", navbarHeight + "px");

              var windowHeight = $(window).height();
              var remainingHeight = windowHeight - navbarHeight;
              $(".grid-container").css("height", remainingHeight + "px");
            }

            // Attach the function to the window resize event
            $(window).on("resize", adjustLayoutOnResize);

          // Trigger a resize on page load
            $(window).trigger("resize");

            function checkNavbarCollapse() {
              // this waits for the resizing of the menu to finish so that it can get an accurate resize
              const interval = setInterval(() => {
                if ($(".navbar-collapse").hasClass("collapse") || $(".navbar-collapse").hasClass("show")) {
                  // Condition met: navbar-collapse classes are active
                  clearInterval(interval); // Stop the interval
                  adjustLayoutOnResize();
                }
              }, 100); // Interval delay in milliseconds
            }

            // Check navbar-collapse classes when menu button is pressed on smaller screens
            $(".navbar-toggle").on("click", checkNavbarCollapse);
          });
      ')),
      tags$script(HTML("
        $(document).ready(function() {
          $('.btn-file').addClass('btn-success');
        });
        "))
    ),



  bslib::page_navbar(

    theme = bslib::bs_theme(version = 5,
                            bootswatch = "flatly",
                            secondary = "#3498DB",
                            success = "#18BC20",
                            info = "#F8EE4B",
                            # Overrides default width that changes when value_box gets stacked vertically.
                            # Available when new version of bslib is released to fix github issue:
                            "bslib-value-box-horizontal-break-point" = "200px"
                            ),

    title = "IntakeInspectR",

    position = 'static-top', # this fixes bar at top, but fancy js above is required for resizing windows.
    id = 'parent_navbarPage',
    collapsible = TRUE, #collapse menu for mobile devices, see help
    fillable = TRUE,
    fillable_mobile = FALSE,
    inverse = FALSE, # background colour default from bootswatch theme, can set manually with bg =

    mod_welcome_ui("welcome_1"),
    mod_uploads_ui("uploads_1"),
    mod_by_bin_clean_ui("by_bin_clean_1"),
    mod_by_bin_vis_ui("by_bin_vis_1"),
    mod_by_animal_clean_ui("by_animal_clean_1"),
    mod_by_animal_vis_ui("by_animal_vis_1"),
    mod_final_summary_ui("final_summary_1"),
    bslib::nav_spacer(),
    bslib::nav_item(
      tags$a(
        paste0("v",utils::packageVersion("IntakeInspectR")),
        bsicons::bs_icon("github"),
        href = "https://github.com/CNM-University-of-Guelph/IntakeInspectR",
        target = "_blank"
      )
    )
  )

  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'

#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "IntakeInspectR"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    # call useShinyjs so that it can be used throughout modules
    shinyjs::useShinyjs(),
    shinybusy::add_busy_bar(color = 'darkred', height = '10px')
  )
}
