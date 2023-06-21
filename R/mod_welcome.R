#' welcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_welcome_ui <- function(id){
  ns <- NS(id)

  bslib::nav_panel(
    "Welcome",

    div(class = 'resized-tab-container',
        gridlayout::grid_container(

          layout = gridlayout::new_gridlayout(c(
            "5px   1fr",
            "1fr  page"
          ),
          alternate_layouts = NULL
          ),


          gridlayout::grid_card(
            area = "page",
            wrapper = function(x) bslib::card_body(x, fillable = FALSE, fill = TRUE),
            # Set page background color
            tags$style(type="text/css", "body {background-color: #f2f2f2;}"),

            # Page title and subtitle
            h2("Welcome to IntakeInspectR"),
            # h3("A dashboard for automatically collected feed intake data"),

            # Further description
            p("IntakeInspectR is intended for cleaning an entire data set from
                 multiple Insentec (RIC) feed bins and multiple animals (we
                 have assumed 'cows' for now) and generating a file containing
                 daily intake (kg) for each cow. In addition, data removals
                 and manipulations are recorded in log files and appended in
                 columns to the original data. A typical workflow involves
                 proceeding through steps 1 to 3 at the top of the screen and
                 following the green buttons throughout. Step 2a cleans the
                 data based on individual feed bins, then Step 3a cleans the
                 data based on individual cows.
                 "),
            br(),


            p("IntakeInspectR is an open-source R package developed by
                   David Innes, Lucas Alcantara and John Cant from The University of Guelph.",
              "It is written as an open-source R Package which contains
                   the code for this RShiny app, but also all of the functions and
                   documentation for the underlying data cleaning which can
                   be used directly in R. It is distributed under a MIT
                   licence and can be found at: ",
              a("https://github.com/CNM-University-of-Guelph/IntakeInspectR",
                target="_blank", href="https://github.com/CNM-University-of-Guelph/IntakeInspectR")
            ),

            br(),
            p("Users should start with a subsection of data (e.g. 7 days) to explore their data.
              This app is deployed on shinyapps.io and is not designed to handle large data sets.
              ",
              a("Click here to learn more about data security when using this app.", target="_blank",
                href = "https://docs.posit.co/shinyapps.io/security-and-compliance.html")),


            # Add app features list
            br(),

            bslib::layout_columns(
              col_widths = c(5,-1,2,-4),
              bslib::card(
                class = 'border-0',
                fill = FALSE,
                wrapper = NULL,
                h4("Key Features"),
                HTML("<ul>
               <li>Import .DAT, .CSV or .TXT files</li>
               <li>Multi-step data cleaning</li>
               <li>Multiple interactive visualisations</li>
               <li>Export cleaned data and summarised data</li>
               <li>Save logs for easier reporting</li>
             </ul>")),

             bslib::card(
               class = 'border-0',
               br(),
               actionButton(ns("go_to_upload"), "Get Started", class = "btn-success btn-lg"),
               br()
               )




            ),

            bslib::layout_columns(

              bslib::card_image(file=app_sys("app/www/absc_logo.png"), width  = '300px', height = '120px'),
              bslib::card_image(file=app_sys("app/www/adc2.png"), width  = '300px', height='113px'),
              bslib::card_image(file=app_sys("app/www/DaG.png"), width  = '210px', height='120px')),


            p(em("This work was supported by the Canada First Research Excellence Fund (CFREF-2015-00004) under grant number THE2-020.
"))



          )
        )
    )
  )
}
col.names = c('transponder_id', 'cow_id',
              'feed_bin_id', 'start_time',
              'end_time', 'feed_duration',
              'start_weight', 'end_weight',
              'diet', 'intake')

#' welcome Server Functions
#'
#' @param id id
#' @param parent_session The session of parent session where this module is called,
#' so that updateNavbarPage knows which scope to look for NavbarPage
#'
#' @noRd
mod_welcome_server <- function(id, parent_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Switch to the first tab (Upload Data)
    observe({
      updateNavbarPage(session = parent_session, 'parent_navbarPage', selected = "1. Upload Data")
    }) %>% bindEvent(input$go_to_upload)



  })
}

## To be copied in the UI
# mod_welcome_ui("welcome_1")

## To be copied in the server
# mod_welcome_server("welcome_1")
