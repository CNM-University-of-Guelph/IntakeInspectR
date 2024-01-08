#' uploads UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_uploads_ui <- function(id){
  ns <- NS(id)

  # tabPanel(
    bslib::nav_panel(
    title = "1. Upload Data",

    div(class = 'resized-tab-container',
        gridlayout::grid_container(

          layout = gridlayout::new_gridlayout(
            c(
              "2px       0.25fr     0.75fr",
              "0.12fr    button_box button_box",
              "0.25fr     user_input summary",
              "0.63fr    user_input display_data "
            ),

          alternate_layouts = list(
            layout = c(
              "2px       1fr     ",
              "200px    button_box ",
              "400px    user_input ",
              "400px    summary",

              "400px    display_data"
            ),
            width_bounds = c(max = 900)
          )),




          gridlayout::grid_card(
            area = "button_box",
             wrapper = function(x) bslib::card_body(x, fill = FALSE),


          bslib::layout_columns(
            fill = FALSE,

            actionButton(ns("button_instructions"), "Upload Instructions", class = "btn-lg"),

            actionButton(ns("button_more_info"), "More about the data",  class = "btn-lg")
          )


          ),

          gridlayout::grid_card(
            area = "user_input",
             wrapper = function(x) bslib::card_body(x, fill=TRUE, fillable=FALSE),
            fileInput(ns("DAT_in"),
                      "Upload File/s (.DAT, .CSV, .TXT):",
                      multiple = TRUE,
                      buttonLabel =  "Browse...", # this uses the .btn-file class. JS script above makes it always match btn-success
                      accept = c(".DAT", ".dat",
                                 ".CSV", ".csv",
                                 ".TXT", ".txt")),
            shinyWidgets::switchInput(inputId = ns("use_demo_data"),
                                      value = FALSE,
                                      onLabel = "TRUE", # TRUE
                                      offLabel = "FALSE", # FALSE
                                      label = "Use demo data?",
                                      labelWidth = '100px',
                                      width='auto',
                                      inline=FALSE),
            br(),

            # hide until data uploaded:
            shinyjs::hidden(
              # bslib::card_body(
              bslib::card(
                id = ns("toggle_date_inputs"), # name needed for toggle to unhide
                wrapper = function(x) bslib::card_body(x,
                                                       fill=FALSE, #card won't change size inside of parent
                                                       fillable=TRUE #contents of card will fill out
                                                       ),
                # Dates
                dateRangeInput(ns('dateRange'),
                               label = 'Date range to use: ',
                               start = NULL, end = NULL
                ),

                # Feed Bin IDs
                selectInput(
                  inputId = ns('bin_id'),
                  label = "Select feed bin ID/s:",
                  multiple = TRUE,
                  selectize = FALSE,# so that it stays in a list format
                  choices = NULL # This is updated by server to include vec_bins
                ),

                # Animal IDs
                selectInput(
                  inputId = ns('animal_id'),
                  label = "Select Animal ID/s (caution: see instructions):",
                  multiple = TRUE,
                  selectize = FALSE, # so that it stays in a list format
                  choices = NULL # This is updated by server to include vec_animals
                ),
                p("All animals and feed bin IDs are selected by default. Use Shift and Ctrl keys to select multiple."),




                actionButton(ns('filter_data'), label = "Filter data", class='btn-success')
              )
            )
          ),

          gridlayout::grid_card(
            area = "summary",
            wrapper = function(x) bslib::card_body(x, fill = FALSE, fillable=TRUE, class = "p-0 margin-top:0 margin-bottom:0"),
            bslib::layout_columns(
              fill=FALSE,
              fillable=FALSE,

              row_heights = 1,
              # height = '140px',

              gap="4px",
              bslib::value_box(
                title = "Number of feed bins:",
                #Format to copy default value_box() value size:
                value = textOutput(ns("bin_n")),
                showcase = bsicons::bs_icon("basket2"),
                height = "150px",
                theme = "primary"
              ),
              bslib::value_box(
                title = "Number of animals:",
                value = textOutput(ns("animal_n")) ,
                showcase = fct_animal_icon('white'),
                height = "150px",
                theme = "primary"
              ),
              bslib::value_box(
                title = "Histogram of intake events",
                value = "",
                showcase = plotly::plotlyOutput(ns("hist_events")),
                theme = "primary",
                showcase_layout = bslib::showcase_left_center(
                  width = 0.5,
                   # max_height = "100px",
                  max_height_full_screen = 0.85
                ),
                full_screen = TRUE,
                p("Click to expand"),
                 height = "150px"
              )
              )
            ),






          gridlayout::grid_card(
            area = "display_data",
            wrapper = bslib::card_body,
            full_screen = TRUE,
            strong("Data structure:"),
            p("This shows the column names of the data, and the row and column numbers update to match the filters applied. The data can be re-filtered based on the original data that was uploaded but only the filtered data is used in the following pages."),
            verbatimTextOutput(ns("dynamic_glimpse")) %>%  shinycssloaders::withSpinner(type = 7)
          )
        )
    )
  )

}

#' uploads Server Functions
#'
#' @noRd
mod_uploads_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # setup a reactive value -- used for changing ui output based on events later
    variable_out <- reactiveValues(current_df = 1)

    ###########################################################
    # File import
    ###########################################################
    full_data <- reactive({
      if(input$use_demo_data == FALSE){
         # require there to be user input from UI, or else don't execute
      req(input$DAT_in)

     filetype <- fct_check_filetypes(input$DAT_in$name)


      print('DEV: Recalculating intake as start_weight_kg - end_weight_kg')

      shinybusy::show_modal_spinner(spin = 'orbit', text = 'Processing files...')

      if(filetype %in% c("dat","DAT")){
        df_out <- fct_import_DAT_default(.x = input$DAT_in$datapath,
                               .y = input$DAT_in$name)
        } else if(filetype %in% c("csv", "txt", "CSV", "TXT")){
        df_out <- fct_import_csv_txt(.x = input$DAT_in$datapath)
        }

      # coerce all numbers to doubles:
      df_out <- df_out %>%
        dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ as.double(.x)))

      shinybusy::remove_modal_spinner()

      variable_out$current_df <- tibble::as_tibble(df_out)
      # browser()

      return(df_out)
      } else if(input$use_demo_data){
        variable_out$current_df <- tibble::as_tibble(IntakeInspectR::demo_insentec_data)

        return( tibble::as_tibble(IntakeInspectR::demo_insentec_data ))# example data stored in ./data/

      }


    })





    ###########################################################
    # calculate min/max dates to help with filtering
    ###########################################################

    dates_min_max <-  reactive({
      dates_list_out <- fct_get_dates_min_max(full_data())

      # at same time, unhide the UI related to dates:
      shinyjs::show(id = 'toggle_date_inputs', anim = TRUE)

      return(dates_list_out)
    })

    # set range of dates allowed based on df
    observe({
      updateDateRangeInput(inputId = "dateRange",
                           start = dates_min_max()$date_min,
                           end = dates_min_max()$date_max,
                           min = dates_min_max()$date_min,
                           max = dates_min_max()$date_max)
    })

    #############################################################
    # Set up animal ID and feed bin ID

    observe({

      vec_animals <- full_data() %>%
        dplyr::pull(.data$animal_id) %>% unique() %>% sort()

      vec_bins <- full_data() %>%
                dplyr::pull(.data$bin_id) %>% unique() %>% sort()

      freezeReactiveValue(input, 'animal_id') # doesn't evaluate on load when no data available
      freezeReactiveValue(input, 'bin_id')

      updateSelectInput(inputId = 'animal_id', choices = vec_animals, selected = vec_animals)
      updateSelectInput(inputId = 'bin_id', choices = vec_bins, selected = vec_bins)
    })






    #############################################################





    # when button pressed, filter df and return it as full_data()
    sub_data <-  reactive({

      print(paste('subsetting data by dates', paste0(input$dateRange, collapse = " to ")))

      sub_out <-
        isolate(full_data()) %>%
        dplyr::filter(.data$date >= lubridate::ymd(input$dateRange[1]) &  .data$date <= lubridate::ymd(input$dateRange[2])) %>%
        dplyr::filter(.data$animal_id %in% input$animal_id) %>%
        dplyr::filter(.data$bin_id %in% input$bin_id)

      variable_out$current_df <- tibble::as_tibble(sub_out)

      return(sub_out)

    }) %>% shiny::bindEvent(input$filter_data)

    # Observe sub_data() for a change, to execute
    observe({ sub_data()  })


    ###########################################################
    # Outputs
    output$glimpse_cols <-  renderPrint({
      print(colnames(full_data()) %>% t())
    })






    # dynamically change display:
    #https://mastering-shiny.org/reactivity-components.html#one-output-modified-by-multiple-inputs
    output$dynamic_glimpse <- renderPrint({
      req(full_data()) # require data input before doing anything else

      if(tibble::is_tibble(variable_out$current_df)){
        dplyr::glimpse(variable_out$current_df)
      } else { "ERROR" }
    })


    output$bin_n <- renderText({
      req(full_data()) # require data input before doing anything else

      if(tibble::is_tibble(variable_out$current_df)){
        variable_out$current_df %>%
          dplyr::pull(.data$bin_id) %>%
          dplyr::n_distinct()
      } else { "ERROR" }
    })

    output$animal_n <- renderText({
      req(full_data()) # require data input before doing anything else

      if(tibble::is_tibble(variable_out$current_df)){
        variable_out$current_df %>%
          dplyr::pull(.data$animal_id) %>%
          dplyr::n_distinct()
      } else { "ERROR" }

      })


    output$hist_events <- plotly::renderPlotly({
      req(full_data()) # require data input before doing anything else

      if(tibble::is_tibble(variable_out$current_df)){
        fct_plotly_hist_transp(variable_out$current_df,
                               x = .data$intake,
                               x_lab = "Feed intake (kg as-fed per feeding event)")
      } else { "ERROR" }

    })


    ####################################################
    # Buttons
    observe({
      fct_show_custom_modal(fct_modal_content_uploads_more_info(), title = "More about the data")
    }) %>% bindEvent({ input$button_more_info })

    observe({
      fct_show_custom_modal(fct_modal_content_uploads_instructions(), title = "Instructions")
    }) %>% bindEvent({ input$button_instructions})




    # return the most current df, i.e. the one that was either uploaded or filtered
    return(reactive(variable_out$current_df))


  })
}

## To be copied in the UI
# mod_uploads_ui("uploads_1")

## To be copied in the server
# mod_uploads_server("uploads_1")
