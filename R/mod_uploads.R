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
              "155px     user_input summary",
              "1fr    user_input display_data "
            ),

            alternate_layouts = list(
              layout = c(
                "2px       1fr     ",
                "400px    user_input ",
                "400px    summary",
                "400px    display_data"
              ),
              width_bounds = c(max = 900)
            )),

          gridlayout::grid_card(
            area = "user_input",
             wrapper = function(x) bslib::card_body(x, fill=TRUE, fillable=FALSE),
            bslib::card(
              wrapper = function(x) bslib::card_body(x,
                                                     fill=TRUE, #card won't change size inside of parent
                                                     fillable=TRUE #contents of card will fill out
              ),

              fileInput(ns("DAT_in"),
                        "Upload File/s (.DAT, .CSV, .TXT):",
                        multiple = TRUE,
                        buttonLabel =  "Browse...", # this uses the .btn-file class. JS script above makes it always match btn-success
                        accept = c(".DAT", ".dat",
                                   ".CSV", ".csv",
                                   ".TXT", ".txt")),
              actionButton(ns("button_instructions"), "Upload Instructions", class = "btn-lg"),

              shinyWidgets::switchInput(inputId = ns("use_demo_data"),
                                        value = FALSE,
                                        onLabel = "TRUE", # TRUE
                                        offLabel = "FALSE", # FALSE
                                        label = "Use demo data?",
                                        labelWidth = '150px',
                                        width='auto',
                                        inline=FALSE),
              ),

            bslib::accordion(
              id = ns('accordian_custom_colnames'), # must have ID to work
              open = FALSE,
              bslib::accordion_panel(
                title = "Advanced: Custom column names",
                p("
                  The following column names are required if uploading a .csv
                  or .txt file. If your file uses different column names,
                  define them under 'uploaded_names' below. They will then be renamed
                  to the required names for this app.
                  "),
                strong("Double click cells to edit."),

                DT::DTOutput(ns("df_custom_colnames")),

              )),

            br(),

            # hide until data uploaded:
            shinyjs::hidden(
              bslib::card(
                id = ns("toggle_date_inputs"), # name needed for toggle to unhide
                wrapper = function(x) bslib::card_body(x,
                                                       fill=FALSE, #card won't change size inside of parent
                                                       fillable=TRUE #contents of card will fill out
                                                       ),
                h5("Filter uploaded data (optional)"),
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
              gap="4px",
              bslib::value_box(
                title = "Number of feed bins:",
                #Format to copy default value_box() value size:
                value = textOutput(ns("bin_n")),
                showcase = bsicons::bs_icon("basket2"),
                height = "150px",
                theme = "secondary"
              ),
              bslib::value_box(
                title = "Number of animals:",
                value = textOutput(ns("animal_n")) ,
                showcase = fct_animal_icon('white'),
                height = "150px",
                theme = "secondary"
              ),
              bslib::value_box(
                title = "Histogram of intake events",
                value = "",
                showcase = plotly::plotlyOutput(ns("hist_events")),
                theme = "secondary",
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


          # gridlayout::grid_card(
          #   area = "display_data",
          #   wrapper = bslib::card_body,
          #   full_screen = TRUE,
          #   strong("Data structure:"),
          #   p("This shows the column names of the data, and the row and column numbers update to match the filters applied. The data can be re-filtered based on the original data that was uploaded but only the filtered data is used in the following pages."),
          #   verbatimTextOutput(ns("dynamic_glimpse")) %>%  shinycssloaders::withSpinner(type = 7)
          # ),
          #
          gridlayout::grid_card(
            area = "display_data",
            wrapper = bslib::card_body(class = "p-0", fillable = FALSE, fill = TRUE), # this removes the padding around edges
            full_screen = TRUE,

            bslib::navset_card_pill( # a card with nav tabs:
              id = ns('display_tabs'), #used for input$ to see active tab

              bslib::nav_panel(
                title = "Intro",
                value = "display_intro", #for accessing input$ details
                bslib::card_title("Upload data"),
                em("Hover mouse over bottom right of screen to show button to expand view. Use Esc or click Close to return to normal screen."),

                h4("Quick Start"),
                tags$div(
                  tags$ol(
                    tags$li(tags$strong("Check file requirements:"), " Click on 'Upload Instructions' for details about which files can be uploaded."),
                    tags$li(tags$strong("Upload files:"), " Click the 'Browse' button to open a window to select the file from your computer to upload."),
                    tags$li(tags$strong("OR demo data:"), " Alternatively, click the 'demo data' toggle to use built-in data."),
                    tags$li(tags$strong("Verify upload:"), " Look at the 'data structure' tab to see that the file successfully uploaded. A warning will be displayed for some problems, such as incorrect column names."),
                    tags$li(tags$strong("Go to Step 2a:"), " Navigate to the '2b. By Bin - Clean' page to start the cleaning pipeline.")
                  )
                )


              ),

              bslib::nav_panel(
                title = "Data Structure",
                value = "display_structure", #for accessing input$ details
                bslib::card_title("Data Structure"),
                p("This shows the column names of the data, and the row and column numbers update to match the filters applied. The data can be re-filtered based on the original data that was uploaded but only the filtered data is used in the following pages."),

                br(),
                verbatimTextOutput(ns("dynamic_glimpse")) %>%  shinycssloaders::withSpinner(type = 7)
              ),

              bslib::nav_panel(
                title = "Download uploaded data",
                value = "download_data", #for accessing input$ details
                bslib::card_title("Downloads"),
                p("Use the following buttons to download either the .rds file (for use in R and retaining date/time formats, etc) or the .csv file. This will be the same data that is displayed in the 'data structure' tab, i.e. multiple files combined and after any filtering."),

                bslib::layout_columns(
                  fill=TRUE,
                  fillable=TRUE,
                  row_heights = 1,
                  col_widths = c(3,3,-6), # negative means empty cols
                  gap="4px",
                  downloadButton(ns("uploaded_df_rds"), "Download .rds", class = c('btn-info')),
                  downloadButton(ns("uploaded_df_csv"), "Download .csv", class = c('btn-info')),
                ),
              ),



            )
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

          df_out <- fct_import_csv_txt(.x = input$DAT_in$datapath,
                                       colnames_df = react_custom_cols$df)

          }

        # coerce all numbers to doubles:
        df_out <- df_out %>%
          dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ as.double(.x)))

        shinybusy::remove_modal_spinner()

        variable_out$current_df <- tibble::as_tibble(df_out)

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

      #in addition, collapse custom column name accordian:
      bslib::accordion_panel_close(id = 'accordian_custom_colnames', values = TRUE)

      return(dates_list_out)
    })

    observe({
      # & unhide download buttons
      # Hide cleaning button until after data is uploaded
      shinyjs::toggleState("uploaded_df_rds", condition = tibble::is_tibble(variable_out$current_df))
      shinyjs::toggleState("uploaded_df_csv", condition = tibble::is_tibble(variable_out$current_df))

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

    ##############################################################
    # Custom column names
    # This uses an editable DT to collect new names for columns

    react_custom_cols <- reactiveValues(
      df = data.frame(
        required_names = c('animal_id', 'bin_id', 'start_time', 'end_time',
                           'duration_sec', 'start_weight_kg', 'end_weight_kg', 'date'),
        uploaded_names = c('animal_id', 'bin_id', 'start_time', 'end_time',
                           'duration_sec', 'start_weight_kg', 'end_weight_kg', 'date')
      )
      )

    # output DT as editable
    output$df_custom_colnames <- DT::renderDT({
      DT::datatable(
        react_custom_cols$df,
        options = list(
          dom = 't'  # Display only the table without additional controls
        ),
        # allow edits in second column only
        editable = list(target = "cell", disable = list(columns = 0)),
        rownames = FALSE
      )
    })


    # Observe for change uses *_cell_edit which returns the position and new value
    # Then at end, overwrite value stored in df
    observeEvent(input$df_custom_colnames_cell_edit, {
      #get values
      info = input$df_custom_colnames_cell_edit
      i = as.numeric(info$row)
      j = as.numeric(info$col)+1
      k = as.character(info$value)

      #write values to reactive
      react_custom_cols$df[i,j] <- k
    })




    #############################################################
    # Filter data
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


    # dynamically change display:
    #https://mastering-shiny.org/reactivity-components.html#one-output-modified-by-multiple-inputs
    output$dynamic_glimpse <- renderPrint({
      req(full_data()) # require data input before doing anything else

      if(tibble::is_tibble(variable_out$current_df)){
        dplyr::glimpse(variable_out$current_df)

        cat("\nColumn names defined for renaming:\n")
        print(react_custom_cols$df)
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
    # observe({
    #   fct_show_custom_modal(fct_modal_content_uploads_more_info(), title = "More about the data")
    # }) %>% bindEvent({ input$button_more_info })

    observe({
      fct_show_custom_modal(fct_modal_content_uploads_instructions(), title = "Instructions")
    }) %>% bindEvent({ input$button_instructions})


    ##################################################
    # Download handlers
    output$uploaded_df_rds <- downloadHandler(
      filename = function() {
        paste0("uploaded_filtered_df_",
               format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      },
      content = function(file) {
        saveRDS(variable_out$current_df, file = file)
      }
    )
    output$uploaded_df_csv <- downloadHandler(
      filename = function() {
        paste0("uploaded_filtered_df_",
               format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        # format start and end time to match original upload:
        # This is important if someone uploads, then downloads and then re-uploads.
        variable_out$current_df %>%
          dplyr::mutate(
            start_time = format(start_time, "%H:%M:%S"),
            end_time = format(end_time, "%H:%M:%S"),
            date = format(date, "%Y-%m-%d")
          ) %>%
          data.table::fwrite( file = file)
      },
      contentType = "text/csv"
    )



    # return the most current df, i.e. the one that was either uploaded or filtered
    return(reactive(variable_out$current_df))


  })
}

## To be copied in the UI
# mod_uploads_ui("uploads_1")

## To be copied in the server
# mod_uploads_server("uploads_1")
