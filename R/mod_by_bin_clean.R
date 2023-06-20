#' by_bin_clean UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_by_bin_clean_ui <- function(id){
  ns <- NS(id)

  # tabPanel(
  bslib::nav_panel(
    title = "2a. By Bin - Clean",

    div(class = 'resized-tab-container',
        gridlayout::grid_container(

          layout = gridlayout::new_gridlayout(c(
            "2px    0.25fr      0.75fr",
            "120px header_box  header_box",
            "0.3fr user_input  summary_box",
            "0.7fr user_input  Display "
          ),
          alternate_layouts = list(
            layout = c(
              "2px       1fr     ",
              "150px    header_box ",
              "200px    user_input ",
              "300px    summary_box",
              "400px    Display"
            ),
            width_bounds = c(max = 900)
          )),


          gridlayout::grid_card(
            area = "header_box",
            full_screen = FALSE,
            # min_height = '100px',
            # max_height = '150px',
            # wrapper = NULL,
            # wrapper=function(x) bslib::card_body(x, min_height = '180px', max_height = '200px'),
            strong("Clean data - grouped by feed bin"),
            p("
        This step of the cleaning checks the row-by-row changes in feed bin weights and times.
        The weights and durations are replaced for some specific conditions,
        others are flagged as errors (but not replaced in this step). Some data are removed but are available
        for download as separate files, see detailed overview for more information.")
          ),



        ############################################### #
        # Inputs panel ----
        ################################################ #
        gridlayout:: grid_card(
          area = "user_input",
          wrapper = function(x) bslib::card_body(x, fill=FALSE, fillable=TRUE ),


          actionButton(ns('execute_clean'), label = "Clean data", class = c("btn-lg btn-success")),
          # br(),
          actionButton(ns("button_more_info"), "Detailed overview of the cleaning"),

          actionButton(ns("button_flowchart"), "Flow chart of all steps"),
          # br(),
          # p("View each functions R Documentation:"),

          bslib::accordion(
            id = ns('accordian'), # must have ID to work
            open = FALSE,
            bslib::accordion_panel(
              title = "Advanced: Change thresholds",
              numericInput(inputId = ns("feedout_threshold"),
                           label = "Enter number to define 'feedout_threshold':",
                           value = 10,
                           min = 1,
                           step = 1),
              numericInput(inputId = ns("zero_threshold"),
                           label = "Enter number to define 'zero_threshold':",
                           value = 0.3,
                           min = 0,
                           step = 0.1)
            ),
            bslib::accordion_panel(
              title = "Advanced: R Documentation",
                div(
                  style = "display: flex; flex-direction: column; align-items: center;",
                  actionButton(ns("f_step1"), "Step 1 docs", class = "btn-fixed-size-rdocs btn-outline-success"),
                  br(),
                  actionButton(ns("f_step2"), "Step 2 docs", class = "btn-fixed-size-rdocs btn-outline-success"),
                  br(),
                  actionButton(ns("f_step3"), "Step 3 docs", class = "btn-fixed-size-rdocs btn-outline-success"),
                  br(),
                  actionButton(ns("f_step4"), "Step 4 docs", class = "btn-fixed-size-rdocs btn-outline-success"),
                  br(),
                  actionButton(ns("f_step5"), "Step 5 docs", class = "btn-fixed-size-rdocs btn-outline-success"),
                  br(),
                  actionButton(ns("f_step6"), "Step 6 docs", class = "btn-fixed-size-rdocs btn-outline-success")
                )
              ),
              bslib::accordion_panel(
                title = "Download removed rows",
                div(
                  style = "display: flex; flex-direction: column; align-items: center;",
                  p("
                  The 0 kg intakes are useful if you're interested in feed bin
                  visits where it detects the ear tag but no intake is
                  recorded, but would otherwise cause issues in the row-by-row
                  checks. They could be safely appended to cleaned data for
                  further analysis.
                  "),
                  downloadButton(ns("download_0kg"), "Download removed rows (0 kg intakes)", class = c('btn-info')),
                  br(),
                  downloadButton(ns("download_errors"), "Download removed rows (Step 2 Errors)", class = c('btn-info'))
                )
              )
            )
          ),

        ################################################ #
        # Display panel ----
        ################################################ #
        gridlayout::grid_card(
          area = "Display",
          full_screen=TRUE,
          wrapper = function(x) bslib::card_body(x),
          strong("Log and data structure"),
          verbatimTextOutput(ns("dynamic_glimpse"), placeholder=TRUE)

        ),

        ################################################ #
        # Summary boxes ----
        ################################################ #
        gridlayout::grid_card(
          area = "summary_box",
          wrapper = function(x) bslib::card_body(x, fill = FALSE, fillable=TRUE, class = "p-0 margin-top:0 margin-bottom:0"),
          bslib::layout_columns(
            fill=FALSE,
            fillable=FALSE,

            row_heights = 1, # Set individual value box heights, and then layout_columns can change to fit grid_card when it wraps

            gap="4px",

            bslib::value_box(
              title = "Feed weights replaced:",
              #Format to copy default value_box() value size:
              value = textOutput(ns("n_weight_replaced")) ,
              showcase = bsicons::bs_icon('arrow-repeat'),
              theme_color = 'info',
              height = '150px'
            ),
            bslib::value_box(
              title = "Feed durations replaced:",
              value = textOutput(ns("n_durations_replaced")) ,
              showcase = bsicons::bs_icon('arrow-repeat'),
              theme_color = 'info',
              height = '150px'
            ),
            bslib::value_box(
              title = "Start weight errors:",
              value = textOutput(ns("n_start_weight_errors")) ,
              showcase = bsicons::bs_icon('exclamation-triangle'),
              theme_color = 'danger',
              height = '150px'
            ),
            bslib::value_box(
              title = "End weight errors:",
              value = h5(textOutput(ns("n_end_weight_errors"))) ,
              showcase = bsicons::bs_icon('exclamation-triangle'),
              theme_color = 'danger',
              height = '150px'
            )
          )
        )
        )
    )
  )
}

#' by_bin_clean Server Functions
#' @param df data frame of uploaded (and potentially filtered) data from user
#'
#' @noRd
mod_by_bin_clean_server <- function(id, df){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

      # Check data has been uploaded:
    df_uploaded <- reactive({
     if(tibble::is_tibble(df())){  return(df())   }
    })

    # setup a reactive value -- used for changing ui output based on events later
    print_out <- reactiveValues(glimpse = 1)

    ######################################################## #
    # Data cleaning ----
    ######################################################## #

    list_cleaned_data <- reactive({

      shinybusy::show_modal_spinner(spin = 'orbit', text = 'Cleaning data...')

      list_cleaned <-
        f_by_bin_clean(df_uploaded(),
                       zero_thresh = input$zero_threshold, # default 0.3
                       feedout_thresh = input$feedout_threshold, # default 10
                       col_bin_ID = .data$feed_bin_id,
                       col_date = .data$date,
                       col_start_time = .data$start_time,
                       col_start_weight = .data$start_weight,
                       col_end_weight = .data$end_weight,
                       col_intake = .data$intake,
                       log = TRUE)

      shinybusy::remove_modal_spinner()

      return(list_cleaned)
    }) %>% bindEvent(input$execute_clean)




    # set up reactivevalues list to store log data (from logr in `f_by_bin_clean`)
    log <- reactiveValues()

    # observe table to override output to show in window:
    observe({
      print_out$glimpse <- tibble::as_tibble(list_cleaned_data()$df_cleaned)

      # read in log file to print to glimpse window:
      log$by_bin <- readLines(list_cleaned_data()$log_path)

    })



    # dynamically change display:
    #https://mastering-shiny.org/reactivity-components.html#one-output-modified-by-multiple-inputs
    output$dynamic_glimpse <- renderPrint({
      if(tibble::is_tibble(print_out$glimpse)){
        # print log file, then a glimpse of data:
        cat(paste(log$by_bin, collapse = "\n"))
        cat("\n")
        dplyr::glimpse(print_out$glimpse)
      }
    })

    ################################################################# #
    # Value Boxes: ----
    ################################################################# #


    output$n_weight_replaced <- renderText({

      req(list_cleaned_data())

      fct_check_for_valuebox_bin(
        list_cleaned_data()$df_cleaned,
        .data$is_corrected_intake_bybin,
        TRUE
      )
    })

    output$n_durations_replaced <- renderText({

      req(list_cleaned_data())

      fct_check_for_valuebox_bin(
        list_cleaned_data()$df_cleaned,
        .data$is_end_time_error,
        TRUE
      )
    })


    # end weight errors
    output$n_end_weight_errors <- renderText({

      req(list_cleaned_data())

      fct_check_for_valuebox_bin(
        list_cleaned_data()$df_cleaned,
        .data$category_end_weight,
        "error"
      )
    })


    output$n_start_weight_errors <- renderText({
      req(list_cleaned_data())

      fct_check_for_valuebox_bin(
        list_cleaned_data()$df_cleaned,
        .data$check_start_weights,
        "error"
      )


    })



    ################################################################# #
    # Buttons: ----
    ################################################################# #

    # Hide cleaning button until after data is uploaded
    observe({
      shinyjs::toggleState("execute_clean", condition = tibble::is_tibble(df())) #indicating data has been uploaded
    })

    # Hide download buttons until after cleaning
    observe({
      shinyjs::toggleState("download_0kg", condition = input$execute_clean > 0)
      shinyjs::toggleState("download_errors", condition = input$execute_clean > 0)
    })

    ################ #
    # Detailed Overview
    ################ #
    observe({

      fct_show_custom_modal(fct_modal_content_bybin_more_info(), title = "More about the 'By Bin' data cleaning")

    }) %>% bindEvent({ input$button_more_info })

    ################ #
    # Flow Chart
    ################ #
    observe({
      # Read the SVG file
      svgContent <- readLines(con = app_sys("app/www/by_bin_workflow.svg"), warn = FALSE)

      showModal(
        modalDialog(
          fct_modal_content_bybin_flowchart(svgContent),
          title = "Flow chart of by bin cleaning steps",
          size = "xl",
          easyClose = TRUE,
          footer = modalButton("Ok")
        )
      )


    }) %>% bindEvent({ input$button_flowchart})


    ################ #
    # Docs for Functions
    ################ #

    observe({ fct_show_modal_bin_functions('f_step1', output, session) }) %>% bindEvent({input$f_step1})
    observe({ fct_show_modal_bin_functions('f_step2', output, session) }) %>% bindEvent({input$f_step2})
    observe({ fct_show_modal_bin_functions('f_step3', output, session) }) %>% bindEvent({input$f_step3})
    observe({ fct_show_modal_bin_functions('f_step4', output, session) }) %>% bindEvent({input$f_step4})
    observe({ fct_show_modal_bin_functions('f_step5', output, session) }) %>% bindEvent({input$f_step5})
    observe({ fct_show_modal_bin_functions('f_step6', output, session) }) %>% bindEvent({input$f_step6})

    ################################################################# #
    # Download Buttons: ----
    ################################################################# #

    output$download_0kg <- downloadHandler(
      filename = function() {
        paste0("rows_removed_0kg_intake_",format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        data.table::fwrite(list_cleaned_data()$df_0kg, file = file)
      },
      contentType = "text/csv"
    )

    output$download_errors<- downloadHandler(
      filename = function() {
        paste0("rows_removed_step2_errors_",format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        data.table::fwrite(list_cleaned_data()$df_step2_errors, file = file)
      },
      contentType = "text/csv"
    )


    return(reactive(list_cleaned_data()))
  })
}

## To be copied in the UI
# mod_by_bin_clean_ui("by_bin_clean_1")

## To be copied in the server
# mod_by_bin_clean_server("by_bin_clean_1")
