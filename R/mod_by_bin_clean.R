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
            "155px user_input  summary_box",
            "1fr user_input  Display "
          ),
          alternate_layouts = list(
            layout = c(
              "2px       1fr     ",
              "200px    user_input ",
              "300px    summary_box",
              "400px    Display"
            ),
            width_bounds = c(max = 900)
          )),


          ############################################### #
          # Inputs panel ----
          ################################################ #
          gridlayout:: grid_card(
            area = "user_input",
            wrapper = function(x) bslib::card_body(x, fill=TRUE, fillable=TRUE ),

            actionButton(ns('execute_clean'), label = "Clean data", class = c("btn-lg btn-success")),
            # br(),
            actionButton(ns("button_more_info"), "Detailed overview of the cleaning"),

            actionButton(ns("button_flowchart"), "Flow chart of all steps"),

            actionButton(ns('bypass_clean'), label = "Bypass 'by bin' cleaning", class = c("btn-lg btn-outline-success")),
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
              ),
              bslib::accordion_panel(
                title = "Download function outputs",
                div(
                  style = "display: flex; flex-direction: column; align-items: center;",
                  p("These files are the",
                    tags$code("df_cleaned"),
                    "output from the list returned by",
                    tags$code("f_by_bin_clean().")),
                  p("Consider downloading the .rds file if using with R as it retains date and time formatting."),
                  downloadButton(ns("download_df_cleaned_rds"), "Download .rds", class = c('btn-info')),
                  br(),
                  downloadButton(ns("download_df_cleaned_csv"), "Download .csv", class = c('btn-info'))
                )
              )
            )
          ),

          ################################################ #
          # Display panel ----
          ################################################ #
          gridlayout::grid_card(
            area = "Display",
            wrapper = bslib::card_body(class = "p-0", fillable = FALSE, fill = TRUE), # this removes the padding around edges
            full_screen = TRUE,

            bslib::navset_card_pill( # a card with nav tabs:
              id = ns('display_tabs'), #used for input$ to see active tab

              bslib::nav_panel(
                title = "Intro",
                value = "display_intro", #for accessing input$ details
                bslib::card_title("Clean data - grouped by feed bin"),
                p("
                This step of the cleaning checks the row-by-row changes in feed
                bin weights and times."),
                p("The weights and durations are replaced
                for some specific conditions, others are flagged as errors (but
                not replaced in this step). In addition, some data are removed but are
                available for download as separate files, see 'detailed overview' button
                for more information.
                "),
                em("Hover mouse over bottom right of screen to show button to expand view. Use Esc or click Close to return to normal screen."),

                h4("Quick Start"),
                tags$div(
                  tags$ol(
                    tags$li(tags$strong("Ensure data upload:"), " The 'Clean data' button will become active once the data is successfully uploaded."),
                    tags$li(tags$strong("Execute:"), " Click the 'Clean data' button to initiate the cleaning process. This function utilises predefined thresholds set in the 'Advanced: Change thresholds' drop-down box."),
                    tags$li(tags$strong("Understand thresholds:"), " Familiarise yourself with the detailed overview and flow chart to understand how the values defined in the 'Advanced: Change thresholds' drop-down affect the cleaning process."),
                    tags$li(tags$strong("Verify successful cleaning:"), " If the cleaning process is successful, the value boxes at the top of the page will be populated, and the Log, Data Structure, and View Table tabs will also be updated."),
                    tags$li(tags$strong("Visualise:"), " For the best evaluation of the cleaning results, navigate to the '2b. By Bin - Vis' page and review the tables and plots of the cleaned data.")
                  )
                ),

              ),

              bslib::nav_panel(
                title = "Log",
                value = "display_log", #for accessing input$ details
                bslib::card_title("Log - 'By Bin' cleaning"),
                p("This log can be copied from this box or downloaded with all other logs on Final Summary tab at end of analysis."),
                verbatimTextOutput(ns("dynamic_log"), placeholder=TRUE),
              ),

              bslib::nav_panel(
                title = "Data Structure",
                value = "display_structure", #for accessing input$ details
                bslib::card_title("Data Structure"),
                p("This shows the column names and dimensions of the data frame returned by the by bin cleaning function.
                It might be expected that some rows are missing compared to the original file uploaded due to 0 kg and some errors being removed. See the 'Download removed rows' drop down box on the left of the screen if these are of interest."),
                verbatimTextOutput(ns("dynamic_glimpse"), placeholder=TRUE),
              ),

              bslib::nav_panel(
                title = "View Table",
                value = "display_table", #for accessing input$ details
                bslib::card_title("View Table"),
                p("The data frame returned from the cleaning table can be explored here. Use the CSV/Excel buttons to download the visible rows, or download the full file under 'Download function outputs'. "),
                DT::DTOutput(ns('bybin_DT')) %>%  shinycssloaders::withSpinner(type=7)
              ),



            )
          ),

          ################################################ #
          # Summary boxes ----va
          ################################################ #
          gridlayout::grid_card(
            area = "summary_box",
            wrapper = function(x) bslib::card_body(x, fill = FALSE, fillable=FALSE, class = "p-0 margin-top:0 margin-bottom:0"),
            bslib::layout_columns(
              fill=FALSE,
              fillable=FALSE,

              row_heights = 1, # Set individual value box heights, and then layout_columns can change to fit grid_card when it wraps

              gap="4px",

              bslib::value_box(
                title = "Feed weights replaced:",
                #Format to copy default value_box() value size:
                value = p(textOutput(ns("n_weight_replaced"), inline = TRUE), style = "font-size: 30px;") ,
                showcase = bsicons::bs_icon('arrow-repeat'),
                theme = 'info',
                height = '150px'
              ),
              bslib::value_box(
                title = "Feed durations replaced:",
                value = p(textOutput(ns("n_durations_replaced"), inline = TRUE), style = "font-size: 30px;") ,
                showcase = bsicons::bs_icon('arrow-repeat'),
                theme = 'info',
                height = '150px'
              ),
              bslib::value_box(
                title = "Start weight errors:",
                value = p(textOutput(ns("n_start_weight_kg_errors"), inline = TRUE), style = "font-size: 30px;"),
                showcase = bsicons::bs_icon('exclamation-triangle'),
                theme = 'danger',
                height = '150px'
              ),
              bslib::value_box(
                title = "End weight errors:",
                value = p(textOutput(ns("n_end_weight_kg_errors"), inline = TRUE), style = "font-size: 30px;"),
                showcase = bsicons::bs_icon('exclamation-triangle'),
                theme = 'danger',
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

    selected_list_bybin <- reactiveValues()
    is_bybin_bypass <- reactiveVal(FALSE)

    observe({
        shinybusy::show_modal_spinner(spin = 'orbit', text = 'Cleaning data...')

        bslib::nav_select('display_tabs', 'display_log')

        list_cleaned <-
          f_by_bin_clean(df_uploaded(),
                         zero_thresh = input$zero_threshold, # default 0.3
                         feedout_thresh = input$feedout_threshold, # default 10
                         col_bin_ID = .data$bin_id,
                         col_date = .data$date,
                         col_start_time = .data$start_time,
                         col_start_weight_kg = .data$start_weight_kg,
                         col_end_weight_kg = .data$end_weight_kg,
                         col_intake = .data$intake,
                         log = TRUE)

        # override reactive variable
        selected_list_bybin$current <- list_cleaned

        is_bybin_bypass(FALSE)

        shinybusy::remove_modal_spinner()

      }) %>% bindEvent(input$execute_clean)


    ######
    # Bypass

    observe({
      bslib::nav_select('display_tabs', 'display_log')

      list_cleaned <- f_by_bin_bypass_shiny(df_uploaded(), log = TRUE)
      # override reactive variable
      selected_list_bybin$current <- list_cleaned

      is_bybin_bypass(TRUE)
    }) %>% bindEvent(input$bypass_clean)


    # Get most recent list_cleaned_data when either the execute_clean or bypass_clean button is pressed.
    list_cleaned_data <- reactive({
      return(selected_list_bybin$current)
    }) %>% bindEvent(c(input$execute_clean, input$bypass_clean), ignoreInit = TRUE)



    # set up reactivevalues list to store log data (from logr in `f_by_bin_clean`)
    log <- reactiveValues()

    # observe table to override output to show in window:
    observe({
      print_out$df_cleaned <- tibble::as_tibble(list_cleaned_data()$df_cleaned)

      # read in log file to print to glimpse window:
      log$by_bin <- readLines(list_cleaned_data()$log_path)

    })



    # dynamically change display:
    #https://mastering-shiny.org/reactivity-components.html#one-output-modified-by-multiple-inputs
    output$dynamic_log <- renderPrint({
      cat(paste(log$by_bin, collapse = "\n"))
    })

    output$dynamic_glimpse <- renderPrint({
      if(tibble::is_tibble(print_out$df_cleaned)){ # requires that analysis was run
        dplyr::glimpse(print_out$df_cleaned)
      }
    })


    # View table

    output$bybin_DT <- DT::renderDT({
      print_out$df_cleaned %>%
        fct_DT_pages(
          pageLength = 20,
          scrollY = 380
          )
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
        .data$is_end_time_overlap_error,
        TRUE
      )
    })


    # end weight errors
    output$n_end_weight_kg_errors <- renderText({

      req(list_cleaned_data())

      fct_check_for_valuebox_bin(
        list_cleaned_data()$df_cleaned,
        .data$category_end_weight_kg,
        "error"
      )
    })


    output$n_start_weight_kg_errors <- renderText({
      req(list_cleaned_data())

      fct_check_for_valuebox_bin(
        list_cleaned_data()$df_cleaned,
        .data$check_start_weight_kgs,
        "error"
      )


    })



    ################################################################# #
    # Buttons: ----
    ################################################################# #

    # Hide cleaning button until after data is uploaded
    observe({
      shinyjs::toggleState("execute_clean", condition = tibble::is_tibble(df())) #indicating data has been uploaded
      shinyjs::toggleState("bypass_clean", condition = tibble::is_tibble(df()))
    })

    # Hide download buttons until after cleaning
    observe({
      shinyjs::toggleState("download_0kg", condition = input$execute_clean > 0)
      shinyjs::toggleState("download_errors", condition = input$execute_clean > 0)
      shinyjs::toggleState("download_df_cleaned_csv", condition = input$execute_clean > 0)
      shinyjs::toggleState("download_df_cleaned_rds", condition = input$execute_clean > 0)

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
      svgContent <- readLines(con = app_sys("app/www/by_bin_workflow_v4.svg"), warn = FALSE)

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
        paste0("by_bin_rows_removed_0kg_intake_",format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        data.table::fwrite(list_cleaned_data()$df_0kg, file = file)
      },
      contentType = "text/csv"
    )

    output$download_errors<- downloadHandler(
      filename = function() {
        paste0("by_bin_rows_removed_step2_errors_",format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        data.table::fwrite(list_cleaned_data()$df_step2_errors, file = file)
      },
      contentType = "text/csv"
    )



    output$download_df_cleaned_rds <- downloadHandler(
      filename = function() {
        paste0("by_bin_df_cleaned_",
               format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      },
      content = function(file) {
        saveRDS(list_cleaned_data()$df_cleaned, file = file)
      }
    )
    output$download_df_cleaned_csv <- downloadHandler(
      filename = function() {
        paste0("by_bin_df_cleaned_",
               format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        data.table::fwrite(list_cleaned_data()$df_cleaned, file = file)
      },
      contentType = "text/csv"
    )

    user_bybin_thresholds <- reactive({
      list(
        zero_thresh = input$zero_threshold, # default 0.3
        feedout_thresh = input$feedout_threshold # default 10
      )

    })



    return(reactive({
      list(
        df_list_bybin = list_cleaned_data(),
        is_bybin_bypass = is_bybin_bypass(),
        log_path = list_cleaned_data()$log_path,
        user_bybin_thresholds = user_bybin_thresholds()
        )
      })
      )
  })
}

## To be copied in the UI
# mod_by_bin_clean_ui("by_bin_clean_1")

## To be copied in the server
# mod_by_bin_clean_server("by_bin_clean_1")
