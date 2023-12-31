#' by_cow_clean UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_by_cow_clean_ui <- function(id){
  ns <- NS(id)

  # tabPanel(
  bslib::nav_panel(
    title = "3a. By Cow - Clean",

    div(class = 'resized-tab-container',
        gridlayout::grid_container(

          layout = gridlayout::new_gridlayout(c(
            "4px     0.25fr      0.75fr",
            "120px   button_box  button_box",
            "0.3fr   user_input  summary_box",
            "0.7fr   user_input  Display "
          ),
          alternate_layouts = list(
            layout = c(
              "4px       1fr     ",
              "150px    button_box ",
              "200px    user_input ",
              "300px    summary_box",
              "400px    Display"
            ),
            width_bounds = c(max = 900)
          )),

          gridlayout::grid_card(
            area = "button_box",
            # wrapper = bslib::card_body,
            full_screen = FALSE,
            strong("Clean data - grouped by cow ID"),
            p("
          This step of the cleaning identifies outliers by fitting a robust
          linear model to the duration vs intake data for each cow. Outliers
          are temporarily removed to fit a bisector regression, then new
          duration or intake values are estimated for each outlier.
          ")
          ),



          ############################################### #
          # Inputs panel ----
          ################################################ #
          gridlayout:: grid_card(
            area = "user_input",
            wrapper = function(x) bslib::card_body(x, fill=FALSE, fillable=TRUE ),


            actionButton(ns('execute_detect_outliers'), label = "Execute Outlier Detection", class = "btn-lg btn-success"),

            br(),
            actionButton(ns("button_more_info"), "Detailed overview of outlier detection"),
            br(),

            bslib::accordion(
              id = ns('accordian'), # must have ID to work
              open = FALSE,
              bslib::accordion_panel(
                title = "Advanced: Change outlier threshold",
                numericInput(inputId = ns("sd_threshold"),
                             label = "Enter residual SD threshold to define 'outliers':",
                             value = 5,
                             min = 0,
                             step = 0.5)

              ),
              bslib::accordion_panel(
                title = "Advanced: R Documentation",
                div(
                  style = "display: flex; flex-direction: column; align-items: center;",
                  actionButton(ns("f_outlier_func"), "f_flag_and_replace_outliers", class = " btn-outline-success"),

                )
              ),
              bslib::accordion_panel(
                title = "Download function outputs",
                div(
                  style = "display: flex; flex-direction: column; align-items: center;",
                  p("
                  These files are useful for tracking how the data was handled in this step,
                  but the Final Summary page has better formatted tables for download.
                  "),
                  downloadButton(ns("download_nested"), "Download raw nested output (.Rds)", class = 'btn-info'),
                  br(),
                  downloadButton(ns("download_merged"), "Download unnested data frame (.csv)", class = 'btn-info')
                )
              )
            )
          ),

        ################################################ #
        # Display panel ----
        ################################################ #
        gridlayout::grid_card(
          area = "Display",
          full_screen = TRUE,
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
              title = "Not outliers:",
              #Format to copy default value_box() value size:
              value = p(textOutput(ns("n_not_error"), inline = TRUE), style = "font-size: 30px;") ,
              showcase = fct_cow_icon(col='white'),
              # theme_color = 'success',
              height='150px'
            ),
            bslib::value_box(
              title = "Durations replaced (neg residual):",
              value = p(textOutput(ns("n_neg"), inline = TRUE), style = "font-size: 30px;") ,
              showcase = bsicons::bs_icon('arrow-repeat'),
              theme_color = 'info',
              height='150px'
            ),
            bslib::value_box(
              title = "Negative intakes set to 0 kg:",
              value = p(textOutput(ns("n_neg_intake"), inline = TRUE), style = "font-size: 30px;") ,
              showcase = bsicons::bs_icon('arrow-repeat'),
              theme_color = 'info',
              height='150px'
            ),
            bslib::value_box(
              title = "Intakes replaced (pos residual):",
              value = p(textOutput(ns("n_pos"), inline = TRUE), style = "font-size: 30px;") ,
              showcase = bsicons::bs_icon('arrow-repeat'),
              theme_color = 'info',
              height='150px'
            )
          )
        )
    )
  )
  )
}

#' by_cow_clean Server Functions
#'
#' @noRd
mod_by_cow_clean_server <- function(id, df_list){
  moduleServer( id, function(input, output, session){
    ns <- session$ns



    df <- reactive({
      df_list()$df_cleaned
    })


    observe({

      shinyjs::disable(id = 'execute_detect_outliers')

      if(tibble::is_tibble(df())){
        shinyjs::enable(id = 'execute_detect_outliers')
      }
    })




    ##################################################################### #
    # Execute Outlier detection ----
    # Returns a nested df with a col of cow_id_nest which is a copy of cow_id
    # then 2x list columns, where the column fitted contains each individual
    # dataframe outputted by f_flag_and_replace_outliers()
    ##################################################################### #

    list_outlier_detection <-
      reactive({
        shinybusy::show_modal_progress_line(text = "Outlier detection running...")

        list_out <- f_iterate_cows(df(),
                                   col_cow_id = cow_id,
                                   col_bin_id = feed_bin_id,
                                   col_date = date,
                                   col_start_time = start_time,
                                   col_intake =  corrected_intake_bybin,
                                   col_duration = corrected_feed_duration_seconds,
                                   sd_thresh = input$sd_threshold, # default = 5
                                   shiny.session = session,
                                   log = TRUE
        )


        shinybusy::remove_modal_progress()

        return(list_out)

      }) %>% bindEvent(input$execute_detect_outliers)



    # Split list output into required components to use here:
    df_outliers_nested_by_cow <- reactive({
      list_outlier_detection()$nested_out
    })

    df_summary_outliers_by_cow <- reactive({ # used for valueBoxes
      list_outlier_detection()$outlier_summary
    })

    # create merged data - unnested - returned in list at end for other modules
    df_outliers_merged <- reactive({ f_merge_corrected_outlier_data(df_outliers_nested_by_cow()) })


    ################################################################# #
    # Dynamic display: ----
    ################################################################# #

    # setup a reactive value -- used for changing ui output based on events later
    print_out <- reactiveValues(glimpse_nested = 1, glimpse_merged = 1)

    # set up reactivevalues list to store log data (from logr in `f_by_bin_clean`)
    log <- reactiveValues()

    # observe table to override output to show in window:
    observe({
      print_out$glimpse_nested <- tibble::as_tibble(df_outliers_nested_by_cow())
      print_out$glimpse_merged <- tibble::as_tibble(df_outliers_merged())

      # read in log file to print to glimpse window:
      log$by_cow <- readLines(list_outlier_detection()$log_path)

    })

    # dynamically change display:
    output$dynamic_glimpse <- renderPrint({
      if(tibble::is_tibble(print_out$glimpse_nested)){
        # print log file, then a glimpse of data:
        cat(paste(log$by_cow, collapse = "\n"))
        cat("\n")
        dplyr::glimpse(print_out$glimpse_nested)
        cat("\n")
        dplyr::glimpse(print_out$glimpse_merged)

      }
    })


    ################################################################# #
    # Value Boxes: ----
    ################################################################# #

    output$n_not_error <- renderText({

      req(list_outlier_detection())

      fct_check_for_valuebox_cow(
        df_summary_outliers_by_cow(),
        .data$outlier_pos_neg,
        "not_error"
      )
    })

    output$n_neg <- renderText({

      req(list_outlier_detection())

      fct_check_for_valuebox_cow(
        df_summary_outliers_by_cow(),
        .data$outlier_pos_neg,
        "neg"
      )
    })


    # end weight errors
    output$n_neg_intake <- renderText({

      req(list_outlier_detection())

      fct_check_for_valuebox_cow(
        df_summary_outliers_by_cow(),
        .data$outlier_pos_neg,
        "neg_intake"
      )
    })


    output$n_pos <- renderText({

      req(list_outlier_detection())

      fct_check_for_valuebox_cow(
        df_summary_outliers_by_cow(),
        .data$outlier_pos_neg,
        "pos"
      )
    })



    ################################################################# #
    # Buttons: ----
    ################################################################# #
    # advance section for SD threshold:
    shinyjs::onclick("toggleAdvanced", shinyjs::toggle(id = "sd_threshold", anim = TRUE))

    # Hide download buttons until after cleaning
    observe({
      shinyjs::toggleState("download_nested", condition = input$execute_detect_outliers > 0)
      shinyjs::toggleState("download_merged", condition = input$execute_detect_outliers > 0)
    })

    ################ #
    # Detailed Overview
    ################ #
    observe({

      fct_show_custom_modal(fct_modal_content_bycow_more_info(), title = "More about the 'By Cow' data cleaning")

    }) %>% bindEvent({ input$button_more_info })

    ################ #
    # Docs for Functions
    ################ #

    observe({ fct_show_modal_bin_functions('f_outlier_func', output, session) }) %>% bindEvent({input$f_outlier_func})



    ################################################################# #
    # Downloads : ----
    ################################################################# #
    output$download_nested <- downloadHandler(
      filename = function() {
        paste0("by_cow_nested_data_out_",
               format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      },
      content = function(file) {
        saveRDS(df_outliers_nested_by_cow(), file = file)
      }
    )

    output$download_merged <- downloadHandler(
      filename = function() {
        paste0("by_cow_outlier_detection_out",
               format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        #saveRDS(df_outliers_nested_by_cow(), file = file)
        data.table::fwrite(df_outliers_merged(), file = file)
      },
      contentType = "text/csv"
    )





    return(reactive({ list(nested_df = df_outliers_nested_by_cow(),
                           merged_df = df_outliers_merged(),
                           log_path = list_outlier_detection()$log_path)
    }))
  })
}

## To be copied in the UI
# mod_by_cow_clean_ui("by_cow_clean_1")

## To be copied in the server
# mod_by_cow_clean_server("by_cow_clean_1")
