#' by_animal_clean UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_by_animal_clean_ui <- function(id){
  ns <- NS(id)

  # tabPanel(
  bslib::nav_panel(
    title = "3a. By Animal - Clean",

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

            shinyWidgets::treeInput(
              inputId = ns('selected_error_types_by_bin'),
              label = "Select corrections from 'By Bin' cleaning that should be used in downstream analysis.
              If any are not selected, the corresponding columns from the original uploaded file will be used.",
              returnValue = 'text',
              selected = 'By Bin Corrections',
              choices = shinyWidgets::create_tree(
                data.frame(
                  Step = c("By Bin Corrections", "By Bin Corrections", "By Bin Corrections"),
                  Error = c("end weight", "start weight", "overlapping end times (long durations)"),
                  stringsAsFactors = FALSE
                ),
                levels = c('Step', 'Error')
              )
            ),

            actionButton(ns('execute_detect_outliers'), label = "Execute Outlier Detection", class = "btn-lg btn-success"),

            checkboxInput(ns("verbose"), "Verbose (show more detail in log)", value = FALSE),

            actionButton(ns("button_more_info"), "Detailed overview of outlier detection"),
            br(),

            bslib::accordion(
              id = ns('accordian'), # must have ID to work
              open = FALSE,
              bslib::accordion_panel(
                title = "Advanced: Set Outlier Thresholds",
                strong("Manual Outliers - biologically relevant limits"),
                p("Outlier Exemption means that values with a duration and intake <= the following values are not classified as outliers or modified. This region of exempt values is represented on the plot with a red box."),

                numericInput(inputId = ns("outlier_exemption_max_duration"),
                             label = "Enter outlier exemption maximum duration (minutes):",
                             value = 1,
                             min = 0,
                             step = 1),

                numericInput(inputId = ns("outlier_exemption_max_intake"),
                             label = "Enter outlier exemption maximum intake (kg):",
                             value = 0.2,
                             min = 0,
                             step = 0.01),

                p("Manual outliers are based on the user entered min and max rate of intake (kg/min).
                  These are for individual events and should be more extreme than what is 'normal' for an animal.
                  Events with a rate of intake < min specified will have a new duration estimated from linear model."),

                numericInput(inputId = ns("min_intake_rate_kg_min"),
                             label = "Enter minimum intake rate (kg/min):",
                             value = 0.05,
                             min = 0,
                             step = 0.01),

                p('Events with a rate of intake > than the max specified will have a new intake estimated from linear model.'),
                numericInput(inputId = ns("max_intake_rate_kg_min"),
                             label = "Enter maximum intake rate (kg/min):",
                             value = 1.5,
                             min = 0,
                             step = 0.1),

                p("Events not caught by min rate of intake that are > this max duration will also get a new duration estimated."),
                numericInput(inputId = ns("max_duration_min"),
                             label = "Enter maximum duration (minutes):",
                             value = 60,
                             min = 0,
                             step = 1),


                strong("Statistical outlier based on linear model"),
                p("This should be used to flag unusual outliers not caught by biologically relevant thresholds above.
                  Please check visualisations to ensure it is not too stringent. Set to a very high number to bypass. "),
                numericInput(inputId = ns("sd_threshold"),
                             label = "Enter residual SD threshold:",
                             value = 20,
                             min = 1,
                             max = Inf,
                             step = 1),



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
                  These files are useful for importing into R. See the View Table to view merged table.
                  The nested output is the file returned by", tags$code("f_flag_and_replace_outliers()"), "before it is merged for easier viewing.
                  "),
                  downloadButton(ns("download_nested"), "Download raw nested output (.rds)", class = 'btn-info'),
                  br(),
                  downloadButton(ns("download_merged"), "Download unnested data frame (.rds)", class = 'btn-info'),
                  br(),
                  downloadButton(ns("download_merged_csv"), "Download unnested data frame (.csv)", class = 'btn-info')
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
                bslib::card_title("Clean data - grouped by animal ID"),

                p("
                  This step of the cleaning identifies outliers based on user defined max
                  and min rates of intake (kg/min). Then, further outliers can be flagged
                  by fitting a robust linear model to the duration vs intake data for each animal.
                  All outliers are temporarily removed to fit a bisector regression which is used to estimate
                  a new duration or intake value for each outlier.
                  "),

                em("Hover mouse over bottom right of screen to show button to expand view. Use Esc or click Close to return to normal screen."),

                h4("Quick Start"),
                tags$div(
                  tags$ol(
                    tags$li(tags$strong("Select columns:"), " Click the categories of 'By Bin Corrections' in the side panel to choose which corrections should be used for 'By Animal' cleaning."),
                    tags$li(
                      tags$strong("Data Corrections:"),
                      " If both 'end weight' and 'start weight' corrections are selected, the previously calculated 'corrected_intake_bybin' column is utilised.
                      If only one is chosen, the intake will be recalculated, and a new column will be added named 'corrected_intake_bybin_startweight' or 'corrected_intake_bybin_endweight'.
                      Note that more detailed selection of individual corrections can be achieved by calling IntakeInspectR's functions directly in R. Refer to the README on GitHub for details."
                    ),

                    tags$li(
                      tags$strong("Execute:"),
                      " Click the 'Execute Outlier Detection' button to initiate the cleaning process. This process uses predefined outlier thresholds set in the 'Advanced: Set Outlier Thresholds' dropdown menu."
                    ),
                    tags$li(
                      tags$strong("Visualise:"),
                      " Evaluate the cleaning process by navigating to the '3b. By Animal - Vis' page. "
                    ),
                    tags$li(
                      tags$strong("Revise and repeat:"),
                      " Adjust thresholds (under 'Advanced: Set Outlier Thresholds') and re-execute until the outlier detection appears reasonable in the visualisations. Take note of what is biologically possible, but be careful to not remove real variation in the data."
                    )
                  )
                )


              ),

              bslib::nav_panel(
                title = "Log",
                value = "display_log", #for accessing input$ details
                bslib::card_title("Log - 'By Animal' cleaning"),
                p("This log can be copied from this box or downloaded with all other logs on Final Summary tab at end of analysis."),
                verbatimTextOutput(ns("dynamic_log"), placeholder=TRUE),
              ),

              bslib::nav_panel(
                title = "Data Structure",
                value = "display_structure", #for accessing input$ details
                bslib::card_title("Data Structure"),
                p("This shows the column names and dimensions of the data frame returned by the by animal cleaning function.
                The MERGED file is the result of merging the nested data frame that is returned by the", tags$code("f_flag_and_replace_outliers()"), " function.
                See 'Download function outputs' for more details.
                "),
                verbatimTextOutput(ns("dynamic_glimpse"), placeholder=TRUE),
              ),

              bslib::nav_panel(
                title = "View Table",
                value = "display_table", #for accessing input$ details
                bslib::card_title("View Table"),
                p("The data frame returned from the cleaning table can be explored here. Use the buttons to download as file/s. "),
                DT::DTOutput(ns('byanimal_DT')) %>%  shinycssloaders::withSpinner(type=7)
              )
            )
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
                showcase = fct_animal_icon(col='white'),
                theme = 'primary',
                height='150px'
              ),
              bslib::value_box(
                title = "Negative intakes set to 0 kg:",
                value = p(textOutput(ns("n_neg_intake"), inline = TRUE), style = "font-size: 30px;") ,
                showcase = bsicons::bs_icon('arrow-repeat'),
                theme = 'info',
                height='150px'
              ),
              bslib::value_box(
                title = "Durations replaced (duration too long):",
                value = p(textOutput(ns("n_neg"), inline = TRUE), style = "font-size: 30px;") ,
                showcase = bsicons::bs_icon('arrow-repeat'),
                theme = 'info',
                height='150px'
              ),

              bslib::value_box(
                title = "Intakes replaced (intake too high):",
                value = p(textOutput(ns("n_pos"), inline = TRUE), style = "font-size: 30px;") ,
                showcase = bsicons::bs_icon('arrow-repeat'),
                # showcase_layout = bslib::showcase_left_center(width = 0.2),
                theme = 'info',
                height='150px'
              )
            )
          )
        )
    )
  )
}

#' by_animal_clean Server Functions
#'
#' @noRd
mod_by_animal_clean_server <- function(id, df_list){
  moduleServer( id, function(input, output, session){
    ns <- session$ns



    df <- reactive({
      df_list()$df_list_bybin$df_cleaned
    })


    observe({

      shinyjs::disable(id = 'execute_detect_outliers')

      if(tibble::is_tibble(df())){
        shinyjs::enable(id = 'execute_detect_outliers')
      }
    })


    observe({
      if(df_list()$is_bybin_bypass){
        shinyWidgets::updateTreeInput('selected_error_types_by_bin', selected = "")
        shinyjs::hide(id = "selected_error_types_by_bin")
      } else{
        shinyWidgets::updateTreeInput('selected_error_types_by_bin', selected = "By Bin Corrections")
        shinyjs::show(id = "selected_error_types_by_bin")
      }

    })




    ##################################################################### #
    # Execute Outlier detection ----
    # Returns a nested df with a col of animal_id_nest which is a copy of animal_id
    # then 2x list columns, where the column fitted contains each individual
    # dataframe outputted by f_flag_and_replace_outliers()
    ##################################################################### #

    list_outlier_detection <-
      reactive({
        shinybusy::show_modal_progress_line(text = "Outlier detection running...")

        # setup generic call for options of column selections to be used with logic below
       .execute_iterate_animals <-
          function(df_in, col_user_intake, col_duration){
            # duration selection logic:

              f_iterate_animals(df_in,
                                col_animal_id = animal_id,
                                col_bin_id = bin_id,
                                col_date = date,
                                col_start_time = start_time,
                                col_intake =  {{ col_user_intake }},
                                col_duration = {{ col_duration }},
                                max_duration_min = input$max_duration_min,
                                min_intake_rate_kg_min = input$min_intake_rate_kg_min,
                                max_intake_rate_kg_min = input$max_intake_rate_kg_min,
                                outlier_exemption_max_duration  = input$outlier_exemption_max_duration,
                                outlier_exemption_max_intake = input$outlier_exemption_max_intake,
                                sd_thresh = input$sd_threshold, # default = 20
                                shiny.session = session,
                                log = TRUE,
                                verbose = input$verbose)

          }

       # Define which 'duration' column to use, parsed to .execute_iterate_animals
        selected_duration_col <-
          if(any('overlapping end times (long durations)' %in% input$selected_error_types_by_bin)){
            'corrected_duration_sec'
          } else {
            'duration_sec'
          }

        # INTAKE SELECTION:
        #prepare data based on user selected errors,
        if(all(c('start weight', 'end weight') %in% input$selected_error_types_by_bin)){
          #Use original df() that has 'corrected_intake_bybin'
          list_out <-
            .execute_iterate_animals(
              df(),
              corrected_intake_bybin,
              !!rlang::sym(selected_duration_col)
            )


        } else if('start weight' %in% input$selected_error_types_by_bin) {

          list_out <-
            df() %>%
            .execute_iterate_animals(
              corrected_intake_bybin_startweight,
              !!rlang::sym(selected_duration_col)
              )


        } else if('end weight' %in% input$selected_error_types_by_bin) {
          list_out <-
            df() %>%
            .execute_iterate_animals(
              corrected_intake_bybin_endweight,
              !!rlang::sym(selected_duration_col)
              )

        } else(
          list_out <- .execute_iterate_animals(df(), intake, !!rlang::sym(selected_duration_col))
        )

        bslib::nav_select('display_tabs', 'display_log')

        shinybusy::remove_modal_progress()

        return(list_out)

      }) %>% bindEvent(input$execute_detect_outliers)



    # Split list output into required components to use here:
    df_outliers_nested_by_animal <- reactive({
      list_outlier_detection()$nested_out
    })

    df_summary_outliers_by_animal <- reactive({ # used for valueBoxes
      list_outlier_detection()$outlier_summary
    })

    # create merged data - unnested - returned in list at end for other modules
    df_outliers_merged <- reactive({ f_merge_corrected_outlier_data(df_outliers_nested_by_animal()) })


    ################################################################# #
    # Dynamic display: ----
    ################################################################# #

    # setup a reactive value -- used for changing ui output based on events later
    print_out <- reactiveValues(df_nested = 1, df_merged = 1)

    # set up reactivevalues list to store log data (from logr in `f_by_bin_clean`)
    log <- reactiveValues()

    # observe table to override output to show in window:
    observe({
      print_out$df_nested <- tibble::as_tibble(df_outliers_nested_by_animal())
      print_out$df_merged <- tibble::as_tibble(df_outliers_merged())

      # read in log file to print to glimpse window:
      log$by_animal <- readLines(list_outlier_detection()$log_path)

    })

    # dynamically change display:
    output$dynamic_glimpse <- renderPrint({
      if(tibble::is_tibble(print_out$df_nested)){
        cat("\nMERGED: \n")
        dplyr::glimpse(print_out$df_merged)
        cat("\nNESTED (for .rds) \n")
        dplyr::glimpse(print_out$df_nested)

      }
    })

    output$dynamic_log <- renderPrint({
      cat(paste(log$by_animal, collapse = "\n"))
    })


    # View table

    output$byanimal_DT <- DT::renderDT({
      req(input$execute_detect_outliers)
      print_out$df_merged %>%
        fct_DT_pages(
          pageLength = 20,
          scrollY = 380
        )
    })


    ################################################################# #
    # Value Boxes: ----
    ################################################################# #

    output$n_not_error <- renderText({

      req(list_outlier_detection())

      fct_check_for_valuebox_animal(
        df_summary_outliers_by_animal(),
        .data$outlier_pos_neg,
        "not_error"
      )
    })

    output$n_neg <- renderText({

      req(list_outlier_detection())

      fct_check_for_valuebox_animal(
        df_summary_outliers_by_animal(),
        .data$outlier_pos_neg,
        "neg"
      )
    })


    # end weight errors
    output$n_neg_intake <- renderText({

      req(list_outlier_detection())

      fct_check_for_valuebox_animal(
        df_summary_outliers_by_animal(),
        .data$outlier_pos_neg,
        "neg_intake"
      )
    })


    output$n_pos <- renderText({

      req(list_outlier_detection())

      fct_check_for_valuebox_animal(
        df_summary_outliers_by_animal(),
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
      shinyjs::toggleState("download_merged_csv", condition = input$execute_detect_outliers > 0)
    })

    ################ #
    # Detailed Overview
    ################ #
    observe({

      fct_show_custom_modal(fct_modal_content_byanimal_more_info(), title = "More about the 'By Animal' data cleaning")

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
        paste0("by_animal_nested_data_out_",
               format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      },
      content = function(file) {
        saveRDS(df_outliers_nested_by_animal(), file = file)
      }
    )

    output$download_merged <- downloadHandler(
      filename = function() {
        paste0("by_animal_merged_data_out_",
               format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      },
      content = function(file) {
        saveRDS(df_outliers_merged(), file = file)
      }
    )
    output$download_merged_csv <- downloadHandler(
      filename = function() {
        paste0("by_animal_outlier_detection_out",
               format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
         data.table::fwrite(df_outliers_merged(), file = file)
      },
      contentType = "text/csv"
    )

    user_inputs_to_parse_to_vis <- reactive({
      list(
        max_duration_min = input$max_duration_min,
        min_intake_rate_kg_min = input$min_intake_rate_kg_min,
        max_intake_rate_kg_min = input$max_intake_rate_kg_min,
        outlier_exemption_max_duration  = input$outlier_exemption_max_duration,
        outlier_exemption_max_intake = input$outlier_exemption_max_intake,
        sd_threshold = input$sd_threshold,
        # store user selected column names for intake and duration so it can be parsed for plotting
        selected_intake_column = list_outlier_detection()$selected_intake_column,
        selected_duration_column = list_outlier_detection()$selected_duration_column
      )

    })



    return(reactive({ list(nested_df = df_outliers_nested_by_animal(),
                           merged_df = df_outliers_merged(),
                           log_path = list_outlier_detection()$log_path,
                           user_inputs_to_parse_to_vis = user_inputs_to_parse_to_vis())
    }))
  })
}

## To be copied in the UI
# mod_by_animal_clean_ui("by_animal_clean_1")

## To be copied in the server
# mod_by_animal_clean_server("by_animal_clean_1")
