#' final_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_final_summary_ui <- function(id){
  ns <- NS(id)


  bslib::nav_panel(
    title = "Final Summary",

    div(class = 'resized-tab-container',
        gridlayout::grid_container(

          layout = gridlayout::new_gridlayout(c(
            "1px  1fr",
            "1fr  Display "
          ),
          alternate_layouts = list(
            layout = c(
              "1px       1fr     ",
              "800px    Display"
            ),
            width_bounds = c(max = 900)
          )),

          ############################################### #
          # Inputs panel ----
          ################################################ #
          # gridlayout:: grid_card(
          #   area = "user_input",
          #   wrapper = function(x) bslib::card_body(x, fill=FALSE, fillable=TRUE, class = "align-items-top"),
          #   br()
          #   # h3("Downloads:"),
          #   # p("Select which outliers from 'By Animal' should be replaced in the final data.
          #   #   The previously selected 'By Bin' corrections are also displayed below.
          #   #   Click 'Prepare Final Data' will calculate 'final' values in the simplified table for download, but all possible combinations of corrections are available in the full data."),
          #   #
          #   # htmlOutput(ns('display_bybin_cols'), inline = TRUE),
          #   #
          #   # shinyWidgets::treeInput(
          #   #   inputId = ns('selected_error_types_by_animal'),
          #   #   label = NULL,
          #   #   returnValue = 'text',
          #   #   selected = 'By Animal Corrections',
          #   #   choices = shinyWidgets::create_tree(
          #   #     data.frame(
          #   #       Step = c( "By Animal Corrections", "By Animal Corrections"),
          #   #       Error = c("replace duration outliers", "replace intake outliers"),
          #   #       stringsAsFactors = FALSE
          #   #     ),
          #   #     levels = c('Step', 'Error')
          #   #   )
          #   # ),
          #   #
          #   # actionButton(ns('recalculate_values'), label = "Prepare Final Data", class = "btn-lg btn-success")
          #
          #
          # ),


          ############################################### #
          # Display panel ----
          ################################################ #

          gridlayout::grid_card(
            area = "Display",
            wrapper = bslib::card_body(class = "p-0", fillable = TRUE, fill = TRUE, padding = 0), # this removes the padding around edges
            full_screen = FALSE,
            bslib::layout_sidebar(
              padding = '3px',
              sidebar = bslib::sidebar(
                id = ns('final_summary_sidebar'),
                width = 280, open = TRUE, padding = 10,

                h3("Downloads:"),
                p("Select which outliers from 'By Animal' should be replaced in the final data.
                The previously selected 'By Bin' corrections are also displayed below.
                Click 'Prepare Final Data' will calculate 'final' values in the simplified table for download, but all possible combinations of corrections are available in the full data."),

                htmlOutput(ns('display_bybin_cols'), inline = TRUE),

                shinyWidgets::treeInput(
                  inputId = ns('selected_error_types_by_animal'),
                  label = NULL,
                  returnValue = 'text',
                  selected = 'By Animal Corrections',
                  choices = shinyWidgets::create_tree(
                    data.frame(
                      Step = c( "By Animal Corrections", "By Animal Corrections"),
                      Error = c("replace duration outliers", "replace intake outliers"),
                      stringsAsFactors = FALSE
                    ),
                    levels = c('Step', 'Error')
                  )
                ),

                shinyTime::timeInput(ns("timeInput"), label ="Time to start a new 'day' for daily summaries ('HH:MM'):",  value = "00:00:00", seconds = FALSE),

                actionButton(ns('recalculate_values'), label = "Prepare Final Data", class = "btn-lg btn-success")


              ),
              bslib::navset_card_tab( # a card with nav tabs:
                id = ns('display_tabs'), #used for input$ to see active tab
                # ############
                # # Shared Sidebar
                # ############

                bslib::nav_panel(
                  title = "Download - all events",
                  value = "full_download", #for accessing input$ details
                  bslib::card_title("Cleaned data"),
                  p(
                    "The final datasets is available for download using the buttons below."),
                  p("The simplified version
                      gives the minimum information to differentiate each event and
                      uses the cleaned/modified durations (sec) and intakes (as-fed
                      kg) as selected."),
                  p("The full data frame can also be downloaded, including all
                      columns added throughout the cleaning (except the copies of
                      previous and next row data). This would be particularly useful
                      for examining the different errors that are flagged and setting
                      up custom rules/filters. The functions in this package's source
                      code can also be modified directly, allowing users to run
                      modified versions of this dashboard."),

                  shinyWidgets::radioGroupButtons(
                    inputId = ns("download_filetype_selection"),
                    label = "Select format for downloads:",
                    individual = TRUE,
                    choices = c(".rds", ".csv", ".txt"),
                    selected = ".csv",
                    status = "success" # equive to  class = 'btn-success'
                  ),
                  br(),

                  downloadButton(ns("download_simplified"), "Cleaned data - simplified", class = 'btn-info btn-fixed-size-summary'),
                  downloadButton(ns("download_full"), "Cleaned data - all columns", class = 'btn-info btn-fixed-size-summary'),

                  em("Summarised data is available to view and download in the other tabs on this page."),

                  strong("Preview of simplified and full data structure (after clicking 'calculate final values'):"),
                  verbatimTextOutput(outputId = ns('glimpse_out'), placeholder = TRUE),
                ),


                bslib::nav_panel(
                  title = "Logs",
                  value = 'logs',
                  bslib::card_title("Logs"),
                  p("
                      It is recommended that the log files are downloaded with the data. These will be especially useful for reporting the proportion of data that is removed before downstream analyses and publications.
                      "),
                  downloadButton(ns('download_logs'), label = 'Log files (as single .txt file)', class = 'btn-info btn-fixed-size-summary'),
                  br(),
                  strong("Log for 'calculate final values':"),
                  verbatimTextOutput(ns("final_log"), placeholder = TRUE)
                ),
                bslib::nav_spacer(),
                bslib::nav_panel(
                  title = 'All daily intakes',
                  bslib::card_title("Summarised Data"),
                  p("
              These summary tabs have a table that has download buttons at the
              top that will copy to clipboard or download as a CSV or Excel
              file. It is common to calculate a daily feed intake for each
              animal, which is the sum of all feeding events for a day. This is
              calculated based on the check boxes selected. In addition, a DM
              % can be added in the box which will add a column to the table
              with the calculated DM DM intakes.
              "),
              # br(),

              bslib::layout_columns(
                fill=TRUE,
                fillable=TRUE,

                row_heights = 1,
                col_widths = c(4,2,-6), # negative means empty cols
                gap="4px",

                span("Enter Dry Matter % to calculate DM intakes (100% is equal to as-fed values):",class = "align-right"),
                numericInput(inputId = ns("DM_perc"),
                             value = 100,
                             label = NULL,
                             min = 0,
                             max = 100,
                             step = 5)
              ),
              br(),

              bslib::card(
                full_screen = TRUE,
                min_height = '420px',
                shiny::plotOutput(outputId = ns('violin_plot_intake'), height = '400px') %>%
                  shinycssloaders::withSpinner(type=7),

              ),
              #
              bslib::card(
                full_screen = TRUE,
                min_height = '500px',
                DT::DTOutput(ns('daily_intake_table')) %>%  shinycssloaders::withSpinner(type=7)

              )
                ),

              bslib::nav_panel(
                title = 'Individual daily intakes',
                p("
                  Daily feed intake from individual animals can be visualised here. Multiple animals can be selected, each given their own colour on the plot.
                "),
                # br(),

                bslib::layout_columns(
                  fill=FALSE,
                  fillable=TRUE,

                  row_heights = 1,
                  col_widths = c(5,-1,2,-4), # negative means empty cols
                  gap="4px",

                  shinyWidgets::pickerInput(
                    inputId = ns("animal_id_ind_plots"),
                    label = "Select animal/s to plot",
                    choices = NULL,
                    options = list(
                      `actions-box` = TRUE),
                    multiple = TRUE
                  ),
                  actionButton(ns("generate_ind_plot"), label = "Update Plot", class = "btn-success btn-vertical-center")
                ),

                bslib::card(
                  full_screen = TRUE,
                  min_height = '620px',
                  plotly::plotlyOutput(outputId = ns("plot_ind_animals"), height = "600px") %>%  shinycssloaders::withSpinner()
                )
              ),

              # bslib::nav_panel(
              #   title = 'Rate of intake',
              #   bslib::card_title("Mean rate of intake across all events for each animal"),
              #   # br(),
              #   bslib::card(
              #     shiny::plotOutput(outputId = ns('rate_histogram'), height = '350px') %>%  shinycssloaders::withSpinner(type=7),
              #     # br(),
              #     DT::DTOutput(ns('intake_rate_table')) %>%  shinycssloaders::withSpinner(type=7)
              #   )
              # ),
              #
              # bslib::nav_panel(
              #   title = 'Feed Duration',
              #   bslib::card_title("Mean feed duration across all events for each animal"),
              #   # br(),
              #   bslib::card(
              #     shiny::plotOutput(outputId = ns('duration_histogram'), height = '350px') %>%  shinycssloaders::withSpinner(type=7),
              #     br(),
              #     DT::DTOutput(ns('duration_table')) %>%  shinycssloaders::withSpinner(type=7)
              #   )
              # ),

              ########################################################
              bslib::nav_spacer(),
              bslib::nav_panel(
                title = 'Feeding Behaviour',

                bslib::card_body(
                  class = "p-0",
                  fillable = TRUE, # CHECK
                  fill = TRUE,

                  div(id = "feeding-behaviour-navset", # this links to custom CSS

                      bslib::navset_card_pill( # a card with nav tabs:
                        id = ns('behaviour-navset'), #used for input$ to see active tab
                        full_screen = TRUE,
                        bslib::nav_panel(
                          title = "Intro",
                          value = "display_intro", #for accessing input$ details
                          # bslib::card_title(""),
                          em("Hover mouse over bottom right of screen to show button to expand view. Use Esc or click Close to return to normal screen."),

                          h4("1. Click button to calculate"),
                          em("Must 'Prepare Final Data' in sidebar first."),
                          shiny::numericInput(ns('meal_criterion_min'), label = 'Meal Criterion (min) - How many minutes between feeding events before next meal is defined?', value = 5, min = 0, step = 1),
                          actionButton(ns("execute_behaviour_analysis"), "Execute Behaviour Analysis", class = " btn-success btn-fixed-size-summary"),

                          h4("2. View & Download"),
                          p("View data in subsequent tabs and use buttons below to download data."),
                          shinyWidgets::radioGroupButtons(
                            inputId = ns("download_filetype_selection_behaviour"),
                            label = "Select format for downloads:",
                            individual = TRUE,
                            choices = c(".rds", ".csv", ".txt"),
                            selected = ".csv",
                            status = "success"
                          ),

                          downloadButton(ns("download_daily"), "Daily Behaviours", class = 'btn-info btn-fixed-size-summary'),
                          downloadButton(ns("download_weekly"), "Weekly Behaviours", class = 'btn-info btn-fixed-size-summary'),
                          downloadButton(ns("download_raw_meals"), "Individual Meals", class = 'btn-info btn-fixed-size-summary'),
                        ),


                        bslib::nav_panel(
                          title = "Daily",
                          value = "display_table_daily", #for accessing input$ details
                          bslib::card_title("Feeding Behaviours grouped by animal and day"),
                          DT::DTOutput(ns('daily_behaviour_DT')) %>%  shinycssloaders::withSpinner(type=7)
                        ),
                        bslib::nav_panel(
                          title = "Weekly",
                          value = "display_table_weekly", #for accessing input$ details
                          bslib::card_title("Feeding Behaviours grouped by animal and week"),
                          DT::DTOutput(ns('weekly_behaviour_DT')) %>%  shinycssloaders::withSpinner(type=7)
                        )
                      )
                  )
                )

              )
              ########################################################
              )
            )

          )
        )
    )
  )


}

#' final_summary Server Functions
#'
#' @param id module id
#' @param df_list_bybin List; output from the `by_bin - clean` module. Required for log filepath.
#' @param df_list_byanimal List; output from the `by_animal - clean` module. Required for final data frames and log filepath.
#'
#' @noRd
mod_final_summary_server <- function(id, df_list_bybin, df_list_byanimal){
  moduleServer( id, function(input, output, session){
    ns <- session$ns



    ############################################## #
    # Prepare final data based on user input ----
    ############################################## #
    thresholds <- reactive({
      #for reporting, not used in calcs
      return(list(
        zero_threshold = df_list_bybin()$user_bybin_thresholds$zero_thresh,
        feedout_threshold = df_list_bybin()$user_bybin_thresholds$feedout_thresh,
        sd_threshold = df_list_byanimal()$user_inputs_to_parse_to_vis$sd_threshold,
        max_duration_min = df_list_byanimal()$user_inputs_to_parse_to_vis$max_duration_min,
        min_intake_rate_kg_min = df_list_byanimal()$user_inputs_to_parse_to_vis$min_intake_rate_kg_min,
        max_intake_rate_kg_min = df_list_byanimal()$user_inputs_to_parse_to_vis$max_intake_rate_kg_min,
        outlier_exemption_max_duration = df_list_byanimal()$user_inputs_to_parse_to_vis$outlier_exemption_max_duration,
        outlier_exemption_max_intake = df_list_byanimal()$user_inputs_to_parse_to_vis$outlier_exemption_max_intake
      ))
    })

    bybin_col_selections <- reactive({
      col_intake <- df_list_byanimal()$user_inputs_to_parse_to_vis$selected_intake_column
      col_duration <- df_list_byanimal()$user_inputs_to_parse_to_vis$selected_duration_column
      return(list(
        col_intake = col_intake,
        col_duration = col_duration
      ))
    })

    # Display the columns that were selected prior to executing 'by animal'
    output$display_bybin_cols <- renderText({
      paste("<b>'By Bin' selected Intake:</b>",bybin_col_selections()$col_intake,
            "<br><br>", "<b>'By Bin' selected Duration:</b>", bybin_col_selections()$col_duration, "<br>")
    })

    # store character strings of final selections, to help with final code reproduce
    list_final_selections <-
      reactiveValues(user_selected_final_intake = NA_character_,
                     user_selected_final_duration = NA_character_)


    final_data_full <-   reactive({

      custom_start_time <- hms::as_hms(input$timeInput) # Convert to hms object

      #log
      tmp <- tempfile(pattern = "log_", fileext = ".log")
      logr::log_open(tmp, logdir = FALSE, show_notes = FALSE)
      logr::sep(" Preparing final data outputs ")



      # If keep all corrections:
      if(all('corrected_intake_bybin' %in% bybin_col_selections()$col_intake &
             'corrected_duration_sec' %in% bybin_col_selections()$col_duration &
             'By Animal Corrections' %in% input$selected_error_types_by_animal)){

        logr::log_print("All cleaning accepted from 'By Bin' and 'By Animal'.")
        logr::log_print("'selected_final_intake_kg' and 'selected_final_duration_sec' incorporate replaced values from 'By Bin' and all outliers corrected from 'By Animal'.")

        list_final_selections$user_selected_final_intake <- 'final_intake_kg'
        list_final_selections$user_selected_final_duration <- 'final_duration_sec'

        df_out <- df_list_byanimal()$merged_df %>%
          # remove prev and next columns
          dplyr::select( !tidyselect::starts_with(c("prev", "next"))) %>%
          dplyr::mutate(
            selected_final_intake_kg = .data$final_intake_kg,
            selected_final_duration_sec = .data$final_duration_sec
          )

      }  else {
        # Select final intake, with logging
        if("replace intake outliers" %in% input$selected_error_types_by_animal){
          logr::log_print("'By Animal' Outliers are re-estimated in final intakes.")
          logr::log_print(paste("`selected_final_intake_kg` is `new_y` column"))
          list_final_selections$user_selected_final_intake <- 'new_y'

          df_out_intake <- df_list_byanimal()$merged_df %>%
            dplyr::mutate(
              selected_final_intake_kg = .data$new_y
              )

        } else{
          logr::log_print("'By Animal' Outliers are ignored in final intakes.")
          logr::log_print(paste("`selected_final_intake_kg` is", bybin_col_selections()$col_intake, "from 'By Bin' cleaning only."))
          list_final_selections$user_selected_final_intake <- bybin_col_selections()$col_intake

          df_out_intake <-df_list_byanimal()$merged_df %>%
            dplyr::mutate(
              selected_final_intake_kg = !!rlang::sym(bybin_col_selections()$col_intake)
            )
        }

        # Select final duration, with logging
        if("replace duration outliers" %in% input$selected_error_types_by_animal){
          logr::log_print("'By Animal' Outliers are re-estimated in final durations.")
          logr::log_print(paste("`selected_final_duration_sec` is `new_x` column"))
          list_final_selections$user_selected_final_duration <- 'new_x'

          df_out <- df_out_intake %>%
            dplyr::mutate(
              selected_final_duration_sec = .data$new_x
            )

        } else{
          logr::log_print("'By Animal' Outliers are ignored in final durations.")
          logr::log_print(paste("`selected_final_duration_sec` is", bybin_col_selections()$col_duration, "from 'By Bin' cleaning only."))
          list_final_selections$user_selected_final_duration <- bybin_col_selections()$col_duration

          df_out <- df_out_intake %>%
            dplyr::mutate(
              selected_final_duration_sec = !!rlang::sym(bybin_col_selections()$col_duration)
            )
        }

      }

      # Add custom day calculation if custom_start_time is not 00:00
      if (custom_start_time != hms::as_hms("00:00:00")) {
        logr::log_print(paste("Custom start time used for defining days:", custom_start_time))
        df_out <- df_out %>%
          dplyr::mutate(datetime = as.POSIXct( start_time, format = "%Y-%m-%d %H:%M:%S")) %>%
          dplyr::mutate(filter_date_daily = dplyr::if_else(lubridate::hour(datetime) >= lubridate::hour(custom_start_time),
                                      lubridate::date(datetime),
                                      lubridate::date(datetime) - lubridate::days(1))) %>%
          dplyr::select(-datetime)
      } else {
        df_out <- df_out %>%
          dplyr::mutate(
            filter_date_daily = date
          )
      }


      # Add times with formatting for easier use with Excel
      df_out <- df_out %>%
        dplyr::mutate(
          start_time_simple = format(start_time, "%H:%M:%S"),
          end_time_simple = format(end_time, "%H:%M:%S")
        ) %>%
        dplyr::relocate(
          tidyselect::ends_with("_simple"),
          .after = .data$end_time
        )


      logr::sep("R Code to reproduce analysis")
      logr::log_print("Copy all code below into a .R or .Rmd file to execute:")
      logr::log_print("# START ###################################################################################")

      logr::log_print(paste0("zero_threshold = ", thresholds()$zero_threshold), blank_after = FALSE)
      logr::log_print(paste0("feedout_threshold = ", thresholds()$feedout_threshold), blank_after = FALSE)


      logr::log_print(paste0("user_selected_intake_col_bybin = '", bybin_col_selections()$col_intake, "'"), blank_after = FALSE)
      logr::log_print(paste0("user_selected_duration_col_bybin = '", bybin_col_selections()$col_duration, "'"), blank_after = TRUE)


      logr::log_print(paste0("user_sd_threshold = ", thresholds()$sd_threshold), blank_after = FALSE)
      logr::log_print(paste0("user_max_duration_min = ", thresholds()$max_duration_min), blank_after = FALSE)
      logr::log_print(paste0("user_min_intake_rate_kg_min = ", thresholds()$min_intake_rate_kg_min), blank_after = FALSE)
      logr::log_print(paste0("user_max_intake_rate_kg_min = ", thresholds()$max_intake_rate_kg_min), blank_after = FALSE)
      logr::log_print(paste0("user_outlier_exemption_max_duration = ", thresholds()$outlier_exemption_max_duration), blank_after = FALSE)
      logr::log_print(paste0("user_outlier_exemption_max_intake = ", thresholds()$outlier_exemption_max_intake), blank_after = TRUE)

      logr::log_print(paste0("user_selected_final_duration = '", list_final_selections$user_selected_final_duration, "'"),blank_after = FALSE)
      logr::log_print(paste0("user_selected_final_intake = '", list_final_selections$user_selected_final_intake, "'"))

      # Implementation of logr::log_code() that reads in another R file instead of the current file.
      lns <- readLines(con = app_sys("app/www/template_reproduceable_analysis.R"), encoding = "UTF-8")
      f <- file(logr::log_path(), "a", encoding = "native.enc")
      writeLines(lns, con = f, useBytes = TRUE)
      close(f)

      # Finish Log ----
      logr::log_close()
      shinybusy::notify_success("Final Data Prepared", position = 'right-bottom')

      # close side bar
      bslib::toggle_sidebar(id = 'final_summary_sidebar')

      return(list(
        df = df_out  %>%  dplyr::select( !tidyselect::starts_with(c("prev", "next"))),
        log_path = tmp))

    }) %>% bindEvent(input$recalculate_values)


    output$final_log <- renderPrint({
      # read in log file to print to glimpse window:
      log_final <- readLines(final_data_full()$log_path)
      cat(paste(log_final, collapse = "\n"))

    })


    # Create simplified version of data with less columns
    simplified_final_df <- reactive({
      req(final_data_full()$df)

      # prepare simplified data
        final_data_full()$df %>%
        dplyr::select(
          "bin_id",
          "animal_id",
          "date",
          original_start_time = "start_time_simple",
          original_end_time = "end_time_simple",
          original_duration_sec = "duration_sec",
          original_start_weight_kg = "start_weight_kg",
          original_end_weight_kg = "end_weight_kg",
          original_intake = "intake",

          "selected_final_intake_kg",
          "selected_final_duration_sec"

        ) %>%
        dplyr::mutate(
          is_modified_intake = !dplyr::near(.data$selected_final_intake_kg, .data$original_intake),
          is_modified_duration = !dplyr::near(.data$selected_final_duration_sec, .data$original_duration_sec),
        )
    })



    output$glimpse_out <- renderPrint({
      print("Simplified data:")
      simplified_final_df() %>% dplyr::glimpse()
      print("--------------------------------------------------------------")
      print("Full data:")
      final_data_full()$df %>% dplyr::glimpse()
      })





    ################################# #
    # Download data ----
    ################################# #

    .f_download_filetype_helper <- function(file, df){
      # save:
      if(input$download_filetype_selection_behaviour == '.rds'){
        saveRDS(df, file = file)

      } else if(input$download_filetype_selection_behaviour %in% c('.csv', '.txt')){
        data.table::fwrite(df, file = file)
      }
    }

    # Hide download buttons until after button pressed
    observe({
      shinyjs::toggleState("download_simplified", condition = input$recalculate_values > 0)
      shinyjs::toggleState("download_full", condition = input$recalculate_values > 0)
    })

    # select minimal columns to export for simplified version
    # Renamed some columns
    output$download_simplified <- downloadHandler(
      filename = function() {
        paste0("IntakeInspectR_cleaned_data_simplified_",
               format(Sys.time(), "%Y%m%d_%H%M%S"),
               input$download_filetype_selection)
      },
      content = function(file) {
        .f_download_filetype_helper(file, simplified_final_df())
      }
    )

    # Download full dataset
    output$download_full <- downloadHandler(
      filename = function() {
        paste0("IntakeInspectR_cleaned_data_full_",
               format(Sys.time(), "%Y%m%d_%H%M%S"),
               input$download_filetype_selection)
      },
      content = function(file) {
        .f_download_filetype_helper(file, final_data_full()$df)
      }
    )

    # combine log files and save as .txt file
    output$download_logs <- downloadHandler(
      filename = function(){
        paste0("IntakeInspectR_logs_",
               format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")

      },
      content = function(file){
        # Combine the log contents into a single vector
        combined_logs<- c(
          readLines(df_list_bybin()$log_path),
          readLines(df_list_byanimal()$log_path),
          readLines(final_data_full()$log_path)
          )


        # Write the combined content to the output file
        writeLines(combined_logs, con = file)
      }
    )



    ################################# #
    # Daily intakes  ----
    ################################# #

    df_daily_intakes <- reactive({

      final_data_full()$df %>%
        dplyr::group_by(.data$animal_id, .data$filter_date_daily) %>%
        # summarise data using either raw or cleaned data:
        dplyr::summarise(
          `Selected as-fed intake kg/d` = sum(.data$selected_final_intake_kg, na.rm=TRUE),
          `Selected feed duration sec/d` = sum( .data$selected_final_duration_sec,  na.rm=TRUE)
          ) %>%
        # clean up data:
        dplyr::mutate(
          `Selected feed duration min/d` = .data$`Selected feed duration sec/d`/60,
          dplyr::across(tidyselect::where(is.numeric), ~round(.x, 1))
        )

    })

    # Violin plot
    output$violin_plot_intake  <-  renderPlot({
      req(df_daily_intakes())

      df_daily_intakes() %>%
        dplyr::mutate(date = .data$filter_date_daily) %>%
        ggplot(aes(x = .data$date, y =.data$`Selected as-fed intake kg/d`, group = .data$date))+
        ggplot2::geom_violin(colour = 'darkorange')+
        ggplot2::geom_jitter(alpha = 0.5, width = 0.2)+
        ggplot2::scale_x_date(date_breaks = 'day')+
        ggplot2::theme_classic(base_size = 13)+
        ggplot2::ggtitle("Distributions of feed intake (as-fed kg) per d")
    })

    # Daily intakes table
    output$daily_intake_table <- DT::renderDT({
      # If user enters a value other than 100% for DM, then add a column with DM intake
      if(input$DM_perc != 100){
        df_daily_intakes() %>%
          dplyr::mutate(
            `DM Intake kg/d` = .data$`Selected as-fed intake kg/d` * input$DM_perc/100
          ) %>%
          fct_DT_nopages(scrollY = 500, fillContainer = FALSE)
      }else {
        df_daily_intakes() %>%
          fct_DT_nopages(scrollY = 500, fillContainer = FALSE)
      }
    })

    ################################# #
    # Individual daily intakes  ----
    ################################# #


    observe({
      vec_bins <- df_daily_intakes()   %>%
        dplyr::select("animal_id") %>%
        dplyr::distinct(.keep_all = TRUE) %>%
        dplyr::arrange(.data$animal_id) %>%
        dplyr::pull(.data$animal_id)

      freezeReactiveValue(input, 'animal_id_ind_plots') # doesn't evaluate on load when no data available
      shinyWidgets::updatePickerInput(session = session, inputId = 'animal_id_ind_plots', choices = vec_bins, selected = vec_bins[1:4])
    })

    output$plot_ind_animals <- plotly::renderPlotly({
      req(!is.null(df_daily_intakes()))


      p <- df_daily_intakes() %>%
        dplyr::filter(.data$animal_id %in% input$animal_id_ind_plots) %>%
        dplyr::mutate(
          'animal_id' = as.character(.data$animal_id),
          date = .data$filter_date_daily) %>%
        ggplot2::ggplot(aes( x = .data$date, y = .data$`Selected as-fed intake kg/d`, colour = .data$animal_id))+
        ggplot2::geom_point(size = 1.5)+
        ggplot2::geom_line()+
        ggplot2::facet_wrap(facets = "animal_id")+
        ggplot2::ylim(c(0,NA))+
        ggplot2::scale_x_date(date_breaks = 'day')+
        ggplot2::scale_colour_viridis_d(option = 'H', begin = 0, end = 0.95, na.value = 'red')+
        ggplot2::theme_classic(base_size = 13)+
        ggplot2::ggtitle("Individual feed intake (as-fed kg) per day")

      plotly::ggplotly(p, dynamicTicks = TRUE) %>% f_change_legend_on_resize()

    }) %>% bindEvent(input$generate_ind_plot, ignoreNULL=FALSE)


     ################################# #
    # Feeding Behaviour ----
    ################################# #
    individual_meals <-
      reactive({
        shinybusy::show_modal_spinner(spin = 'orbit', text =  "Calculating Behaviour Metrics...")

        f_combined_meal_analysis(
          df_in = final_data_full()$df,
          meal_interval_criteria_min = input$meal_criterion_min,
        )
      }) %>% bindEvent(input$execute_behaviour_analysis)

    daily_behaviour <-
      reactive({
        daily_summary <- f_behaviour_daily_summary(
          df_in = final_data_full()$df
        )

        daily_meal_summary <- f_daily_meal_summaries(
          df_in = individual_meals()
          )

        combined_daily_summary <- f_final_daily_summary(
          daily_summary,
          daily_meal_summary
        ) %>%
          dplyr::mutate(animal_id = as.factor(.data$animal_id))
        return(combined_daily_summary)
      }) %>% bindEvent(input$execute_behaviour_analysis)

    weekly_behaviour <-
      reactive({
        df_out <- f_behaviour_summary_weekly(
          daily_behaviour()
        )

        shinybusy::remove_modal_spinner()
        shinybusy::notify_success("Behaviours Calculated", position = 'right-bottom')

        return(df_out)
      }) %>% bindEvent(input$execute_behaviour_analysis)

    # observe to force execution
    observe({
      individual_meals()
      daily_behaviour()
      weekly_behaviour()
    })


    # Render Tables
    output$daily_behaviour_DT <- DT::renderDT({
      daily_behaviour() %>%
        dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~signif(.x, 3))) %>%
        fct_DT_pages(pageLength = 20, scrollY = 400)
    })

    output$weekly_behaviour_DT <- DT::renderDT({
      weekly_behaviour() %>%
        dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~signif(.x, 3))) %>%
        fct_DT_pages(pageLength = 20, scrollY = 400)
    })


    # disable button until available
    observe({
      shinyjs::disable(id = 'execute_behaviour_analysis')

      if(tibble::is_tibble(final_data_full()$df)){
        shinyjs::enable(id = 'execute_behaviour_analysis')
      }
    })




    ################################# #
    # Download Behaviour data ----
    ################################# #
    # Hide download buttons until after button pressed
    # Hide download buttons until after cleaning
    observe({
      shinyjs::toggleState("download_daily", condition = input$execute_behaviour_analysis > 0)
      shinyjs::toggleState("download_weekly", condition = input$execute_behaviour_analysis > 0)
      shinyjs::toggleState("download_raw_meals", condition = input$execute_behaviour_analysis > 0)
    })


    # Daily
    output$download_daily <- downloadHandler(
      filename = function() {
        paste0("daily_behaviours_", format(Sys.time(), "%Y%m%d_%H%M%S"),
               input$download_filetype_selection_behaviour)
      },
      content = function(file) {
        .f_download_filetype_helper(file, daily_behaviour())
      }
    )

    # Weekly
    output$download_weekly <- downloadHandler(
      filename = function() {
        paste0("weekly_behaviours_", format(Sys.time(), "%Y%m%d_%H%M%S"),
               input$download_filetype_selection_behaviour)
      },
      content = function(file) {
        .f_download_filetype_helper(file, weekly_behaviour())
      }
    )

    # Raw meals
    output$download_raw_meals <- downloadHandler(
      filename = function() {
        paste0("individual_meal_behaviours_", format(Sys.time(), "%Y%m%d_%H%M%S"),
               input$download_filetype_selection_behaviour)
      },
      content = function(file) {
        .f_download_filetype_helper(file, individual_meals())
      }
    )


    ################################# #
    # Rate of intake  ----
    ################################# #
#
#     summary_table <-  reactive({
#       final_data_full()$df %>%
#         dplyr::mutate(
#           instaneous_rate_of_intake_kg_sec_ = .data$selected_final_intake_kg / .data$selected_final_duration_sec
#         ) %>%
#         dplyr::group_by(.data$animal_id) %>%
#         dplyr::summarise(instaneous_rate_of_intake_kg_sec = mean(.data$instaneous_rate_of_intake_kg_sec_ , na.rm=TRUE),
#                          selected_final_duration_sec = mean(.data$selected_final_duration_sec, na.rm=TRUE)) %>%
#         dplyr::mutate(instaneous_rate_of_intake_g_min = .data$instaneous_rate_of_intake_kg_sec*1000*60 %>% round(2),
#                       selected_final_duration_min = .data$selected_final_duration_sec/60 %>% round(2))
#     })
#
#
#     output$rate_histogram <- renderPlot({
#       req(!is.null(df_list_byanimal()))
#
#       mean_rate <- summary_table()$instaneous_rate_of_intake_g_min %>%
#         mean(na.rm=TRUE) %>%
#         round(2)
#
#       summary_table() %>%
#         ggplot2::ggplot(aes(x = .data$instaneous_rate_of_intake_g_min)) +
#         ggplot2::geom_histogram(bins = 50, fill ='darkgreen',colour = 'grey') +  # Themes, text size = 12 and black
#         ggplot2::geom_vline(aes(xintercept = mean_rate))+
#         ggplot2::geom_label(aes(x = mean_rate, y = 3, label = paste('mean:', mean_rate)))+
#         ggplot2::theme_classic(base_size = 16) +
#         ggplot2::theme(axis.text = ggplot2::element_text(colour = 'black')) +
#         ggplot2::scale_x_continuous(n.breaks = 10)
#     })
#
#
#     output$intake_rate_table <- DT::renderDT({
#       summary_table() %>%
#         dplyr::select('animal_id', 'instaneous_rate_of_intake_g_min') %>%
#         dplyr::mutate(instaneous_rate_of_intake_g_min = .data$instaneous_rate_of_intake_g_min %>% round(1)) %>%
#         dplyr::arrange(dplyr::desc(.data$instaneous_rate_of_intake_g_min)) %>%
#         fct_DT_nopages(scrollY = 300)
#     })

    ################################# #
    # Duration summary  ----
    # similar functions with user selection?
    ################################# #
#
#     output$duration_histogram <- renderPlot({
#       req(!is.null(df_list_byanimal()))
#
#       mean_duration <- summary_table()$selected_final_duration_min %>%
#         mean(na.rm=TRUE) %>%
#         round(2)
#
#       summary_table() %>%
#         ggplot2::ggplot(aes(x = .data$selected_final_duration_min)) +
#         ggplot2::geom_histogram(bins = 50, fill ='darkgreen',colour = 'grey') +  # Themes, text size = 12 and black
#         ggplot2::geom_vline(aes(xintercept = mean_duration))+
#         ggplot2::geom_label(aes(x = mean_duration, y = 3, label = paste('mean:', mean_duration)))+
#         ggplot2::theme_classic(base_size = 16) +
#         ggplot2::theme(axis.text = ggplot2::element_text(colour = 'black')) +
#         ggplot2::scale_x_continuous(n.breaks = 10)
#     })
#
#
#     output$duration_table <- DT::renderDT({
#       summary_table() %>%
#         dplyr::select('animal_id', 'selected_final_duration_min') %>%
#         dplyr::mutate(selected_final_duration_min = .data$selected_final_duration_min %>% round(2)) %>%
#         dplyr::arrange(dplyr::desc(.data$selected_final_duration_min)) %>%
#         fct_DT_nopages(scrollY = 300)
#     })


  })
}

## To be copied in the UI
# mod_final_summary_ui("final_summary_1")

## To be copied in the server
# mod_final_summary_server("final_summary_1")
