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
            "1px    0.2fr       0.8fr",
            "1fr    user_input  Display "
          ),
          alternate_layouts = list(
            layout = c(
              "1px       1fr     ",
              "400px    user_input ",
              "800px    Display"
            ),
            width_bounds = c(max = 900)
          )),

          ############################################### #
          # Inputs panel ----
          ################################################ #
          gridlayout:: grid_card(
            area = "user_input",

            wrapper = function(x) bslib::card_body(x, fill=FALSE, fillable=TRUE, class = "align-items-top"),

            h3("Downloads:"),

            shinyWidgets::treeInput(
              inputId = ns('selected_error_types_by_bin'),
              label = "Select corrections to customise final intake and duration values, then press button to calculate values ready for download.",
              returnValue = 'text',
              selected = 'By Bin',
              choices = shinyWidgets::create_tree(
                data.frame(
                  Step = c("By Bin", "By Bin", "By Bin"),
                  Error = c("correct end weight", "correct start weight", "correct overlapping end times"),
                  stringsAsFactors = FALSE
                ),
                levels = c('Step', 'Error')
              )
            ),

            shinyWidgets::treeInput(
              inputId = ns('selected_error_types_by_cow'),
              label = NULL,
              returnValue = 'text',
              selected = 'By Cow',
              choices = shinyWidgets::create_tree(
                data.frame(
                  Step = c( "By Cow", "By Cow"),
                  Error = c("replace duration outliers", "replace intake outliers"),
                  stringsAsFactors = FALSE
                ),
                levels = c('Step', 'Error')
              )
            ),

            actionButton(ns('recalculate_values'), label = "Calculate final values", class = "btn-lg btn-success")


          ),



          ############################################### #
          # Display panel ----
          ################################################ #

          gridlayout:: grid_card(
            area = "Display",
            wrapper = bslib::card_body(fillable = FALSE, fill = TRUE),
            full_screen = FALSE,

            bslib::navset_tab( # a card with nav tabs:
              id = ns('display_tabs'), #used for input$ to see active tab

               # wrapper = function(x) bslib::card_body(x, fillable = FALSE, fill = TRUE),

              bslib::nav_panel(
                title = "Download - all events",
                value = "full_download", #for accessing input$ details

                bslib::card(
                  bslib::card_body(
                    fillable=FALSE,
                    strong("Cleaned data"),
                    br(),
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

                      br(),

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
                    br(),
                    downloadButton(ns("download_full"), "Cleaned data - all columns", class = 'btn-info btn-fixed-size-summary'),

                    p(strong(em("Summarised data is available to view and download in the other tabs on this page."))),
                    br(),


                    strong("Logs"),
                    p("It is recommended that the log files are downloaded with the data.
              These will be especially useful for reporting the proportion
              of data that is removed before downstream analyses and publications."),

              downloadButton(ns('download_logs'), label = 'Log files (as single .txt file)', class = 'btn-info btn-fixed-size-summary'),
              br(),

              strong("Preview of simplified and full data (after clicking 'calculate final values'):"),
              verbatimTextOutput(outputId = ns('glimpse_out'), placeholder = TRUE)
                  )

                )


              ),

              bslib::nav_panel(
                title = 'All daily intakes',
                bslib::card(
                  bslib::card_title("Summarised Data"),
                  p("
              These summary tabs have a table that has download buttons at the
              top that will copy to clipboard or download as a CSV or Excel
              file. It is common to calculate a daily feed intake for each
              cow, which is the sum of all feeding events for a day. This is
              calculated based on the check boxes selected. In addition, a DM
              % can be added in the box which will add a column to the table
              with the calculated DM DM intakes.
              "),
              # br(),

              bslib::layout_columns(
                fill=FALSE,
                fillable=FALSE,

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

              bslib::card(
                full_screen = TRUE,
                shiny::plotOutput(outputId = ns('violin_plot_intake'), height = '400px') %>%
                  shinycssloaders::withSpinner(type=7)

              ),

              bslib::card(
                full_screen = TRUE,
                min_height = '500px',
                DT::DTOutput(ns('daily_intake_table')) %>%  shinycssloaders::withSpinner(type=7)

              )
                )


              ),

              bslib::nav_panel(
                title = 'Individual daily intakes',

                bslib::card(

                p("
              Daily feed intake from individual cows can be visualised here. Multiple cows can be selected, each given their own colour on the plot.
              "),
              # br(),

              bslib::layout_columns(
                fill=FALSE,
                fillable=FALSE,

                row_heights = 1,
                col_widths = c(5,2,-5), # negative means empty cols
                gap="4px",

                shinyWidgets::pickerInput(
                  inputId = ns("animal_id_ind_plots"),
                  label = "Select cow/s to plot",
                  choices = NULL,
                  options = list(
                    `actions-box` = TRUE),
                  multiple = TRUE
                ),
                actionButton(ns("generate_ind_plot"), label = "Update Plot", class = "btn-success btn-vertical-center")

              ),
              # br(),

              bslib::card(
                full_screen = TRUE,
                plotly::plotlyOutput(outputId = ns("plot_ind_cows"), height = "600px") %>%  shinycssloaders::withSpinner()
              )

                )

              ),




              bslib::nav_panel(
                title = 'Rate of intake',
                bslib::card(
                  bslib::card_title("Mean rate of intake across all events for each cow"),
                # br(),
                bslib::card(
                  shiny::plotOutput(outputId = ns('rate_histogram'), height = '350px') %>%  shinycssloaders::withSpinner(type=7),
                  # br(),
                  DT::DTOutput(ns('intake_rate_table')) %>%  shinycssloaders::withSpinner(type=7)
                )
                )

              ),

              bslib::nav_panel(
                title = 'Feed Duration',
                bslib::card(
                  bslib::card_title("Mean feed duration across all events for each cow"),
                # br(),
                bslib::card(
                  shiny::plotOutput(outputId = ns('duration_histogram'), height = '350px') %>%  shinycssloaders::withSpinner(type=7),
                  br(),
                  DT::DTOutput(ns('duration_table')) %>%  shinycssloaders::withSpinner(type=7)
                )
                )

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
#' @param df_list_bycow List; output from the `by_cow - clean` module. Required for final data frames and log filepath.
#'
#' @noRd
mod_final_summary_server <- function(id, df_list_bybin, df_list_bycow){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ############################################## #
    # Prepare final data based on user input ----
    ############################################## #

    observe({
      # Error handler
      # If any from 'By Cow' are selected, then it must use 'by bin'
      if(any(c("replace duration outliers", "replace intake outliers") %in% input$selected_error_types_by_cow)){
        # make sure all of 'by bin' are selected, with warning:
        if( ! all(c("correct end weight", "correct start weight", "correct overlapping end times") %in% input$selected_error_types_by_bin)){
          # Currently this isn't possible. Show warning and reset by bin

          shinyWidgets::show_alert(title = "By Cow must use By Bin", text = "Currently 'By Cow' outlier detection and cleaning defaults to using all cleaning methods from 'By Bin' step.
                                   This can be changed by running codes manually in R, but will be added to app in future. Therefore, all of 'By Bin' has been re-selected.
                                   To customise 'By Bin', deselect 'By Cow' first.
                                   ",
                                   type = 'error',
                                   showCloseButton = TRUE)

          shinyWidgets::updateTreeInput('selected_error_types_by_bin', selected = 'By Bin')

        }
      }

    })

    final_data_full <-   reactive({

      # If keep all corrections:
      if(all('By Bin' %in% input$selected_error_types_by_bin & 'By Cow' %in% input$selected_error_types_by_cow)){

        df_out <- df_list_bycow()$merged_df %>%
          # remove prev and next columns
          dplyr::select( !tidyselect::starts_with(c("prev", "next"))) %>%
          dplyr::mutate(
            selected_final_intake_kg = .data$final_intake_kg,
            selected_final_duration_sec = .data$final_duration_sec
          )

      }  else {
        # return the data based on user input
        df_list_bycow()$merged_df %>%
          dplyr::mutate(
            selected_final_intake_kg = dplyr::case_when(
              "replace intake outliers" %in% input$selected_error_types_by_cow ~ new_y,
              all(c('correct end weight', 'correct start weight') %in% input$selected_error_types_by_bin) ~ corrected_intake_bybin,
              'correct end weight' %in% input$selected_error_types_by_bin ~ (start_weight_kg - corrected_end_weight_kg_bybin),
              'correct start weight' %in% input$selected_error_types_by_bin ~ (corrected_start_weight_kg_bybin - end_weight_kg),
              TRUE ~ intake
            ),
            selected_final_duration_sec = dplyr::case_when(
              "replace duration outliers" %in% input$selected_error_types_by_cow ~ new_x,
              "correct overlapping end times" %in% input$selected_error_types_by_bin ~ corrected_duration_sec_seconds,
              TRUE ~ duration_sec
            )
          ) %>%
          dplyr::select( !tidyselect::starts_with(c("prev", "next")))

      }

    }) %>% bindEvent(input$recalculate_values)




    # Create simplified version of data with less columns
    simplified_final_df <- reactive({
      req(final_data_full())

      # prepare simplified data
        final_data_full() %>%
        dplyr::select(
          "bin_id",
          "animal_id",
          "date",
          original_start_time = "start_time",
          original_end_time = "end_time",
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
      final_data_full() %>% dplyr::glimpse()
      })





    ################################# #
    # Download data ----
    ################################# #
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

        df_simplified <- simplified_final_df()

        # save:
        if(input$download_filetype_selection == '.rds'){
          saveRDS(df_simplified, file = file)

        } else if(input$download_filetype_selection %in% c('.csv', '.txt')){
          data.table::fwrite(df_simplified, file = file)
        }

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

        # save:
        if(input$download_filetype_selection == '.rds'){
          saveRDS(final_data_full(), file = file)

        } else if(input$download_filetype_selection %in% c('.csv', '.txt')){
          data.table::fwrite(final_data_full(), file = file)
        }

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
          readLines(df_list_bycow()$log_path)
          )


        # Write the combined content to the output file
        writeLines(combined_logs, con = file)
      }
    )



    ################################# #
    # Daily intakes  ----
    ################################# #

    df_daily_intakes <- reactive({

      final_data_full() %>%
        dplyr::group_by(.data$animal_id, .data$date) %>%
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

    output$plot_ind_cows <- plotly::renderPlotly({
      req(!is.null(df_daily_intakes()))


      p <- df_daily_intakes() %>%
        dplyr::filter(.data$animal_id %in% input$animal_id_ind_plots) %>%
        dplyr::mutate('animal_id' = as.character(.data$animal_id)) %>%
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
    # Rate of intake  ----
    ################################# #

    summary_table <-  reactive({
      final_data_full() %>%
        dplyr::mutate(
          selected_slop_kg_sec = .data$selected_final_intake_kg / .data$selected_final_duration_sec
        ) %>%
        dplyr::group_by(.data$animal_id) %>%
        dplyr::summarise(selected_slope_kg_sec = mean(.data$selected_slop_kg_sec , na.rm=TRUE),
                         selected_final_duration_sec = mean(.data$selected_final_duration_sec, na.rm=TRUE)) %>%
        dplyr::mutate(selected_slope_g_min = .data$selected_slope_kg_sec*1000*60 %>% round(2),
                      selected_final_duration_min = .data$selected_final_duration_sec/60 %>% round(2))
    })


    output$rate_histogram <- renderPlot({
      req(!is.null(df_list_bycow()))

      mean_rate <- summary_table()$selected_slope_g_min %>%
        mean(na.rm=TRUE) %>%
        round(2)

      summary_table() %>%
        ggplot2::ggplot(aes(x = .data$selected_slope_g_min)) +
        ggplot2::geom_histogram(bins = 50, fill ='darkgreen',colour = 'grey') +  # Themes, text size = 12 and black
        ggplot2::geom_vline(aes(xintercept = mean_rate))+
        ggplot2::geom_label(aes(x = mean_rate, y = 3, label = paste('mean:', mean_rate)))+
        ggplot2::theme_classic(base_size = 16) +
        ggplot2::theme(axis.text = ggplot2::element_text(colour = 'black')) +
        ggplot2::scale_x_continuous(n.breaks = 10)
    })


    output$intake_rate_table <- DT::renderDT({
      summary_table() %>%
        dplyr::select('animal_id', 'selected_slope_g_min') %>%
        dplyr::mutate(selected_slope_g_min = .data$selected_slope_g_min %>% round(1)) %>%
        dplyr::arrange(dplyr::desc(.data$selected_slope_g_min)) %>%
        fct_DT_nopages(scrollY = 300)
    })

    ################################# #
    # Duration summary  ----
    # similar functions with user selection?
    ################################# #

    output$duration_histogram <- renderPlot({
      req(!is.null(df_list_bycow()))

      mean_duration <- summary_table()$selected_final_duration_min %>%
        mean(na.rm=TRUE) %>%
        round(2)

      summary_table() %>%
        ggplot2::ggplot(aes(x = .data$selected_final_duration_min)) +
        ggplot2::geom_histogram(bins = 50, fill ='darkgreen',colour = 'grey') +  # Themes, text size = 12 and black
        ggplot2::geom_vline(aes(xintercept = mean_duration))+
        ggplot2::geom_label(aes(x = mean_duration, y = 3, label = paste('mean:', mean_duration)))+
        ggplot2::theme_classic(base_size = 16) +
        ggplot2::theme(axis.text = ggplot2::element_text(colour = 'black')) +
        ggplot2::scale_x_continuous(n.breaks = 10)
    })


    output$duration_table <- DT::renderDT({
      summary_table() %>%
        dplyr::select('animal_id', 'selected_final_duration_min') %>%
        dplyr::mutate(selected_final_duration_min = .data$selected_final_duration_min %>% round(2)) %>%
        dplyr::arrange(dplyr::desc(.data$selected_final_duration_min)) %>%
        fct_DT_nopages(scrollY = 300)
    })


  })
}

## To be copied in the UI
# mod_final_summary_ui("final_summary_1")

## To be copied in the server
# mod_final_summary_server("final_summary_1")
