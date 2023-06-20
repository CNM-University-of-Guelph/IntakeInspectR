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
          layout = c(
            "user_input Display"
          ),
          row_sizes = c(
            "1fr"
          ),
          col_sizes = c(
            "0.5fr",
            "1.5fr"
          ),

          gap_size = "5px",

          ############################################### #
          # Inputs panel ----
          ################################################ #
          gridlayout::grid_card(
            area = 'user_input',
            wrapper = bslib::card_body,
            h3("Downloads:"),
            shinyWidgets::radioGroupButtons(
              inputId = ns("download_filetype_selection"),
              label = "Select format for downloads:",
              individual = TRUE,
              choices = c(".rds", ".csv", ".txt"),
              selected = ".csv",
              status = "success" # equive to  class = 'btn-success'
            ),
            br(),
            br(),
            #gap = '20px',
            downloadButton(ns("download_simplified"), "Cleaned data - simplified", class = 'btn-info'),
            br(),
            downloadButton(ns("download_full"), "Cleaned data - all columns", class = 'btn-info'),
            br(),
            downloadButton(ns('download_logs'), label = 'Log files (as single .txt file)', class = 'btn-info'),
            br(),
            p("Summarised data is available in the tabs to the right. These can be downloaded using the buttons above each table.")
          ),

          ############################################### #
          # Display panel ----
          ################################################ #

          gridlayout:: grid_card(
            area = "Display",
            wrapper = bslib::card_body(class = "p-0",fillable = FALSE, fill = FALSE), # this removes the padding around edges
            full_screen = FALSE,

            bslib::navset_card_tab( # a card with nav tabs:
              id = ns('display_tabs'), #used for input$ to see active tab
              wrapper = function(x) bslib::card_body(x, fillable = FALSE, fill = FALSE),

              bslib::nav_panel(
                title = "Overview",
                value = "overview", #for accessing input$ details
                bslib::card_title("Summarised Data"),
                p(
                  "Different mean values of interest are shown in the following
              tabs to give a quick overview of the data. Each tab has a table
              that has download buttons at the top that will copy to clipboard
              or download as a CSV or Excel file. It is common to calculate a
              daily feed intake for each cow, which is the sum of all feeding
              events for a day. This is offered with both the original intake
              values or the cleaned/modified intake values. In addition, you
              can enter a single DM % that will add a column with the as-fed
              intakes converted to DM intakes. "),
              br(),

              h5("Downloads"),
              strong("Cleaned data"),
              p(
                "The final datasets, after all cleaning steps, is available for
              download using the buttons to the left. The simplified version
              gives the minimum information to differentiate each event and
              uses the cleaned/modified durations (sec) and intakes (as-fed
              kg). The full data frame can also be downloaded, including all
              columns added throughout the cleaning (except the copies of
              previous and next row data). This would be particularly useful
              for examining the different errors that are flagged and setting
              up custom rules/filters. The functions in this package's source
              code can also be modified directly, allowing users to run
              modified versions of this dashboard."),

              strong("Logs"),
              p("It is recommended that the log files are downloaded with the data.
              These will be especially useful for reporting the proportion
              of data that is removed before downstream analyses and publications.")

              ),
              bslib::nav_panel(
                title = 'Rate of intake',
                bslib::card_title("Mean rate of intake across all events for each cow"),
                br(),
                bslib::card(
                  shiny::plotOutput(outputId = ns('rate_histogram'), height = '350px') %>%  shinycssloaders::withSpinner(type=7),
                  br(),
                  DT::DTOutput(ns('intake_rate_table')) %>%  shinycssloaders::withSpinner(type=7)
                )
              ),

              bslib::nav_panel(
                title = 'Feed Duration',
                bslib::card_title("Mean feed duration across all events for each cow"),
                br(),
                bslib::card(
                  shiny::plotOutput(outputId = ns('duration_histogram'), height = '350px') %>%  shinycssloaders::withSpinner(type=7),
                  br(),
                  DT::DTOutput(ns('duration_table')) %>%  shinycssloaders::withSpinner(type=7)
                )
              ),

              bslib::nav_panel(
                title = 'Daily intake',

                bslib::layout_columns(
                  fill=FALSE,
                  fillable=FALSE,

                  row_heights = 1,
                  col_widths = c(4,-2,4,2), # negative means empty cols
                  gap="4px",

                  shinyWidgets::switchInput(inputId = ns("use_clean_intakes_d"),
                                            value = TRUE,
                                            onLabel = "Clean", # TRUE
                                            offLabel = "Raw", # FALSE
                                            label = "Use cleaned data?",
                                            labelWidth = '250px'),
                  span("Enter Dry Matter % to calculate DM intakes (100% is equal to as-fed values):",class = "align-right"),
                  numericInput(inputId = ns("DM_perc"),
                               value = 100,
                               label = NULL,
                               min = 0,
                               max = 100,
                               step = 5)
                ),

                shiny::plotOutput(outputId = ns('violin_plot_intake'), height = '400px') %>%  shinycssloaders::withSpinner(type=7),
                bslib::card(
                  full_screen = TRUE,
                  min_height = '500px',
                  DT::DTOutput(ns('daily_intake_table')) %>%  shinycssloaders::withSpinner(type=7)

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


    # df_list_in is a list with either nested_df or merged_df from output of mod_by_cow

    ################################# #
    # Download data ----
    ################################# #


    # add overall flag for if any changes made to event:
    final_merged_df <- reactive({
      df_list_bycow()$merged_df %>%
        # remove prev and next columns
        dplyr::select( !tidyselect::starts_with(c("prev", "next"))) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          # overall flag for if anything was modified in the event
          is_modified = any(.data$is_corrected_intake_bybin, .data$is_end_time_error, .data$is_outlier, na.rm=TRUE)
        ) %>%
        dplyr::ungroup()
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
        # prepare simplified data
        df_simplified <-
          final_merged_df() %>%
          dplyr::select(
            "cow_id", "feed_bin_id",  "start_time" ,
            "end_time" = "corrected_end_time",
            "duration_seconds" = "final_duration_sec",
            "intake_kg" = "final_intake_kg" ,
            "is_modified"
          )

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
          saveRDS(final_merged_df(), file = file)

        } else if(input$download_filetype_selection %in% c('.csv', '.txt')){
          data.table::fwrite(final_merged_df(), file = file)
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
    # Rate of intake  ----
    ################################# #

    summary_table <-  reactive({
      df_list_bycow()$merged_df %>%
        dplyr::group_by(.data$cow_id) %>%
        dplyr::summarise(slope_kg_sec = mean(.data$slope, na.rm=TRUE),
                         feed_duration_sec = mean(.data$final_duration_sec, na.rm=TRUE)) %>%
        dplyr::mutate(slope_g_min = .data$slope_kg_sec*1000*60 %>% round(2),
                      feed_duration_min = .data$feed_duration_sec/60 %>% round(2))
    })


    output$rate_histogram <- renderPlot({
      req(!is.null(df_list_bycow()))

      mean_rate <- summary_table()$slope_g_min %>%
        mean(na.rm=TRUE) %>%
        round(2)

      summary_table() %>%
        ggplot2::ggplot(aes(x = .data$slope_g_min)) +
        ggplot2::geom_histogram(bins = 50, fill ='darkgreen',colour = 'grey') +  # Themes, text size = 12 and black
        ggplot2::geom_vline(aes(xintercept = mean_rate))+
        ggplot2::geom_label(aes(x = mean_rate, y = 3, label = paste('mean:', mean_rate)))+
        ggplot2::theme_classic(base_size = 16) +
        ggplot2::theme(axis.text = ggplot2::element_text(colour = 'black')) +
        ggplot2::scale_x_continuous(n.breaks = 10)
    })


    output$intake_rate_table <- DT::renderDT({
      summary_table() %>%
        dplyr::select('cow_id', 'slope_g_min') %>%
        dplyr::mutate(slope_g_min = .data$slope_g_min %>% round(1)) %>%
        dplyr::arrange(dplyr::desc(.data$slope_g_min)) %>%
        fct_DT_nopages(scrollY = 300)
    })

    ################################# #
    # Duration summary  ----
    # similar functions with user selection?
    ################################# #

    output$duration_histogram <- renderPlot({
      req(!is.null(df_list_bycow()))

      mean_duration <- summary_table()$feed_duration_min %>%
        mean(na.rm=TRUE) %>%
        round(2)

      summary_table() %>%
        ggplot2::ggplot(aes(x = .data$feed_duration_min)) +
        ggplot2::geom_histogram(bins = 50, fill ='darkgreen',colour = 'grey') +  # Themes, text size = 12 and black
        ggplot2::geom_vline(aes(xintercept = mean_duration))+
        ggplot2::geom_label(aes(x = mean_duration, y = 3, label = paste('mean:', mean_duration)))+
        ggplot2::theme_classic(base_size = 16) +
        ggplot2::theme(axis.text = ggplot2::element_text(colour = 'black')) +
        ggplot2::scale_x_continuous(n.breaks = 10)
    })


    output$duration_table <- DT::renderDT({
      summary_table() %>%
        dplyr::select('cow_id', 'feed_duration_min') %>%
        dplyr::mutate(feed_duration_min = .data$feed_duration_min %>% round(2)) %>%
       dplyr::arrange(dplyr::desc(.data$feed_duration_min)) %>%
        fct_DT_nopages(scrollY = 300)
    })


    ################################# #
    # Daily intakes  ----
    ################################# #

    df_daily_intakes <- reactive({
      col_intake <- dplyr::if_else(input$use_clean_intakes_d,
                                   true = 'final_intake_kg',
                                   false = 'intake')
      col_duration <- dplyr::if_else(input$use_clean_intakes_d,
                                     true = 'final_duration_sec',
                                     false = 'feed_duration')

      df_list_bycow()$merged_df %>%
        dplyr::group_by(.data$cow_id, .data$date) %>%
        # summarise data using either raw or cleaned data:
        dplyr::summarise(
          `As-fed Intake kg/d` = sum(.data[[col_intake]], na.rm=TRUE),
          `Feed duration sec/d` = sum( .data[[col_duration]],  na.rm=TRUE)
          )%>%
        # clean up data:
        dplyr::mutate(
          `Feed duration min/d` = .data$`Feed duration sec/d`/60,
          dplyr::across(tidyselect::where(is.numeric), ~round(.x, 1))
        )

    })

    # Violin plot
    output$violin_plot_intake  <-  renderPlot({
      req(df_daily_intakes())

      df_daily_intakes() %>%
        ggplot(aes(x = .data$date, y =.data$`As-fed Intake kg/d`, group = .data$date))+
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
            `DM Intake kg/d` = .data$`As-fed Intake kg/d` * input$DM_perc/100
          ) %>%
          fct_DT_nopages(scrollY = 500, fillContainer = FALSE)
      }else {
        df_daily_intakes() %>%
          fct_DT_nopages(scrollY = 500, fillContainer = FALSE)
      }
    })

  })
}

## To be copied in the UI
# mod_final_summary_ui("final_summary_1")

## To be copied in the server
# mod_final_summary_server("final_summary_1")
