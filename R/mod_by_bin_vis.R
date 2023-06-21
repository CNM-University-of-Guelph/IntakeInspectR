#' by_bin UI Function
#'
#' @description module for displaying by_bin outputs
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'

mod_by_bin_vis_ui <- function(id){
  ns <- NS(id)

  # tabPanel(
    bslib::nav_panel(
    title = "2b. By Bin - Vis",

    div(class = 'resized-tab-container',
        gridlayout::grid_container(

          layout = gridlayout::new_gridlayout(c(
            "1px    0.5fr       1.5fr",
            "1fr    user_input  Display "
          ),
          alternate_layouts = list(
            layout = c(
              "1px       1fr     ",
              "200px    user_input ",
              "800px    Display"
            ),
            width_bounds = c(max = 900)
          )),


          ############################################### #
          # Inputs panel ----
          ################################################ #
          gridlayout::grid_card(
            area = "user_input",
            wrapper = function(x) bslib::card_body(x, fill=FALSE, fillable=FALSE),

              bslib::card_body(
                fill=FALSE,
              max_height = "300px", # so that it doesn't push the toggled UI below off the screen
              verbatimTextOutput(ns("selected_rows"), placeholder = FALSE)
              ),


              bslib::card_body(
              id = ns("toggle_plot_inputs"), # name needed for toggle to unhide
              fill=FALSE, #card won't change size inside of parent
              fillable=TRUE, #contents of card will fill out
              selectInput(
                inputId = ns('bin_id'),
                label = "Select bin:",
                # selected = 1,
                choices = NULL # This is updated by server to include vec_bins
              ),
              radioButtons(
                inputId = ns("colour_aes"),
                label = "Error column to view",
                choices = list(
                  is_end_time_overlap_error = 'is_end_time_overlap_error',
                  is_corrected_intake_bybin = 'is_corrected_intake_bybin',
                  check_end_weights = 'check_end_weights',
                  check_start_weights = 'check_start_weights'
                ),
                selected = 'check_end_weights'
              )
            ),

            # bslib::card_body(
              radioButtons(
                inputId = ns("data_type"),
                label = "Select data type:",
                choices = list(
                  raw = "raw",
                  corrected = "corrected"
                )
              ),
            br(),

              radioButtons(
                inputId = ns("plot_type_overall"),
                label = "Select plot type:",
                choices = list(
                  histogram = "hist",
                  regression = "reg"
                )
              )),


          ################################################ #
          # Display panel ----
          ################################################ #
          gridlayout::grid_card(
            area = "Display",
            wrapper = bslib::card_body(class = "p-0", fillable = FALSE, fill = TRUE), # this removes the padding around edges
            full_screen = FALSE,
            bslib::navset_card_tab( # a card with nav tabs:
              id = ns('display_tabs'), #used for input$ to see active tab
               # wrapper = bslib::card_body,

              bslib::nav_panel(
                title = "Overall Plots",
                value = "overall_plots", #for accessing input$ details
                bslib::card_title("Regression or histogram of all feeding events"),
                p("Overall visualisation of all feeding events. Select data type (raw/corrected) and plot type (histogram/regression) to view."),
                bslib::card(
                  shiny::plotOutput(outputId = ns("p"))  %>%  shinycssloaders::withSpinner(),
                  full_screen = TRUE
                )
              ),

              bslib::nav_panel(
                title = "Table: Negatives",
                value = "table_negatives", #for accessing input$ details
                bslib::card_title("Feed bins with largest negative values"),
                fct_table_description(),
                DT::DTOutput(outputId = ns("neg_table")) %>%  shinycssloaders::withSpinner()
              ),

              bslib::nav_panel(
                title = "Table: Errors",
                value = "table_errors",
                bslib::card_title("Feed bins with highest number of errors"),
                fct_table_description(),
                DT::DTOutput(outputId = ns("error_table")) %>%  shinycssloaders::withSpinner()
              ),

              bslib::nav_panel(
                title = "Plot: Duration vs Intake",
                value = 'regression', #for accessing input$ details
                p("Visualise feed duration vs intake for individual feed bins, coloured by the error type selected by user.
                  These plots are interactive, hover over points for more information,
                  click and drag to zoom, click on legend items and see menu that appears top right of plot."),
                bslib::card(
                  plotly::plotlyOutput(outputId = ns("p_duration_intake"),
                                       height = "500px") %>%  shinycssloaders::withSpinner(),
                  full_screen = TRUE
                )
              ),
              bslib::nav_panel(
                title = "Plot: Timeline",
                value = 'timeline', #for accessing input$ details
                p("Visualise the change in feed bin weight over time. An individual feeding event is drawn as a single line with dots on each end, representing the start and end of the event.
                  These plots are interactive, hover over points for more information,
                  click and drag to zoom, click on legend items and see menu that appears top right of plot."),
                bslib::card(
                  plotly::plotlyOutput(outputId = ns("p_timeline"), height = "500px") %>%  shinycssloaders::withSpinner(),
                  full_screen = TRUE
                )
              )
            )
          )
        )
    )
  )
}

#' by_bin Server Functions
#' @noRd
mod_by_bin_vis_server <- function(id, df_list){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    df <- reactive({
      df_list()$df_cleaned
    })

    ##################################################################### #
    # Overall Plots ----
    ##################################################################### #

    selectedData <- reactive({ input$data_type })

    output$p <- shiny::renderPlot({

      if(selectedData() == "raw") {

        df() %>%
          f_plot_summary(
            x = .data$feed_duration,
            y = .data$intake,
            type = input$plot_type_overall)



      }else if(selectedData() == "corrected") {
        df() %>%
          f_plot_summary(
            x = .data$corrected_feed_duration_seconds,
            y = .data$corrected_intake_bybin,
            type = input$plot_type_overall)

      }},
      #This controls resolution, or how big it appears. for renderplot
      res = 120
    )



    ##################################################################### #
    #Table ----
    ##################################################################### #
    # Create table summarising negative values by count or by total kg (of neg only)
    # create df in reactive first so that it can be accessed downstream
    df_neg <- reactive({
      df <- df() %>%
          dplyr::group_by(.data$feed_bin_id) %>%
          dplyr::summarise(
            n_neg = sum(.data$intake<0),
            sum_neg_kg = sum(.data$intake[.data$intake<0])
            ) %>%
          dplyr::arrange(dplyr::desc(.data$n_neg) ) %>%
          dplyr::mutate(sum_neg_kg = round(.data$sum_neg_kg, 2))

      colnames(df) <- c('Feed Bin ID', 'Count negative intakes', 'Sum of negative intakes (kg)' )
      return(df)
    })

    output$neg_table <- DT::renderDT({ df_neg() %>%
        fct_DT_nopages( scrollY = 400,
                        buttons_at_bottom = TRUE,
                        fillContainer = TRUE)})


    # create table to count number of errors produced for each bin
    df_error <- reactive({
       df <- df() %>%
        dplyr::group_by(.data$feed_bin_id) %>%
        dplyr::summarise(
          n_cor_weights = sum(.data$is_corrected_intake_bybin),
          n_cor_time = sum(.data$is_end_time_overlap_error),
          n_errors = .data$n_cor_weights + .data$n_cor_time ) %>%
        dplyr::arrange(dplyr::desc(.data$n_errors))

       colnames(df) <- c('Feed Bin ID', 'Count corrected weights', 'Count corrected times', 'Total errors')
       return(df)

    })

    output$error_table <-
      DT::renderDT({ df_error() %>%
          fct_DT_nopages( scrollY = 400,
                          buttons_at_bottom = TRUE,
                          fillContainer = TRUE
                          )})



    # Call back for DT table
    # https://yihui.shinyapps.io/DT-rows/
    output$selected_rows <-
      renderPrint({

        s_neg <- input$neg_table_rows_selected # DT exports row index
        s_error <- input$error_table_rows_selected

        cat("Selected bins (Table: Negatives):\n")
        if (length(s_neg)){
          # use row index to slice out rows of df, then return bin ID
          ids_neg <- df_neg() %>% dplyr::slice(s_neg) %>% dplyr::pull(.data$`Feed Bin ID`)
          cat(ids_neg, sep = "\n")
        }

        cat("\nSelected bins (Table: Errors):\n")
        if (length(s_error)){
          # use row index to slice out rows of df, then return bin ID
          ids_error <- df_error() %>% dplyr::slice(s_error) %>% dplyr::pull(.data$`Feed Bin ID`)
          cat(ids_error, sep = "\n")
        }
      })

    #################################################### #
    # Toggle visibility of different user input elements:
    #################################################### #

    # observe when the tabs are on either of the plots, and then show more details in the user_input section:
    observe({

      shinyjs::toggle(id = 'toggle_plot_inputs', anim = TRUE,
                      condition = input$display_tabs %in% c('regression', 'timeline'))

      shinyjs::toggle(id = 'data_type', anim = TRUE,
                      condition = input$display_tabs %in% c('overall_plots','regression', 'timeline'))

      shinyjs::toggle(id = 'plot_type_overall', anim = TRUE,
                      condition = input$display_tabs %in% c('overall_plots'))

    })


    ##################################################################### #
    # Visualise ----
    ##################################################################### #
    # It doesn't make sense to load a dataframe in UI as it is built only at startup
    # Therefore, we can 'update' the UI to include these values
    observe({
      vec_bins <- df() %>%
          dplyr::select("feed_bin_id") %>%
          dplyr::distinct(.keep_all = TRUE) %>%
          dplyr::arrange(.data$feed_bin_id) %>%
          dplyr::pull(.data$feed_bin_id)

      freezeReactiveValue(input, 'bin_id') # doesn't evaluate on load when no data available
      updateSelectInput(inputId = 'bin_id', choices = vec_bins)
    })

    # Filter dataframe based on selected bin and produce ggplots based on selected plot type
    # each user input should generate a new graph on-click.
    p_out <- reactive({
      # filter data
      .df <- df() %>% dplyr::filter(.data$feed_bin_id %in% input$bin_id) %>%
        # add extra data for hover tooltip
        dplyr::mutate(
          tooltip = paste('cow_id:', .data$cow_id)
        )

      if(input$data_type == "raw") {
        # Regression:
        p_duration_intake <- .df %>%
          plot_bin_regression(x = .data$feed_duration,
                                  y = .data$intake,
                                  col_colour = .data[[input$colour_aes]],
                                  col_hover_text = .data$tooltip)+ # this format because it comes in as a string
          labs(x = 'Feed Duration (seconds)',
               y = 'Feed Intake (kg)')


        # Timeline:
        p_timeline <-
          .df %>% plot_bin_timeline(
            .data$start_time,
            .data$end_time,
            .data$start_weight,
            .data$end_weight,
            col_colour = .data[[input$colour_aes]],
            col_hover_text = .data$tooltip)+
          labs(x = 'Time',
               y = 'Bin Weight (kg)')

      } else if(input$data_type == "corrected"){
        # Regression:
        p_duration_intake <- .df %>%
          plot_bin_regression( x = .data$corrected_feed_duration_seconds,
                                   y = .data$corrected_intake_bybin,
                                   col_colour =  .data[[input$colour_aes]])+
          labs(x = 'Corrected Feed Duration (seconds)',
               y = 'Corrected Feed Intake (kg)')

        # Timeline:
        p_timeline <-
          .df %>%
          plot_bin_timeline(
            .data$start_time,
            .data$corrected_end_time,
            .data$corrected_start_weight_bybin,
            .data$corrected_end_weight_bybin,
            col_colour = .data[[input$colour_aes]],
            col_hover_text = .data$tooltip)+
          labs(x = 'Time',
               y = 'Bin Weight (kg)')

      }

      return(list(p_timeline = p_timeline, p_duration_intake = p_duration_intake))
    })


    # plots
    ##PLOTLY
    output$p_duration_intake <- plotly::renderPlotly({
      plotly::ggplotly(p_out()$p_duration_intake,
                       dynamicTicks = TRUE)%>%
        f_change_legend_on_resize()
    })

    output$p_timeline <-  plotly::renderPlotly({
      plotly::ggplotly(p_out()$p_timeline,
                       dynamicTicks = TRUE)%>%
        f_change_legend_on_resize()
    })


  })
}


## To be copied in the UI
# mod_by_bin_vis_ui("by_bin_vis_1")

## To be copied in the server
# mod_by_bin_vis_server("by_bin_vis_1")
