#' by_animal_vis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_by_animal_vis_ui <- function(id){
  ns <- NS(id)

  bslib::nav_panel(
  # tabPanel(
    title = "3b. By Animal - Vis",
    div(class = 'resized-tab-container',
        gridlayout::grid_container(

          layout = gridlayout::new_gridlayout(c(
            "1px    0.5fr       1.5fr",
            "1fr    user_input  Display "
          ),
          alternate_layouts = list(
            layout = c(
              "1px       1fr     ",
              "300px    user_input ",
              "800px    Display"
            ),
            width_bounds = c(max = 900)
          )),


          ################################################ #
          # Inputs panel ----
          ################################################ #
          gridlayout::grid_card(
            area = "user_input",
            wrapper = function(x) bslib::card_body(x, fill=TRUE, fillable=FALSE),

            bslib::card_body(
              fill=FALSE,
              max_height = "300px", # so that it doesn't push the toggled UI below off the screen
              verbatimTextOutput(ns("selected_rows"), placeholder = FALSE)
            ),

              bslib::card_body(
                id = ns("toggle_plot_inputs"), # name needed for toggle to unhide
                fill=TRUE,
                fillable = TRUE,
                selectInput(
                  inputId = ns('animal_id'),
                  label = "Select Animal ID to view:",
                  # selected = 1,
                  choices = NULL # This is updated by server
                ),
                dateRangeInput(ns('dateRange'),
                               label = 'Date range to view: ',
                               start = NULL, end = NULL,
                               min = NULL, max = NULL
                )
              ),


            radioButtons(
              inputId = ns('plot_type'),
              label = "What type of plot to show?",
              choiceNames = list("Static (ggplot2)", "Interactive (plotly)"),
              choiceValues = list('static' , 'plotly')
            ),

            radioButtons(
              inputId = ns("plot_type_overall"),
              label = "Select plot type:",
              choices = list(
                histogram = "hist",
                regression = "reg"
              )
            )
          ),

          ################################################ #
          # Display panel ----
          ################################################ #

          gridlayout:: grid_card(
            area = "Display",
            wrapper = bslib::card_body(class = "p-0", fillable = TRUE, fill = TRUE), # this removes the padding around edges
            full_screen = FALSE,
            bslib::navset_card_tab( # a card with nav tabs:
              id = ns('display_tabs'), #used for input$ to see active tab
              # wrapper = bslib::card_body(fill = TRUE, fillable = TRUE),
              full_screen = TRUE,


              bslib::nav_panel(
                title = "Overall Plots",
                value = "overall_plots", #for accessing input$ details
                bslib::card_title("Regression or histogram of all feeding events"),
                p("Overall visualisation of all feeding events. Select plot type (histogram/regression) to view. Regression only shows first 80,000 rows."),
                # bslib::card(
                #   shiny::plotOutput(outputId = ns("p"))  %>%  shinycssloaders::withSpinner(type=7),
                #   full_screen = TRUE
                # ),
                # bslib::card(
                  shiny::plotOutput(outputId = ns("p"), height = '500px')  %>%  shinycssloaders::withSpinner(type=7),
                  # full_screen = TRUE
                # )
              ),

              bslib::nav_panel(
                title = "Table: Outliers",
                value = "outliers_table",
                bslib::card_title("Animals with most outliers (based on rate of intake regression)"),
                fct_table_description(),
                DT::DTOutput(outputId = ns("outlier_table")) %>%  shinycssloaders::withSpinner(type=7)
              ),

              bslib::nav_panel(
                title = "Table: Long durations",
                value = "durations_table",
                bslib::card_title("Animals with longest feed durations: "),
                fct_table_description(),
                DT::DTOutput(outputId = ns("durations_table")) %>%  shinycssloaders::withSpinner(type=7)
              ),

              bslib::nav_panel(
                title = "Plot: Duration vs Intake",
                value = 'plot_regression', #for accessing input$ details
                bslib::accordion(
                  id = ns('accordian_duration_intake'), # must have ID to work
                  open = FALSE,
                  bslib::accordion_panel(
                    title = "Plot description:",
                    icon = bsicons::bs_icon('info-circle'),
                    h5("Duration vs Intake Plot"),
                    p("Visualise feed duration vs intake for individual animals. Each point is an individual feeding event. Outlier
                  points are shown twice, with an arrow pointing from original value to new fitted value. The outlier classifications are based on
                  values selected when cleaned. Outlier type refers to negative (neg) or positive (pos) residual from fitted regression.
                  Boundary lines are the max and min rates of intake selected when cleaning to define outliers.
                  The red box represents the user-defined region where values are 'exempt' from outlier detection.
                  Select 'interactive' plot on left to allow more data on hover or to zoom in and out. "),
                  strong("Hover over bottom right of screen to show 'full screen' button.")
                  )),
                # bslib::card(
                  #make dynamically created as either interactive or not:
                  uiOutput(ns('selected_plot')),
                  # full_screen = TRUE
                # )
              ),

              bslib::nav_panel(
                title = "Plot: Feeding Durations",
                value = 'plot_bins', #for accessing input$ details
                bslib::accordion(
                  id = ns('accordian_long_durations'), # must have ID to work
                  open = FALSE,
                  bslib::accordion_panel(
                    title = "Plot description:",
                    icon = bsicons::bs_icon('info-circle'),
                    h5("Long Durations Plot"),
                    p('
                    The bars on this plot show the start and original (un-corrected) end
                    time of individual feeding events. Very long bars (i.e.
                    long durations) might be errors. The tooltip that shows up
                    when hovering over a bar will include the corrected
                    duration. Durations that were corrected in "by bin" step
                    will overlap with other feeding events at the same time.
                    Durations that do not overlap another event may have been
                    corrected by outlier detection.
                    '),
                    p("Filter by date in side-bar and use controls on top right of plot to zoom in closer."),
                  strong("Hover over bottom right of screen to show 'full screen' button.")
                  )),

                # bslib::card(
                  ggiraph::girafeOutput(outputId = ns("p_bins_duration"), height = '500px') %>%  shinycssloaders::withSpinner(type=7),
                  # full_screen = TRUE
                # )
              )
            )
          )
        )
    )
  )

}

#' by_animal_vis Server Functions
#'
#' @noRd
mod_by_animal_vis_server <- function(id, df_list){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ################################################################# #
    # Get date range from data: ----
    ################################################################# #

    # set range of dates in UI allowed based on df
    observe({

      req(req(nrow(df_list()$merged_df) > 0))

      dates <- df_list()$merged_df  %>% dplyr::summarise(min = min(date), max = max(date))
      date_min <- min(dates$min)
      date_max <- max(dates$max)

      updateDateRangeInput(inputId = "dateRange",
                           start = date_min,
                           end = date_max,
                           min = date_min,
                           max = date_max)
    })

    ##################################################################### #
    # Overall Plots ----
    ##################################################################### #

    output$p <- shiny::renderPlot({

      .df <- isolate(df_list()$nested_df) %>%
        tidyr::unnest(.data$fitted)

      if(input$plot_type_overall == "reg") {
        .df %>%
          dplyr::slice_head(n = 80000) %>%
          fct_plot_by_animal_overall(
            col_intake = !!rlang::sym(df_list()$user_inputs_to_parse_to_vis$selected_intake_column),
            col_duration = !!rlang::sym(df_list()$user_inputs_to_parse_to_vis$selected_duration_column),
            pt_size = 3)+
          ggplot2::labs(
            caption = "This plot only shows first 80,000 rows due to memory constraints."
          )

      }else if(input$plot_type_overall == "hist") {
        .df %>%
        f_plot_summary(
          x = .data$new_x,
          y = .data$new_y,
          col_colour = NULL,
          type = 'hist')

      }},
      #This controls resolution, or how big it appears. for renderplot
      res = 120
    )





    ##################################################################### #
    # Outliers Table ----
    ##################################################################### #
    # Create table summarising number of outliers in each animal
    # create df in reactive first so that it can be accessed downstream
    # ns: outlier_table
    df_n_outliers <- reactive({

      df <- df_list()$nested_df %>%
        dplyr::select(-'data') %>%
        tidyr::unnest(.data$fitted) %>%
        dplyr::count(.data$animal_id, .data$outlier_pos_neg) %>%
        dplyr::mutate(dplyr::across('outlier_pos_neg', ~tidyr::replace_na(.x, 'not_error'))) %>%
        tidyr::pivot_wider(names_from = 'outlier_pos_neg', values_from = 'n') %>%
        dplyr::rowwise() %>%
        dplyr::mutate(total_outliers = sum(
          dplyr::c_across(colnames(.)[colnames(.) %in% c('neg', 'neg_intake' , 'neg_duration', 'pos')] # sum only the cols available
          ), na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(dplyr::desc(.data$total_outliers))

      cols_names <- c(
        `Animal ID` = 'animal_id',
        `Count outliers (intake too high)` = 'pos',
        `Count outliers (duration too long)` = 'neg',
        `Count negative intakes` = 'neg_intake',
        `Count negative durations` = 'neg_duration',
        `Count not error` = 'not_error',
        `Total outliers` = 'total_outliers'
      )

      df <- df %>% dplyr::rename(dplyr::any_of(cols_names))

      return(df)

    })

    output$outlier_table <- DT::renderDT({
      df_n_outliers() %>% fct_DT_nopages(buttons_at_bottom = TRUE, scrollY = 400)
    })

    ##################################################################### #
    # Durations Table ----
    ##################################################################### #

    df_long_durations <- reactive({

      df <- df_list()$merged_df %>%
        # the 2 ways a duration could be an error:
        dplyr::filter(.data$is_end_time_overlap_error | .data$is_outlier) %>%
        dplyr::select('start_time', 'animal_id', 'bin_id', 'duration_sec', 'final_duration_sec', 'is_end_time_overlap_error', 'outlier_pos_neg') %>%
        dplyr::arrange(dplyr::desc(.data$duration_sec))


      colnames(df) <- c('Event start time','Animal ID', 'Feed Bin ID', 'Original duration (sec)', 'Final Duration (sec)', 'Is duration error from "by bin"?', 'Is positive or negative outlier "by animal" ?' )

      return(df)
    })

    output$durations_table <-  DT::renderDT({
      df_long_durations() %>%
        fct_DT_pages(
          pageLength = 20,
          scrollY = 400,
          buttons_at_bottom = TRUE
        )

    })





    ################################# #
    ## Call back for DT table ----
    ################################# #
    # # https://yihui.shinyapps.io/DT-rows/
    output$selected_rows <-
      renderPrint({

        s_outlier <- input$outlier_table_rows_selected # DT exports row index
        s_durations <- input$durations_table_rows_selected

        cat("Selected animal IDs from outliers table:\n")
        if (length(s_outlier)){
          # use row index to slice out rows of df, then return bin ID
          ids_outlier <- df_n_outliers() %>%
            dplyr::slice(s_outlier) %>%
            dplyr::mutate(animalID_asnum = as.numeric(.data$`Animal ID`)) %>%
            dplyr::arrange(.data$animalID_asnum) %>%
            dplyr::pull(.data$`Animal ID`)

          cat(unique(ids_outlier), sep = "\n")
        }

        cat("\n\nSelected animal IDs from durations table:\n")
        if (length(s_durations)){
          # use row index to slice out rows of df, then return bin ID
          ids_durations <- df_long_durations() %>% dplyr::slice(s_durations) %>% dplyr::pull(.data$`Animal ID`)
          cat(unique(ids_durations), sep = "\n")
        }

      })
    #

    #################################################### #
    # Toggle visibility of different user input elements:
    #################################################### #

    # observe when the tabs are on either of the plots, and then show more details in the user_input section:
    observe({

      shinyjs::toggle(id = 'toggle_plot_inputs', anim = TRUE,
                      condition = input$display_tabs %in% c('plot_regression', 'plot_bins' ))

      shinyjs::toggle(id = 'plot_type', anim = TRUE,
                      condition = input$display_tabs %in% c('plot_regression'))

      shinyjs::toggle(id = 'plot_type_overall', anim = TRUE,
                      condition = input$display_tabs %in% c('overall_plots'))

    })





    ##################################################################### #
    # Plot - regression ----
    ##################################################################### #
    # It doesn't make sense to load a dataframe in UI as it is built only at startup
    # Therefore, we can 'update' the UI to include these values

    observe({
      vec_animals <- df_list()$nested_df %>%
        dplyr::select("animal_id_nest") %>%
        dplyr::distinct(.keep_all = TRUE) %>%
        dplyr::arrange(.data$animal_id_nest) %>%
        dplyr::pull(.data$animal_id_nest)

      freezeReactiveValue(input, 'animal_id') # doesn't evaluate on load when no data available
      updateSelectInput(inputId = 'animal_id', choices = vec_animals)
    })

    # Filter data frame based on selected bin and produce ggplots based on selected plot type
    # each user input should generate a new graph on-click.

    data_to_plot <- reactive({
      req(df_list())

      # extra table from nested df for 1 animal and plot:
      isolate(df_list()$nested_df) %>%
        dplyr::filter(.data$animal_id_nest == input$animal_id) %>%
        dplyr::pull(.data$fitted) %>%
        magrittr::extract2(1) %>%
        dplyr::filter(.data$date >= lubridate::ymd(input$dateRange[1]) &  .data$date <= lubridate::ymd(input$dateRange[2]))

    })


    # change plot type based on user input:
    output$selected_plot <- renderUI({
      # require data before rendering plot:
      req(nrow(data_to_plot()) > 0)

      if(any(data_to_plot()$outlier_pos_neg %in% 'insufficient_rows_for_regression')){
        # No regression line available, so use different function:
        p <- data_to_plot() %>%
          fct_plot_by_animal_overall(
            col_intake = !!rlang::sym(df_list()$user_inputs_to_parse_to_vis$selected_intake_column),
            col_duration = !!rlang::sym(df_list()$user_inputs_to_parse_to_vis$selected_duration_column),
            pt_size = 5)+
          labs(title = paste(unique(data_to_plot()$animal_id)),
               x = 'Corrected Feed Duration (seconds)',
               y = 'Corrected Feed Intake (kg)',
               caption = "Showing simplified plot as 'insufficient_rows_for_regression'")


      } else {
        #make plot object:
        p <- data_to_plot() %>%
          fct_plot_by_animal(
            max_intake_rate_kg_min = df_list()$user_inputs_to_parse_to_vis$max_intake_rate_kg_min,
            min_intake_rate_kg_min = df_list()$user_inputs_to_parse_to_vis$min_intake_rate_kg_min,
            outlier_exemption_max_duration = df_list()$user_inputs_to_parse_to_vis$outlier_exemption_max_duration,
            outlier_exemption_max_intake = df_list()$user_inputs_to_parse_to_vis$outlier_exemption_max_intake,
            col_intake = !!rlang::sym(df_list()$user_inputs_to_parse_to_vis$selected_intake_column),
            col_duration = !!rlang::sym(df_list()$user_inputs_to_parse_to_vis$selected_duration_column),
            pt_size = 5)+
          labs(x = 'Corrected Feed Duration (seconds)',
               y = 'Corrected Feed Intake (kg)')
      }


      if(input$plot_type == 'static'){

        output$p_static <- renderPlot({ p })

        return(plotOutput(ns("p_static"), height = '500px') %>% shinycssloaders::withSpinner())


      } else if(input$plot_type == 'plotly'){

        output$p_inter <- plotly::renderPlotly({
          plotly::ggplotly(p+ggplot2::theme_classic(base_size = 12),
                           dynamicTicks = TRUE
                           ) %>%
            plotly::layout(yaxis = list(autorange = FALSE)) %>% # allows coord_cartesian to work when dynamicTicks is TRUE
            f_change_legend_on_resize() })

        return(plotly::plotlyOutput(ns("p_inter"), height = '500px') %>% shinycssloaders::withSpinner())

      }

    })




    ##################################################################### #
    # Plot - bin durations ----
    ##################################################################### #
    # currently this function requires the merged data, not just hte output from the 'fitted' column within the nested df

    output$p_bins_duration <- ggiraph::renderGirafe({
      req(df_list())

      df_list()$merged_df %>%
        dplyr::filter(.data$animal_id == input$animal_id) %>%
        dplyr::filter(.data$date >= lubridate::ymd(input$dateRange[1]) &  .data$date <= lubridate::ymd(input$dateRange[2])) %>%
        fct_plot_animal_bins(col_end_time = .data$end_time, subtitle = NULL)

    })




  })
}

## To be copied in the UI
# mod_by_animal_vis_ui("by_animal_vis_1")

## To be copied in the server
# mod_by_animal_vis_server("by_animal_vis_1")
