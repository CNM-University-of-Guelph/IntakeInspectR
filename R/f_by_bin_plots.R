# f_by_bin_plots
# Contains functions for plotting data from 'by bin' analysis
################################################################################

#' Plot timeline of feeding events at each feed bin
#'
#' @description A function to plot a point for start and end of feeding event, connected by a line, for each feed bin.
#'
#' @param df_in Data frame that has been filtered via user input (`input$bin_id`) to only include 1 feed bin.
#' @param col_start_time,col_end_time,col_start_weight,col_end_weight,col_colour Column names used for plotting
#' @param col_hover_text column name of extra text for tooltip. Usually from adding a column with paste, e.g. `paste('cow_id:', cow_id)`
#'
#' @returns A ggplot2 object
#'
#' @export


plot_bin_timeline <-
  function(df_in, col_start_time, col_end_time, col_start_weight, col_end_weight, col_colour, col_hover_text){

    n_colour <-  df_in %>% dplyr::pull({{ col_colour }}) %>% dplyr::n_distinct()

    p <- df_in %>%
      ggplot(aes(colour = {{ col_colour }}, text = {{ col_hover_text }}))+
      geom_point(aes(x = {{ col_start_time }}, y = {{ col_start_weight }}))+
      geom_point(aes(x = {{ col_end_time }}, y = {{ col_end_weight }}))+
      geom_segment(aes(x = {{ col_start_time }}, xend = {{ col_end_time }},
                       y = {{ col_start_weight }}, yend = {{ col_end_weight }}))+
      theme_classic(base_size = 14)

    if(n_colour <= 3){
      p <-  p+ggplot2::scale_colour_viridis_d(option = 'H', begin = 0, end = 0.8, na.value = 'red')
    } else{

      p <-  p+ggplot2::scale_colour_viridis_d(option = 'H', begin = 0, end = 0.95, na.value = 'red')
    }

    return(p)
  }


#' plot_bin_regression function
#'
#' @description A function to plot feed_duration vs feed_intake scatter plot.
#' This doesn't include an actual regression function because this is best done per cow, not per bin.
#'
#' @param df_in Dataframe that has been filtered via user input (`input$bin_id`) to only include 1 feed bin.
#' @param x,y columns to plot on x- and y-axis
#' @param col_colour column name to use for `aes(colour = )`
#' @param col_hover_text column name of extra text for tooltip. Usually from adding a column with paste, e.g. `paste('cow_id:', cow_id)`
#'
#' @returns A ggplot2 object
#'
#' @export

plot_bin_regression <-  function(df_in, x, y, col_colour, col_hover_text = NA_character_){
  n_colour <-  df_in %>% dplyr::pull({{ col_colour }}) %>% dplyr::n_distinct()

  p <-  df_in %>%
    ggplot(aes(x = {{ x }},
               y = {{ y }},
               colour = {{ col_colour }},
               text = {{ col_hover_text }}))+
    geom_point(alpha = 0.8, size = 2)+
    theme_classic(base_size = 14)

  if(n_colour <= 3){
    p <-  p+ggplot2::scale_colour_viridis_d(option = 'H', begin = 0, end = 0.8, na.value = 'red')
  } else{

    p <-  p+ggplot2::scale_colour_viridis_d(option = 'H', begin = 0, end = 0.95, na.value = 'red')
  }

  return(p)

}
