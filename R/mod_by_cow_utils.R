# Functions for use in Shiny module - by cow



#' Convert Rd to HTML
#'
#' This is used to convert the documentation .Rd files into HTML that can be
#' used inside a modal.
#'
#' For using in a modal, this function will be called inside a renderUI() that
#' is assigned to an output$ . Then, this output is called inside a
#' `showModal(modalDialog( uiOutput(HERE)))`
#'
#' @param Rd_filepath An .Rd file, normally in man/ e.g. "./man/f_flag_and_replace_outliers.Rd"
#'
#' @return HTML file itself
#'

fct_Rd_to_HTML = function(Rd_filepath){
  # 1) create a temp file for step 2 to use. tmp object is the filepath.
  tmp <- tempfile()

  # 2) read the .Rd file and convert to a HTML file
  tools::Rd2HTML(tools::parse_Rd(file = Rd_filepath),
                 out = tmp,
                 no_links = TRUE,
                 package = "IntakeInspectR")

    return( includeHTML(tmp) )
}




#' Check errors for value box
#'
#' Helper function to return the value of that count for when it meets a certain condition.
#' Assumes has been counted with [f_count_errors()].
#'
#' @param df_summary summary table that has a column with different errors
#' @param col What is the name of the column that was counted
#' @param condition A string to match which error's count data should be returned
#'
#' @return A number to print in value box

fct_check_for_valuebox_cow <- function(df_summary, col, condition){
  n_tmp <-
    df_summary %>%
    dplyr::mutate(to_print = paste0(.data$n, " (",.data$percent,"%)")) %>%
    dplyr::filter({{ col }} == condition)

  if(nrow(n_tmp) == 0){  return("0")
  } else { return(n_tmp$to_print)  }
}




#' Function to store text for 'more info' button for mod_by_bin.R
#' Normally parsed to fct_show_custom_modal()
#'
#' @return a list of HTML
#' @noRd
fct_modal_content_bycow_more_info <- function() {
  html <- list(
    #h4("More about the data"),
    p("
      The data is split into individual data sets for each cow which are
      iteratively parsed to the ",
      tags$code("f_flag_and_replace_outliers()"),
      " function. This
      function will firstly drop any rows of data with missing intake or
      duration values, but these should not normally be present. It also
      checks that only 1 cow ID is in the data and that there are 5 or rows of
      data, if there are less then outlier detection is not executed.
      Then, a
      robust linear model is fitted with ",
      tags$code(a(
        href= "https://rdrr.io/cran/MASS/man/rlm.html",
        target="_blank",
        "MASS::rlm()")),
        " function, using a fixed
      intercept (0) as it is expected that a duration of 0 sec is equal to 0
      kg of intake. This also prevents fitting lines with a slope close to 0
      which can occur when a small number of data points are present. The robust linear
      model is used so that large outliers (e.g very long durations with low
      intakes) are less influential on determining the average rate of intake (slope). The
      'robust scale estimate' is used as the residual SD value,
      which is multiplied by the user-supplied threshold (see
      under `+ Advanced` to modify) to flag outlier values. The purpose of identifing
      outliers is to estimate new intake or duration values, but users should be
      careful to only identify very extreme values as outliers. The relatively conservative
      threshold of 5 SD is used as the default value in this analysis. Some values that may appear
      to be 'outliers' may by physically possible but just not as common for that cow, e.g. a
      cow on a feed restriction experiment may have a larger meal once a day, that is consumed much
      faster than other meal events, which could appear as outliers but are actually real events. "),
    p("
      The flagged outliers are temporarily removed from the analysis to fit a
      bisector linear model, based on ",
      a(href = "https://doi.org/10.1086/169390",
        target = "_blank",
        "Isobe (1990)"),". This refers to the bisector of
      both the 'X on Y' and 'Y on X' ordinary least squares
      (OLS) linear models. This bisector model has symmetrical properties that
      allow both the x-axis (duration) and y-axis (intake) to be estimated from a single model.
      This is also important because both of these values are measured by the
      instrument and both are prone to error.
      Outliers (from the initial robust linear model) that have a positive residual
      (i.e. that have an intake that is too high for the recorded duration) are
      then re-estimated by solving the bisector model for X (intake) using the
      known Y value (duration). When the outliers have a negative residual (i.e.
      the duration is too long for the recorded intake) the bisector model is solved
      for X (duration) using the known Y (intake) value. The intercept and slope of the
      bisector model are also returned in the output.
       "),

    em("
        The methods described in this step were developed based on ",
       a(href= "https://doi.org/10.3168/jds.2020-18195",
         target = "_blank",
         "Mensching et al. (2020).")),
    br(),br(),
    strong("The Logs"),
    p("
      A log of the steps are recorded when the outlier detection is
      executed, similar to 'by bin' page. This log will also display a warning for when
      a robust model was not fitted for a particular cow due to there being less than
      5 data points. Logs can be downloaded in a single file on Final Summary page.
      ")
  )
  return(html)
}





#' fct_plot_by_cow
#'
#' Plot outlier detection results
#'
#' Assumes certain column names that were generated by `f_flag_and_replace_outliers`: 'new_x', 'new_y', 'predicted_y', 'is_outlier', 'outlier_pos_neg', 'cow_id'
#'
#' @param df dataframe to plot
#' @param col_intake name of column with intake data to use
#' @param col_duration name of coumn with duration data to use
#' @param pt_size number. point size for `geom_point()`
#'
#' @return ggplot2 object
#' @export
#'

fct_plot_by_cow <- function(df,
                            col_intake,
                            col_duration,
                            pt_size = 3){

  df_outliers <- df %>%  dplyr::filter(!is.na(.data$outlier_pos_neg))

  df %>%
    ggplot(aes(x = {{ col_duration }}, y = {{ col_intake }}, text = paste("cow_id:", .data$cow_id)))+
    geom_point(aes(shape = .data$is_outlier, colour = .data$outlier_pos_neg), size = pt_size)+
    geom_line(aes(y = .fitted, linetype = '1) Robust Linear Regression'),
              colour = 'blue', linewidth = 1)+
    geom_line(aes(y = .data$predicted_y, linetype = '2) Bisector Regression'),
              colour = 'blue', linewidth = 1)+
    scale_linetype_manual(values = c('1) Robust Linear Regression' = 'dashed',
                                     '2) Bisector Regression' = 'solid')) +

    # add points for outliers only:
    geom_point(aes(x = .data$new_x, y = .data$new_y, colour = .data$outlier_pos_neg), size = pt_size,
               data = df_outliers)+

    # Draw line between points that changed (inherits x and y from ggplot call):
    geom_segment(aes(xend = .data$new_x, yend = .data$new_y, colour = .data$outlier_pos_neg),
                 alpha = 0.75,
                 linewidth = 0.8,
                 data = df_outliers,
                 arrow = grid::arrow(angle = 20, length = ggplot2::unit(0.6, 'cm')),
                 linejoin = 'mitre')+
    # rename labels and format:
    labs(title = paste(unique(df$cow_id)),
         linetype = "Fitted Line Type")+
    # NA values are from outlier_pos_neg (if NA it means it wasn't an outlier)
    ggplot2::scale_colour_viridis_d("Outlier type", option = 'D',
                                    end = 0.8, na.value = "grey60")+
    ggplot2::theme_classic(base_size = 18)+
    ggplot2::theme(axis.text = ggplot2::element_text(colour = 'black'))

}



#' Plot for overall data
#'
#' By cow overall plot which plots a regression showing the different outliers.
#' Assumes various columns are present from the [f_flag_and_replace_outliers()] function
#'
#' @param df data frame to plot
#' @param col_intake column with intake data (y axis)
#' @param col_duration column with duration data (x axis)
#' @param pt_size point size to use (for ggplot2)
#'
#' @return a ggplot2
fct_plot_by_cow_overall <- function(df,
                                    col_intake,
                                    col_duration,
                                    pt_size = 3){

  df_outliers <- df %>%  dplyr::filter(!is.na(.data$outlier_pos_neg))

  df %>%
    ggplot(aes(x = {{ col_duration }}, y = {{ col_intake }}))+
    geom_point(aes(shape = .data$is_outlier, colour = .data$outlier_pos_neg), size = pt_size)+

    # add points for outliers only:
    geom_point(aes(x = .data$new_x, y = .data$new_y, colour = .data$outlier_pos_neg), size = pt_size,
               data = df_outliers)+

    # Draw line between points that changed (inherits x and y from ggplot call):
    geom_segment(aes(xend = .data$new_x, yend = .data$new_y, colour = .data$outlier_pos_neg),
                 alpha = 0.75,
                 linewidth = 0.8,
                 data = df_outliers,
                 arrow = grid::arrow(angle = 20, length = ggplot2::unit(0.6, 'cm')),
                 linejoin = 'mitre')+
    # NA values are from outlier_pos_neg (if NA it means it wasn't an outlier)
    ggplot2::scale_colour_viridis_d("Outlier type", option = 'D',
                                    end = 0.8, na.value = "grey60")+
    ggplot2::theme_classic(base_size = 18)+
    ggplot2::theme(axis.text = ggplot2::element_text(colour = 'black'))+
    ggplot2::labs(x = "Corrected Feed Duration (sec)",
                  y = "Corrected intake (kg)")

}



# fct_plot_by_cow_ggiraph <- function(df,
#                                     col_intake,
#                                     col_duration,
#                                     pt_size = 3){
#
#   df_outliers <- df %>%  dplyr::filter(!is.na(.data$outlier_pos_neg))
#
#   p <- df %>%
#     ggplot(aes(x = {{ col_duration }}, y = {{ col_intake }}))+
#     ggiraph::geom_point_interactive(aes(shape = .data$is_outlier, colour = .data$outlier_pos_neg), size = pt_size)+
#     ggiraph::geom_line_interactive(aes(y = .data$predicted_y, linetype = 'Bisector Regression'),
#                                    colour = 'blue', linewidth = 1)+
#
#
#     # add points for outliers only:
#     ggiraph::geom_point_interactive(aes(x = .data$new_x, y = .data$new_y, colour = .data$outlier_pos_neg), size = pt_size,
#                                     data = df_outliers)+
#
#     # Draw line between points that changed (inherits x and y from ggplot call):
#     ggiraph::geom_segment_interactive(aes(xend = .data$new_x, yend = .data$new_y, colour = .data$outlier_pos_neg),
#                                       alpha = 0.75,
#                                       linewidth = 0.8,
#                                       data = df_outliers,
#                                       arrow = grid::arrow(angle = 20, length = ggplot2::unit(0.6, 'cm')),
#                                       linejoin = 'mitre')+
#     # rename labels and format:
#     ggplot2::labs(title = paste(unique(df$cow_id)),
#                   linetype = 'Fitted line')+
#     # NA values are from outlier_pos_neg (if NA it means it wasn't an outlier)
#     ggplot2::scale_colour_viridis_d("Outlier type", option = 'D',
#                                     end = 0.8, na.value = "grey60")+
#     ggplot2::theme_classic(base_size = 18)+
#     ggplot2::theme(axis.text = ggplot2::element_text(colour = 'black'))
#
#   # make girafe plot:
#
#   g_p <- ggiraph::girafe(ggobj = p, width = 12, height = 7) %>%
#     ggiraph::girafe_options(
#       ggiraph::opts_tooltip(opacity = .7),
#       ggiraph::opts_zoom(min = .5, max = 4),
#
#       ggiraph::opts_sizing(rescale = TRUE),
#       ggiraph::opts_hover(css = "fill:red;stroke:orange;r:3pt;")
#       # ggiraph::opts_selection(type = "multiple", only_shiny = FALSE, css = "fill:red;stroke:gray;r:3pt;") )
#     )
#
#   return(ggiraph::renderGirafe(g_p))
#
# }




# NOTE: this one returns the actuale girafe() object, and then is rendered inside server()
fct_plot_cow_bins <-  function(df,
                               col_end_time,
                               subtitle = ""){

  p <-
    df  %>%
    dplyr::arrange(.data$feed_bin_id) %>% # ggiraph needs this
    dplyr::mutate(feed_bin_id = forcats::fct_inorder(as.factor(.data$feed_bin_id)),
                  # intake_groups_kg = forcats::fct_lump_n(as.factor(.data$final_intake_kg), n = 10) ,
                  intake_groups_kg = cut(.data$final_intake_kg, breaks = 10, ordered_result = TRUE) ,
                  final_duration_sec = round(.data$final_duration_sec, 2),
                  final_duration_min = round(.data$final_duration_sec/60, 0),
                  feed_duration_min = round(.data$feed_duration/60, 0),
                  hover_col = glue::glue("
                                         Corrected intake (kg): {final_intake_kg}
                                         Corrected duration (sec): {final_duration_sec}
                                         Corrected duration (min): {final_duration_min}
                                         Original duration (sec): {feed_duration}
                                         Original duration (min): {feed_duration_min}\n
                                         Is duration error 'by bin': {is_end_time_error}
                                         Is pos or neg outlier: {outlier_pos_neg}

                                         Cow: {cow_id}
                                         Feed Bin: {feed_bin_id}
                                         Start time: {start_time}"
                                         )) %>%

    ggplot(aes(
      y = .data$feed_bin_id,
      xmin = .data$start_time,
      xmax = {{ col_end_time }},
      colour = .data$intake_groups_kg,
      tooltip = .data$hover_col
    ))+
    ggiraph::geom_linerange_interactive(linewidth = 7, hover_nearest = TRUE)+
    ggplot2::scale_colour_viridis_d(option = "H")+
    ggplot2::labs(subtitle = subtitle)+
    ggplot2::theme_classic(base_size = 16)+
    labs(y = 'Feed Bin ID',
         colour = 'Intake groups (kg)')+
    NULL

  g_p <-  ggiraph::girafe(ggobj = p, width = 12) %>%
    ggiraph::girafe_options(
      ggiraph::opts_tooltip(opacity = .7),
      ggiraph::opts_zoom(min = .5, max = 4),

      ggiraph::opts_sizing(rescale = TRUE),
      ggiraph::opts_hover(css = "fill:red;stroke:orange;r:3pt;")
      # ggiraph::opts_selection(type = "multiple", only_shiny = FALSE, css = "fill:red;stroke:gray;r:3pt;") )
    )

  return(g_p)

}

