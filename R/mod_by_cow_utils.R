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
      data, if there are less then outlier detection is not executed."),
    h3("Step 1: Biologically relevant outliers"),
    p(""),

    h3("Step 2: Statistical outliers"),
      p("
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

    h3("Step 3: Estimate new values for outliers"),
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
#' Plot 'by cow' outlier detection results. Visualise feed duration vs intake
#' for individual cows. Each point is an individual feeding event. Outlier
#' points are shown twice, with an arrow pointing from original value to new
#' fitted value. The outlier classifications are based on values selected when
#' cleaned. Outlier type refers to negative (neg) or positive (pos) residual
#' from fitted regression. Boundary lines are the max and min rates of intake
#' selected when cleaning to define outliers. The red box represents the
#' user-defined region where values are 'exempt' from outlier detection. Select
#' 'interactive' plot on left to allow more data on hover or to zoom in and out.
#'
#' Assumes certain column names that were generated by `f_flag_and_replace_outliers`: 'new_x', 'new_y', 'predicted_y_bisector', 'is_outlier', 'outlier_pos_neg', 'cow_id'
#'
#' @param df dataframe to plot
#' @param min_intake_rate_kg_min,max_intake_rate_kg_min number (kg/min). Events with a rate of intake (i.e. intake / duration) greater than the `max_intake_rate_kg_min` and less than the `min_intake_rate_kg_min` will be classed as a 'manual outlier'
#' @param outlier_exemption_max_duration,outlier_exemption_max_intake number (kg or min). Events below this `outlier_exemption_max_intake` (kg) & with a duration less than `outlier_exemption_max_duration` (min) are exempt from other outlier detection methods.
#' @param col_intake name of column with intake data to use
#' @param col_duration name of column with duration data to use
#' @param col_cow_id name of column with cow ID
#' @param pt_size number. point size for `geom_point()`
#'
#' @return ggplot2 object
#' @export
#'
fct_plot_by_cow <- function(df,
                            max_intake_rate_kg_min = 1.5,
                            min_intake_rate_kg_min = 0.05,
                            outlier_exemption_max_duration = 1,
                            outlier_exemption_max_intake = 0.3,
                            col_intake = .data$corrected_intake_bybin,
                            col_duration = .data$corrected_feed_duration_seconds,
                            col_cow_id = .data$cow_id,
                            pt_size = 3){

  df_outliers <- df %>%  dplyr::filter(.data$is_outlier)

  # used to prevent lines breaking when plotting .fitted
  df_noNA <- df %>% dplyr::filter(!is.na(.data$.fitted))

  # for setting coord-cartesian
  max_intake <- df %>% dplyr::pull({{ col_intake }}) %>% max()
  max_duration <- df %>% dplyr::pull({{ col_duration }}) %>% max()


  df_rates_of_intake_bounds <-
    data.frame(x_duration = seq(0, max_duration, by = 10)) %>%
    dplyr::mutate(
      upper_line = max_intake_rate_kg_min/60 * .data$x_duration,
      lower_line = min_intake_rate_kg_min/60 * .data$x_duration
    )

  p <- df %>%
    ggplot2::ggplot(aes(x = {{ col_duration }}, y = {{ col_intake }}, text = paste("cow_id:", {{ col_cow_id }})))+
    ggplot2::geom_point(aes(
      colour = .data$outlier_pos_neg,
      fill = .data$outlier_classification,
      text = paste("Rate of Intake kg/min: ", .data$instant_rate_of_intake_kg_min)
    ),
    size = pt_size,
    shape = 21
    )+
    # Using the pre-calculated values allows better compatability with plotly
    ggplot2::geom_line(aes(x = .data$x_duration, y = .data$upper_line, linetype = 'Boundary of max intake rate kg/min'),
                       colour = 'black',
                       linewidth = 0.8,
                       inherit.aes = FALSE,
                       data = df_rates_of_intake_bounds)+
    ggplot2::geom_line(aes(x = .data$x_duration, y = .data$lower_line, linetype = 'Boundary of min intake rate kg/min'),
                       colour = 'black',
                       linewidth = 0.8,
                       inherit.aes = FALSE,
                       data = df_rates_of_intake_bounds)+

    ggplot2::geom_line(aes(y = .data$.fitted,
                           linetype = '1) Robust Linear Regression'),
                       # colour = 'blue',
                       linewidth = 0.8,
                       data = df_noNA)+
    ggplot2::geom_line(aes(y = .data$predicted_y_bisector, linetype = '2) Bisector Regression'),
                       # colour = 'blue',
                       linewidth = 0.8,
                       data = df_noNA)+

    ggplot2::scale_linetype_manual(
      values = c('1) Robust Linear Regression' = 'dashed',
                 '2) Bisector Regression' = 'solid',
                 'Boundary of max intake rate kg/min' = 'dotted',
                 'Boundary of min intake rate kg/min' = 'twodash'
      )) +

    # add points for outliers only:
    # geom_point(aes(x = .data$new_x, y = .data$new_y, colour = .data$outlier_pos_neg), size = pt_size,
    #            data = df_outliers)+
    ggplot2::geom_point(aes(
      x = .data$new_x, y = .data$new_y,
      colour = .data$outlier_pos_neg,
      fill = .data$outlier_classification,
      # shape = .data$is_outlier
    ),
    size = pt_size, shape = 21,
    data = df_outliers
    )

  # Check if there are outliers or not. If no outliers, plotly can't draw geom_segment so need to bypass.
  if (nrow(df_outliers) > 0){
    p2 <- p +
      # Draw line between points that changed (inherits x and y from ggplot call):
      ggplot2::geom_segment(aes(xend = .data$new_x, yend = .data$new_y, colour = .data$outlier_pos_neg),
                            alpha = 0.75,
                            linewidth = 0.8,
                            data = df_outliers,
                            arrow = grid::arrow(angle = 20, length = ggplot2::unit(0.4, 'cm')),
                            linejoin = 'mitre')
  } else {
    p2 <- p
  }

  p3 <- p2 +
    ggplot2::geom_rect(
      aes(xmin = 0,
          xmax = outlier_exemption_max_duration*60,
          ymin = 0,
          ymax = outlier_exemption_max_intake),
      fill = NA,
      colour = 'red')+

    # rename labels and format:
    ggplot2::labs(
      title = df %>% dplyr::pull({{ col_cow_id }}) %>% unique(),
      linetype = "Fitted Line Type")+

    # NA values are from outlier_pos_neg (if NA it means it wasn't an outlier)
    ggplot2::scale_colour_viridis_d("Outlier Correction Type", option = 'D',
                                    end = 0.8, na.value = "grey60")+
    ggplot2::scale_fill_viridis_d("Outlier Classification", option = 'H',
                                  end = 0.8, na.value = "grey60")+
    ggplot2::theme_classic(base_size = 18)+
    ggplot2::theme(axis.text = ggplot2::element_text(colour = 'black'))+
    ggplot2::coord_cartesian(ylim = c(NA,max_intake),
                             # xlim = c(0,500)
    )+
    NULL

  return(p3)

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
#' @export
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
#     ggiraph::geom_line_interactive(aes(y = .data$predicted_y_bisector, linetype = 'Bisector Regression'),
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
                                         Is duration error 'by bin': {is_end_time_overlap_error}
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

