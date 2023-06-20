# utility functions used by all modules


#' DT table template
#'
#' Call to `DT::datatable()` with added download buttons, fixed columns and no pages
#'
#' @param x dataframe
#' @param scrollY number to represent size of y axis before scrolling
#' @param buttons_at_bottom boolean. Should buttons for downloading table be at bottom?
#' @param rownames boolean. Are rownames used in this table? Default FALSE.
#' @param fillContainer boolean for DT::datatable(fillContainer)
#'
#'
#' @return DT
#' @export
fct_DT_nopages <- function(x, scrollY = 350, buttons_at_bottom = FALSE, rownames = FALSE,  fillContainer = FALSE) {
  DT::datatable(x,
                fillContainer = fillContainer,
                rownames = rownames,
                escape = TRUE, # see ?datatable
                extensions = c('Buttons','FixedColumns'),
                options = list(
                  #pagelength=20,
                  paging=FALSE,
                  dom = ifelse(buttons_at_bottom, 'rtiB', 'Brti'), #pf
                  #buttons = c('copy', 'csv', 'excel'),
                  buttons = list(
                    list(extend = 'copy', className = 'btn-info'),
                    list(extend = 'csv', className = 'btn-info'),
                    list(extend = 'excel', className = 'btn-info')
                  ),
                  columnDefs = list(list(className = 'dt-center', targets = "_all")),
                  # fixedcolumns options
                  scrollX = TRUE,
                  scrollY = scrollY,
                  #scrollY = TRUE,
                  scrollCollapse = TRUE,
                  fixedColumns = list(leftColumns = 2)
                ) )}





#' Summary plots
#'
#' Summary plots of either regression or histogram
#'
#' @param df_in data frame to plot
#' @param x column name for x axis (if regression) or for histogram
#' @param y if regression, then name of column for y axis (typically intake)
#' @param type either 'hist' or 'reg' for histogram or regression, respectively
#' @param col_colour name of column to colour regression by
#'
#' @return a ggplot2


fct_plots_summary <-  function(df_in, x, y = NULL, col_colour = .data$check_end_weights, type){

  .fct_hist <- function(){
    list(
      ggplot2::geom_histogram(fill ='darkgreen',colour = 'grey'),  # Themes, text size = 12 and black
      ggplot2::theme_classic(base_size = 16),
      ggplot2::theme(axis.text = ggplot2::element_text(colour = 'black')),
      ggplot2::scale_y_continuous(trans = 'log10', n.breaks = 10, labels = ~round(.x, 0)),
      ggplot2::scale_x_continuous(n.breaks = 10))
  }

  if (type == 'reg'){
    df_in %>%
      ggplot(aes(x = {{ x }},
                 y = {{ y }},
                 colour = {{ col_colour }}))+
      geom_point(alpha = 0.7)+
      fct_custom_theme()

  } else if(type == 'hist'){

    p_1 <- df_in %>%
      ggplot(aes(x = {{ x }}/60))+
      .fct_hist()+
      fct_custom_theme()+
      ggplot2::labs(x = 'Feed duration (min)',
                    y = 'Count (log10)')

    p_2 <- df_in %>%
      ggplot(aes(x = {{ y }}))+
      .fct_hist()+
      fct_custom_theme()+
      ggplot2::labs(x = 'Feed intake (kg)',
                    y = 'Count (log10)')

    return(p_1 + p_2)
  }
}


#' Remove legend when window size is less than 900 pixels
#'
#' @param x plotly object
#'
#' @noRd
f_change_legend_on_resize <- function(x){
  htmlwidgets::onRender(x,"
  function(el) {
    var ro = new ResizeObserver(function() {
      var width = window.innerWidth || document.documentElement.clientWidth || document.body.clientWidth;

      if (width < 900) {
        Plotly.relayout(el, { 'showlegend': false });  // Hide the legend
      } else {
        Plotly.relayout(el, { 'showlegend': true });  // Show the legend
      }
    });
    ro.observe(el);
  }
  ")
}
