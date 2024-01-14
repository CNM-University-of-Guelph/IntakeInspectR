#' Utility function to store text that is repeated on each table
#'
#' @return a <p> via p()
#' @noRd

fct_table_description <- function(){
  p("Select rows of interest to store ID in panel on the left. This panel will be visible when viewing plots. Click column names to sort data. ")
}


#' show modal helper function
#'
#' Helper function for showing modal for all steps of function assigns to a
#' temporary output that is used by showModal once. Also includes a function from 'by animal'.
#'
#' @param doc The documentation name to show. Matches input$f_step*
#' @param output,session parsed down from Shiny module
#'
#' @return Called for side effect of executing showModal()

fct_show_modal_bin_functions <- function(doc, output, session){

  filename <- switch(doc,
                     'f_step1' = "f_by_bin_clean.Rd",
                     'f_step2' = "f_step2.Rd",
                     'f_step3' = "f_step3.Rd",
                     'f_step4' = "f_step4.Rd",
                     'f_step5' = "f_step5_correct_intakes.Rd",
                     'f_step6' = "f_step6_correct_end_times.Rd",
                     'f_outlier_func' = "f_flag_and_replace_outliers.Rd")

  output$tmp_output <- renderUI({
    fct_Rd_to_HTML(Rd_filepath = app_sys(paste0("man/", filename)))
  })

  #show .Rd file in modal:
  showModal(modalDialog(
    title = paste("Function documentation for", filename),
    if(doc == 'f_step1'){
      p("'Step 1' is to call the `f_by_bin_clean()` which implements all steps of this workflow. Step 1 of this function separates out all row's where intake == 0.")
    } else {p()},
    uiOutput(session$ns("tmp_output")),
    size = "l",
    easyClose = TRUE,
    footer = modalButton("Ok")
  ))
}



#' Count and check errors for valuebox
#'
#' Helper function to count a data frame by a particular column, and then
#' return the value of that count for when it meets a certain condition.
#'
#' @param df a data frame with at least col
#' @param col column name to check
#' @param condition A string to match which error's count data should be returned
#'
#' @return a number to print in value box
#'

fct_check_for_valuebox_bin <- function(df, col, condition){
  n_tmp <-
    df %>%
    f_count_errors({{ col }})   %>%
    dplyr::mutate(to_print = paste0(.data$n, " (",.data$percent,"%)")) %>%
    dplyr::filter({{ col }} == condition)

  if(nrow(n_tmp) == 0){  return("0")
  } else { return(n_tmp$to_print)  }
}


#' Function to store text for 'more info' button for mod_by_bin
#' Normally parsed to fct_show_custom_modal()
#'
#' @return a list of HTML

fct_modal_content_bybin_more_info <- function() {
  html <- list(
    #h4("More about the data"),
    p("
      This step of the cleaning checks the row-by-row changes for each feed
      bin in chronological order. This is important because it can be expected
      that the weight recorded by the feed bin should reduce during a feeding
      event and remain the same between feeding events, unless feed is
      intentionally added. When the observed weights recorded by the system do
      not follow this assumption then it is likely that the values are not
      realistic. We can use the assumptions to fix some errors in the end
      weights, by replacing an incorrect end weight with the following start
      weight (see Step 3). Similar logic is applied to the start weights (Step
      4), although it is less likely that the start weight will be incorrect.
      There are also inconsistencies that we can not safely modify, and we
      call these 'errors'. These should be checked manually as the data may
      need to be excluded manually.

      It is important to note that no data is actually deleted during these checks,
      but each classification is stored in new columns. However, in 2 cases the
      rows need to be removed:"),
    HTML("<ul>
             <li> 1) 0 kg intakes are removed as row-by-row checks are difficult when they are not removed </li>
             <li> 2) Step 2 errors, which are considered complete irregularities
         in data, are removed as the row-by-row checks perform better when they are removed. </li>
          </ul>"),
    p("
      In both cases, these removed rows are stored in the list object that
      is returned by the function. When implemented in this RShiny app, only
      the 'cleaned' data is used in downstream cleaning, but the other missing
      rows can be downloaded using the buttons provided. The number of rows
      that will be in these files are shown in the log.
      "),
    strong("The Logs"),
    p("
      The log files, shown in the display panel when the cleaning is executed,
      is also saved in a temporary location while the session is active. It
      displays a summary output that is printed by the",
      tags$code("f_by_bin_clean()"),
      "function. This function can be executed in R (outside of RShiny) and the
      filepath to the tmp location is returned, or logging can be turned off.
      It is a good idea to save these log files (see Final Summary tab)
      alongside your input and output files for a record of how the data was
      manipulated.
      "),
    p("
      The documentation for the underlying R functions that are used by ",
      tags$code("f_by_bin_clean()"),
      " can be viewed using the other buttons. The source
      codes can also be viewed on Github (or by pressing F2 on any function
      name inside RStudio if this package is installed and loaded)."
      )
  )
  return(html)
}


#' Function to create HTML content for modal in mod_by_bin_clean.R
#'
#' @param svgContent SVG content object. Normally loaded into session with a
#' call to readLines(con = app_sys("app/www/filename.svg"), warn = FALSE)
#'
#' @return a list of HTML
fct_modal_content_bybin_flowchart <- function(svgContent){
  html <- list(
    tags$style(
      HTML("
            .modal-dialog {
              max-width: 90%; /* Adjust this value as needed */
            }
            .modal-content {
              width: 100%;
              height: auto;
            }
            .modal-body {
              overflow: auto;
            }
            svg {
              width: 100%;
              height: auto;
            }
          ")
    ),
    HTML(paste0(svgContent, collapse = ""))
  )
  return(html)
}
