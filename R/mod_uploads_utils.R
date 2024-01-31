# Combined utils functions for mod_uploads.R
# Sections:
#   - Data Import Handlers
#   - Modals
#   - Value Boxes



##############################################################################
# Data Import Handlers ----
##############################################################################

#' Import DAT files with default column names
#'
#' These are hard-coded column names, expecting a specific file type. It also
#' requires the file names to have the date as 6 numbers in it as: YYMMDD.
#' This is the default behaviour of DAT files from insentec.
#'
#'
#' @param .x datapath from import$DAT_in$datapath
#' @param .y filename from import$DAT_in$name
#'
#' @export
fct_import_DAT_default <- function(.x, .y){

  print('DEV: Using hard-coded column names for .DAT')

  # create internal fread function with hard coded colnames.
  # used below
  .fread_DAT <- function(.x, .y){
    data.table::fread(.x,
                    select = 1:10, # column numbers to keep
                    col.names = c('transponder_id', 'animal_id',
                                  'bin_id', 'start_time',
                                  'end_time', 'duration_sec',
                                  'start_weight_kg', 'end_weight_kg',
                                  'diet', 'intake'))%>%
    dplyr::mutate(filename = .y)
  }

  fct_date_time_parse_error_handler({

    # map over each file
    purrr::map2(.x = .x,
                .y = .y,
                ~ .fread_DAT(.x, .y)
    ) %>%
      purrr::list_rbind() %>%
      dplyr::mutate(date = stringr::str_extract(.data$filename, pattern = "[:digit:]{6}"),
                    dplyr::across(c('start_time', 'end_time'), ~ lubridate::ymd_hms(stringr::str_c(.data$date, .x, sep = "_"))),
                    date = lubridate::ymd(.data$date)) %>%
      dplyr::select(-'filename') %>%

      # re-calculate intake column to mimick raw files not (FR)
      dplyr::mutate(intake = .data$start_weight_kg - .data$end_weight_kg)

  })
}



#' Function for importing .csv or .txt
#'
#' This gives more custom options, but it still has rules. It checks for certain
#' column names to be present. This is a requirement for the Shiny app, not the underlying
#' R code which can have any column name parsed to it.
#'
#' @param .x datapath from import$DAT_in$datapath
#' @param colnames_df a dataframe with 2 columns 'required_names' and 'uploaded_names'. Uploaded names are entered by user and get renamed to required_names.
#'
#' @export
fct_import_csv_txt <- function(.x, colnames_df){


  # map over each file
  df_imported <- purrr::map(.x, data.table::fread) %>%
    purrr::list_rbind()

  #store sample of original uploaded df
  cols_original_import <- colnames(df_imported)


  print('DEV: checking column names for matches')

  # 1. Select columns by name and check they are present
  required_columns <- c('animal_id', 'bin_id', 'start_time',
                        'end_time', 'duration_sec', 'start_weight_kg', 'end_weight_kg',
                        'date')

  if(!all(required_columns %in% colnames(df_imported))){
    # try to rename columns:
    df_imported <-
      fct_missing_col_error_handler({
        df_imported %>%
          dplyr::rename_with(~ colnames_df$required_names[match(.x, colnames_df$uploaded_names, nomatch = "ERROR")],
                             .cols = colnames_df$uploaded_names)
      })

  }

  # Then, re-check df_imported and continue
  if(all(required_columns %in% colnames(df_imported))){
    # if true, then format dates and times
    # Try executing the code and catch the warning
    df_out <- fct_date_time_parse_error_handler({
      return(
        df_imported %>%
        dplyr::mutate(dplyr::across(c('start_time', 'end_time'), ~ lubridate::ymd_hms(stringr::str_c(.data$date, .x, sep = "_"))),
                      date = lubridate::ymd(.data$date)) %>%
        # re-calculate intake column to mimic raw files not (FR)
        dplyr::mutate(intake = .data$start_weight_kg - .data$end_weight_kg)
      )
    })

  } else {
    warning("column names of original df uploaded:")
    warning(cols_original_import)

    if (!is.null(df_imported)){
      # if false, then try and select with all_of() so that meaningful error is thrown. Not returning anything.
      fct_missing_col_error_handler({
        df_imported %>% dplyr::select(tidyselect::all_of(required_columns))
      })
    }

    # return an empty data frame so that app doesn't crash
    df_out <- data.frame(
      animal_id = integer(),
      bin_id = integer(),
      start_time = character(),
      end_time = character(),
      duration_sec = integer(),
      start_weight_kg = numeric(),
      end_weight_kg = numeric(),
      date = character(),
      stringsAsFactors = FALSE
    )
  }


  return(df_out)

}


#' Error handling for date / times
#'
#' This is a wrapper for tryCatch that is designed to be used for checking the format
#' of dates. Normally, lubridate will throw a warning if it can't format the dates/times correctly.
#' This function will detect that error and create a modal to warn the user that the df was uploaded, but that the
#' dates/times are likely incorrect.
#'
#' There is a slight performance decrease as the expr is evaluated a second time to return the data.
#'
#' @param expr A section of R code inside of a `{}`, see [tryCatch()]
#'
#' @return Returns NULL because it can't keep the data frame with incorrect files
fct_date_time_parse_error_handler <- function(expr) {
  data_out <- tryCatch(
    expr,
    warning = function(w) {
      # Check if the warning matches the parse failure due to incorrect date time
      if (grepl("All formats failed to parse", w$message, fixed = TRUE)) {
        # Throw warning and create modal to warn the user
        warning(w)
        fct_show_custom_modal(
          content = list(p("Error parsing date or times. Please close this box and check files before re-uploading."),
                         p("Currently the date must be provided separately from the start_time and end_time. The date
                           should be in a format that can be parsed to",
                           tags$code(a(href="https://lubridate.tidyverse.org/reference/ymd.html", target="_blank", "lubridate::ymd()")),
                           " (e.g. '2022-04-28' or '220428'). The start_time and end_time should be in a format that can be parsed to",
                           tags$code(a(href="https://lubridate.tidyverse.org/reference/hms.html",
                                       target = "_blank", "lubridate::hms()."))
                         )),
          title = "Error!"
        )
      } else {
        # Re-raise the warning if it doesn't match the specific warning message
        warning(w)
      }
      # Return the result of the expr as is
      return(expr)
    }
  )

  # Return the result outside the tryCatch block
  return(data_out)
}


#' Error handling for missing columns
#'
#' This is a wrapper for tryCatch that is designed to be used for checking if columns are missing.
#' Normally, select(all_of()) will throw a meainingful warning.
#' This function will detect that error and create a modal to warn the user that the upload failed.
#'
#' @param expr A section of R code inside of a `{}`, see [tryCatch()]
#'
#' @return Returns the result of expr, which is normally a data frame.
fct_missing_col_error_handler <- function(expr) {
  data_out <- tryCatch(
    expr,
    error = function(e) {

      # Throw warning and create modal to warn the user
      warning("An error occurred:", conditionMessage(e))
      fct_show_custom_modal(
        content = list(
          p("Error: missing columns in uploaded data. Please close this box and try to upload files with correct column names.
            Or, use the 'Advanced: Custom column names' to rename columns.
              More details:"),
          conditionMessage(e)
        ),
        title = "Error!"
      )
      return(NULL)
    }
  )

  # Return the result outside the tryCatch block
  return(data_out)
}





#' Check file types of user input
#'
#' The files should all be the same type. IF they are, then return the name of it
#'
#' @param char_user_in character vector of filenames. Extracted from file name inputs.
#' @returns IF all equal, then returns the name filetype uploaded
#' @noRd
#'
fct_check_filetypes <- function(char_user_in) {

  # extract file type from filenames, returns a char vector if multiple:
  filetype_in <- char_user_in %>% stringr::str_split_i(pattern="\\.", i = 2)

  if (length(unique(filetype_in)) > 1) {
    stop("Error: Uploaded files should all be of a single file type.")
  } else {
    filetype = filetype_in[1]
  }

  print(paste("DEV: filetype:", filetype))

  if(filetype %in% c("DAT", "dat", "CSV", "csv", "TXT", "txt")){
    return(filetype)
  } else{
    stop("Error: File type must be either .DAT, .CSV or .TXT")
  }

}



#' Get min and max date
#'
#' Returns the min() and max() date from `df_in`. Also has the side effect of showing
#' the UI that was hidden with the id 'toggle_date_inputs'.
#'
#' @param df_in The uploaded, combined data frame
#'
#' @return a list with date_min and date_max
#' @noRd
fct_get_dates_min_max <- function(df_in){
  dates <- df_in  %>% dplyr::summarise(min = min(date), max = max(date))
  date_min <- min(dates$min)
  date_max <- max(dates$max)

  # at same time, unhide the UI related to dates:
  shinyjs::show(id = 'toggle_date_inputs', anim = TRUE)

  return(list(date_min = date_min, date_max = date_max))
}



##############################################################################
# Modals ----
##############################################################################

#' A function to show the modal dialog
#'
#' A wrapper for `showModal(modalDialog())`
#'
#' @param content a list of html to parse to modalDialog function
#' @param title The title of the modal
#'
#' @return NULL - used for side effects
#' @noRd
fct_show_custom_modal <- function(content, title=NULL) {
  showModal(modalDialog(
    content,
    title = title,
    size = "l",
    easyClose = TRUE,
    footer = modalButton("Ok")
  ))
}


# #' Function to store text for 'more info' button for mod_uploads.R
# #' Normally parsed to fct_show_custom_modal()
# #'
# #' @return a list of HTML
# #' @noRd
# fct_modal_content_uploads_more_info <- function() {
#   html <- list(
#     #h4("More about the data"),
#     p(
#       "The Insentec Roughage Intake Control (RIC) System is one type
#                  of system used by livestock researchers. This system consists of
#                  feed bins which are suspended on load cells that constantly
#                  measure the weight of the bin and the feed inside it. When a animal
#                  visits the feed bin, a RFID reader checks the animal's ear tag,
#                  determines if it is allowed access and opens a gate (on some
#                  models) and records the starting time and weight of the feed bin
#                  of the feeding event. The system uses 2 lasers to determine if
#                  the animal's head is still over the feed bin, and once it detects
#                  that the animal has left it will record the end time and weight.
#                  This 'event' corresponds to 1 row of data in the database. This
#                  data can be messy and researchers often do not have the ability to
#                  easily visualise all of these individual events before
#                  calculating mean daily feed intake values.\n\nThis dashboard allows
#                  you to upload raw data from the insentec feed
#                  system in multiple flat file formats. The minimum information required is:
#                  "),
#     HTML("<ul>
#                    <li> Animal ID </li>
#                    <li> Feed Bin ID </li>
#                    <li> Start Time (and date) </li>
#                    <li> End Time (and date) </li>
#                    <li> Start Weight </li>
#                    <li> End Weight </li>
#                  </ul>"),
#     br(),
#     p("For more information see Upload Instructions")
#   )
#   return(html)
# }

#' Function to store text for instructions for mod_uploads.R
#' Normally parsed to fct_show_custom_modal()
#'
#' @return a list of HTML
#' @noRd
fct_modal_content_uploads_instructions <- function() {
  bslib::navset_card_pill( # a card with nav tabs:

    bslib::nav_panel(
      title = ".CSV and .TXT files",
      #h4("Instructions"),
      em("Multiple files will be joined together automatically, see details below."),
      p("When uploading .CSV and .TXT files they can be delimited by commas, spaces, tabs, or any other delimiter that is
      accepted by", tags$code(a(href = "https://rdatatable.gitlab.io/data.table/reference/fread.html",
                                target="_blank",
                                "data.table::fread(...,sep='auto')")),
      ". These files must also include the same column names for each file and be
      in the same order for each file uploaded simultaneously. The following column
      names are required (in any order):"),

      HTML(paste("<ul>",
                 paste("<li>",
                       c('animal_id', 'bin_id', 'start_time',
                         'end_time', 'duration_sec', 'start_weight_kg', 'end_weight_kg',
                         'date'
                       ), "</li>", sep = "", collapse = ""),
                 "</ul>", sep = "")),
      p("
      A warning will be displayed if a file is uploaded with incorrect column names. You
      can then use the 'Advanced: Custom column names' drop down box to rename
      the columns that were uploaded to the required column names.
      "),

      p("Currently the date must be provided separately from the start_time and end_time column. The date
      should be in a format that can be parsed to",
      tags$code(a(href="https://lubridate.tidyverse.org/reference/ymd.html",
                  target="_blank",
                  "lubridate::ymd()")),
      " (e.g. '2022-04-28' or '220428').
      The start_time and end_time should be in a format that can be parsed to",
      tags$code(a(href="https://lubridate.tidyverse.org/reference/hms.html",
                  target = "_blank",
                  "lubridate::hms()."))
      ),

      p("If using .csv or .txt files you can upload additional columns beyond those required by IntakeInspectR and they will
      be retained throughout analysis. If there is an `intake` column among the
      additional columns, it will be ignored as intake is always calculated based on
      the provided `start_weight_kg` and `end_weight_kg` columns."),

      strong("Filtering feed bin and animal IDs"),
      br(),
      p("After uploading the files, use the provided controls to filter the data
      by dates, feed bin IDs, and animal IDs. However, please ensure that all data
      for a feed bin is included. If you filter the data to include only some
      animals, but other animals also used the same feed bin, the cleaning process may not work as expected."),


    ),
    bslib::nav_panel(
      title = ".DAT files from Insentec (Hokofarm RIC)",
      em("Multiple files will be joined together automatically, see details below."),
      strong(".DAT Files"),
      p("When uploading .DAT files, please note that they are specific to the
      Insentec (RIC) system and therefore these should not have column names.
      However, the columns should be in the following order, as they will be
      assigned the following column names (only these first 10 columns will be imported):"),

      HTML(paste("<ul>",
                 paste("<li>",
                       c('transponder_id', 'animal_id', 'bin_id', 'start_time',
                         'end_time', 'duration_sec', 'start_weight_kg', 'end_weight_kg',
                         'diet', 'intake'
                       ), "</li>", sep = "", collapse = ""),
                 "</ul>", sep = "")),
      p("In addition, the file names for .DAT files must include the date as 6 digits in the format YYMMDD,
      and no other numbers should be in the file name, e.g. `VR220428.DAT`. This is the only way the Insentec system records the date for the files."),
      h5("More about the Insentec system"),
      p(
        "The Insentec Roughage Intake Control (RIC) System is one type
                 of system used by livestock researchers. This system consists of
                 feed bins which are suspended on load cells that constantly
                 measure the weight of the bin and the feed inside it. When a animal
                 visits the feed bin, a RFID reader checks the animal's ear tag,
                 determines if it is allowed access and opens a gate (on some
                 models) and records the starting time and weight of the feed bin
                 of the feeding event. The system uses 2 lasers to determine if
                 the animal's head is still over the feed bin, and once it detects
                 that the animal has left it will record the end time and weight.
                 This 'event' corresponds to 1 row of data in the database. This
                 data can be messy and researchers often do not have the ability to
                 easily visualise all of these individual events before
                 calculating mean daily feed intake values. ")
    ),
    bslib::nav_panel(
      title = "",
      p("This dashboard allows you to upload raw data from automatic feed bins
        in multiple flat file formats. See The minimum information required is:"),
      HTML("<ul>
        <li> Animal ID </li>
        <li> Feed Bin ID </li>
        <li> Start Time (and date) </li>
        <li> End Time (and date) </li>
        <li> Start Weight </li>
        <li> End Weight </li>
        </ul>")
    )
  )
  #
  #  list(
  #   #h4("Instructions"),
  #   p("Either .DAT, .CSV or .TXT files can be uploaded.
  #     Multiple files will be joined together automatically.
  #     See detailed instructions for each file type below."),
  #   br(),
  #
  #   strong(".DAT Files"),
  #   br(),
  #   p("When uploading .DAT files, please note that they are specific to the
  #     Insentec (RIC) system and therefore these should not have column names.
  #     However, the columns should be in the following order, as they will be
  #     assigned the following column names (only these first 10 columns will be imported):"),
  #
  #   HTML(paste("<ul>",
  #              paste("<li>",
  #                    c('transponder_id', 'animal_id', 'bin_id', 'start_time',
  #                      'end_time', 'duration_sec', 'start_weight_kg', 'end_weight_kg',
  #                      'diet', 'intake'
  #                    ), "</li>", sep = "", collapse = ""),
  #              "</ul>", sep = "")),
  #   br(),
  #   p("In addition, the file names for .DAT files must include the date as 6 digits in the format YYMMDD,
  #     and no other numbers should be in the file name, e.g. `VR220428.DAT`. This is the only way the Insentec system records the date for the files."),
  #
  #   strong(".CSV and .TXT Files"),
  #   br(),
  #   p("When uploading .CSV and .TXT files they can be delimited by commas, spaces, tabs, or any other delimiter that is
  #     accepted by", tags$code(a(href = "https://rdatatable.gitlab.io/data.table/reference/fread.html",
  #                               target="_blank",
  #                               "data.table::fread(...,sep='auto')")),
  #     ". These files must also include the same column names for each file and be
  #     in the same order for each file uploaded simultaneously. The following column
  #     names are required (in any order):"),
  #
  #   HTML(paste("<ul>",
  #              paste("<li>",
  #                    c('animal_id', 'bin_id', 'start_time',
  #                      'end_time', 'duration_sec', 'start_weight_kg', 'end_weight_kg',
  #                      'date'
  #                    ), "</li>", sep = "", collapse = ""),
  #              "</ul>", sep = "")),
  #   p("
  #     If a file is uploaded with incorrect names a warning will be shown. You
  #     can then use the 'Advanced: Custom column names' drop down box to rename
  #     the columns that were uploaded to the required column names.
  #     "),
  #
  #   p("Currently the date must be provided separately from the start_time and end_time. The date
  #     should be in a format that can be parsed to",
  #     tags$code(a(href="https://lubridate.tidyverse.org/reference/ymd.html",
  #                 target="_blank",
  #                 "lubridate::ymd()")),
  #     " (e.g. '2022-04-28' or '220428').
  #     The start_time and end_time should be in a format that can be parsed to",
  #     tags$code(a(href="https://lubridate.tidyverse.org/reference/hms.html",
  #                 target = "_blank",
  #                 "lubridate::hms()."))
  #     ),
  #
  #   p("If using .csv or .txt files you can upload additional columns beyond the required ones and they will
  #     be retained throughout analysis. If there is an `intake` column among the
  #     additional columns, it will be ignored as intake is always calculated based on
  #     the provided `start_weight_kg` and `end_weight_kg` columns."),
  #
  #   strong("Filtering feed bin and animal IDs"),
  #   br(),
  #   p("After uploading the files, use the provided controls to filter the data
  #     by dates, feed bin IDs, and animal IDs. However, please ensure that all data
  #     for a feed bin is included. If you filter the data to include only some
  #     animals, but other animals also used the same feed bin, the cleaning process may not work as expected."),
  #
  #   p(
  #     "The Insentec Roughage Intake Control (RIC) System is one type
  #                of system used by livestock researchers. This system consists of
  #                feed bins which are suspended on load cells that constantly
  #                measure the weight of the bin and the feed inside it. When a animal
  #                visits the feed bin, a RFID reader checks the animal's ear tag,
  #                determines if it is allowed access and opens a gate (on some
  #                models) and records the starting time and weight of the feed bin
  #                of the feeding event. The system uses 2 lasers to determine if
  #                the animal's head is still over the feed bin, and once it detects
  #                that the animal has left it will record the end time and weight.
  #                This 'event' corresponds to 1 row of data in the database. This
  #                data can be messy and researchers often do not have the ability to
  #                easily visualise all of these individual events before
  #                calculating mean daily feed intake values.\n\nThis dashboard allows
  #                you to upload raw data from the insentec feed
  #                system in multiple flat file formats. The minimum information required is:
  #                "),
  #   HTML("<ul>
  #                  <li> Animal ID </li>
  #                  <li> Feed Bin ID </li>
  #                  <li> Start Time (and date) </li>
  #                  <li> End Time (and date) </li>
  #                  <li> Start Weight </li>
  #                  <li> End Weight </li>
  #                </ul>")
  # )
}

##############################################################################
# Value Boxes ----
##############################################################################


#' Draw SVG animal face
#'
#' @param col colour to use for lines. either 'black' or 'white'
#'
#' @return returns HTML() with the <svg> code to draw icon.
#' Can be tested in R console using `htmltools::browsable(fct_animal_icon('white'))`
#' @export
#'
fct_animal_icon <- function(col){
  if(col == 'white'){
    HTML('<svg fill="#ffffff" height="64px" width="64px" version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" viewBox="0 0 512 512" xml:space="preserve" stroke="#ffffff"><g id="SVGRepo_bgCarrier" stroke-width="0"></g><g id="SVGRepo_tracerCarrier" stroke-linecap="round" stroke-linejoin="round" stroke="#CCCCCC" stroke-width="1.024"></g><g id="SVGRepo_iconCarrier"> <g> <g> <path d="M211.054,349.845c-8.773,0-15.886,7.113-15.886,15.886v38.019c0,8.773,7.113,15.886,15.886,15.886 s15.886-7.113,15.886-15.886v-38.019C226.941,356.958,219.828,349.845,211.054,349.845z"></path> </g> </g> <g> <g> <path d="M300.945,349.845c-8.773,0-15.886,7.113-15.886,15.886v38.019c0,8.773,7.113,15.886,15.886,15.886 c8.773,0,15.886-7.113,15.886-15.886v-38.019C316.832,356.958,309.719,349.845,300.945,349.845z"></path> </g> </g> <g> <g> <path d="M512,153.677c0-8.773-7.113-15.886-15.886-15.886H394.07c-7.068-12.725-15.95-24.544-26.527-35.122 c-0.377-0.377-0.764-0.737-1.143-1.11V44.186c0-8.773-7.113-15.886-15.886-15.886s-15.886,7.113-15.886,15.886v33.17 c-49.075-28.287-109.4-27.364-157.256-0.116V44.186c0-8.773-7.113-15.886-15.886-15.886s-15.886,7.113-15.886,15.886v57.058 c-11.042,10.853-20.323,23.267-27.637,36.549H15.886C7.113,137.792,0,144.905,0,153.678c0,50.445,50.811,85.642,98.249,67.288 v78.902c-24.231,15.814-38.92,42.592-38.92,72.051v25.642c0,47.497,38.641,86.139,86.14,86.139h221.063 c47.498,0,86.14-38.641,86.14-86.139v-25.642c0-29.462-14.694-56.242-38.92-72.052v-78.901 C461.232,239.336,512,204.06,512,153.677z M256.221,88.228c-10.365,57.372-63.049,98.196-121.841,93.074 C149.187,126.34,199.212,88.162,256.221,88.228z M130.021,214.213c0-0.472,0.011-0.944,0.016-1.416 c76.41,7.501,145.145-45.73,157.736-120.637c53.146,13.727,94.206,62.646,94.206,122.053v72.938 c-18.231-3.294-244.246-1.398-251.958,0V214.213z M35.027,169.563h69.636c-1.099,3.735-2.125,7.782-2.943,11.621 C81.902,202.533,46.556,196.414,35.027,169.563z M420.898,371.919v25.642h0.001c0,29.978-24.388,54.367-54.368,54.367H145.468 c-29.979,0-54.368-24.388-54.368-54.367v-25.642c0-30.536,24.73-54.367,54.368-54.367h221.063 C396.111,317.553,420.898,341.329,420.898,371.919z M410.3,181.205c-0.831-3.925-1.81-7.809-2.936-11.642h69.609 C465.434,196.438,430.102,202.507,410.3,181.205z"></path> </g> </g> <g> <g> <circle cx="179.916" cy="240.569" r="21.015"></circle> </g> </g> <g> <g> <circle cx="332.729" cy="240.569" r="21.015"></circle> </g> </g> </g></svg>'
    )
  }else if(col == 'black'){
    HTML('<svg fill="#000000" height="64px" width="64px" version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" viewBox="0 0 512 512" xml:space="preserve" stroke="#000000"><g id="SVGRepo_bgCarrier" stroke-width="0"></g><g id="SVGRepo_tracerCarrier" stroke-linecap="round" stroke-linejoin="round" stroke="#CCCCCC" stroke-width="1.024"></g><g id="SVGRepo_iconCarrier"> <g> <g> <path d="M211.054,349.845c-8.773,0-15.886,7.113-15.886,15.886v38.019c0,8.773,7.113,15.886,15.886,15.886 s15.886-7.113,15.886-15.886v-38.019C226.941,356.958,219.828,349.845,211.054,349.845z"></path> </g> </g> <g> <g> <path d="M300.945,349.845c-8.773,0-15.886,7.113-15.886,15.886v38.019c0,8.773,7.113,15.886,15.886,15.886 c8.773,0,15.886-7.113,15.886-15.886v-38.019C316.832,356.958,309.719,349.845,300.945,349.845z"></path> </g> </g> <g> <g> <path d="M512,153.677c0-8.773-7.113-15.886-15.886-15.886H394.07c-7.068-12.725-15.95-24.544-26.527-35.122 c-0.377-0.377-0.764-0.737-1.143-1.11V44.186c0-8.773-7.113-15.886-15.886-15.886s-15.886,7.113-15.886,15.886v33.17 c-49.075-28.287-109.4-27.364-157.256-0.116V44.186c0-8.773-7.113-15.886-15.886-15.886s-15.886,7.113-15.886,15.886v57.058 c-11.042,10.853-20.323,23.267-27.637,36.549H15.886C7.113,137.792,0,144.905,0,153.678c0,50.445,50.811,85.642,98.249,67.288 v78.902c-24.231,15.814-38.92,42.592-38.92,72.051v25.642c0,47.497,38.641,86.139,86.14,86.139h221.063 c47.498,0,86.14-38.641,86.14-86.139v-25.642c0-29.462-14.694-56.242-38.92-72.052v-78.901 C461.232,239.336,512,204.06,512,153.677z M256.221,88.228c-10.365,57.372-63.049,98.196-121.841,93.074 C149.187,126.34,199.212,88.162,256.221,88.228z M130.021,214.213c0-0.472,0.011-0.944,0.016-1.416 c76.41,7.501,145.145-45.73,157.736-120.637c53.146,13.727,94.206,62.646,94.206,122.053v72.938 c-18.231-3.294-244.246-1.398-251.958,0V214.213z M35.027,169.563h69.636c-1.099,3.735-2.125,7.782-2.943,11.621 C81.902,202.533,46.556,196.414,35.027,169.563z M420.898,371.919v25.642h0.001c0,29.978-24.388,54.367-54.368,54.367H145.468 c-29.979,0-54.368-24.388-54.368-54.367v-25.642c0-30.536,24.73-54.367,54.368-54.367h221.063 C396.111,317.553,420.898,341.329,420.898,371.919z M410.3,181.205c-0.831-3.925-1.81-7.809-2.936-11.642h69.609 C465.434,196.438,430.102,202.507,410.3,181.205z"></path> </g> </g> <g> <g> <circle cx="179.916" cy="240.569" r="21.015"></circle> </g> </g> <g> <g> <circle cx="332.729" cy="240.569" r="21.015"></circle> </g> </g> </g></svg>'
    )
  }
}


#' Plotly histogram for valueBox
#'
#' This makes a plotly histogram with transparent background and some extra javascript
#' to make it work with the resizing of a value box.
#'
#' @param df_in Data to plot, usually this is provided by a reactive var
#' @param x The column to plot for the histogram
#' @param x_lab A string to label the x axis with
#'
#' @return a plotly plot
#' @noRd
 fct_plotly_hist_transp <- function(df_in, x, x_lab = "") {
    df_in %>%
     dplyr::mutate(x_new = {{ x }}) %>%
     plotly::plot_ly(x = ~x_new, type = "histogram",marker = list(color = "white")) %>%
     plotly::layout(
       showlegend = FALSE,

       xaxis = list(visible = FALSE, showgrid = FALSE, title = x_lab),
       yaxis = list(visible = FALSE, showgrid = FALSE, title = "Count (log10)", type = "log"),
       hovermode = "x",
       margin = list(t = 0, r = 0, l = 0, b = 0),
       font = list(color = "white"),
       paper_bgcolor = "transparent",
       plot_bgcolor = "transparent"
     ) %>%
     plotly::config(displayModeBar = FALSE) %>%
     # this will modify the axis so that are visible when the value box is expanded
     htmlwidgets::onRender(
       "function(el) {
          var ro = new ResizeObserver(function() {
           var visible = el.offsetHeight > 200;
           Plotly.relayout(el, {
              'xaxis.visible': visible,
              'yaxis.visible': visible
           });
      });
      ro.observe(el);
    }"
     )
  }





