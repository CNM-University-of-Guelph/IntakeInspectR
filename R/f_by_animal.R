

#' Function to iterate each animal's data through the 'by animal' cleaning functions
#'
#' This splits the df_in into a list of data frames, which is parsed to
#' [f_flag_and_replace_outliers()] one at a time. Also can create a log.
#'
#' Error types are: pos = new_y (i.e. new intake), neg = new_x (i.e. new
#' duration), and neg_intake = replaced with 0 kg and neg_duration = new
#' duration. See [f_flag_and_replace_outliers()]
#'
#' @param df_in A data frame, typically that has been processed by
#'   [f_by_bin_clean()]
#' @param col_animal_id,col_duration,col_bin_id,col_date,col_start_time,col_intake,
#'   Column names for the columns in df_in that contain animal ID, intake (kg),
#'   duration (sec), feed bin ID, date and feeding event start time.
#' @param max_duration_min number of minutes. Events with a duration longer than
#'   this will be classed as a 'manual outlier'
#' @param min_intake_rate_kg_min,max_intake_rate_kg_min number (kg/min). Events
#'   with a rate of intake (i.e. intake / duration) greater than the
#'   `max_intake_rate_kg_min` and less than the `min_intake_rate_kg_min` will be
#'   classed as a 'manual outlier'
#' @param outlier_exemption_max_duration,outlier_exemption_max_intake number (kg
#'   or min). Events below this `outlier_exemption_max_intake` (kg) & with a
#'   duration less than `outlier_exemption_max_duration` (min) are exempt from
#'   other outlier detection methods.
#' @param sd_thresh number to use as threshold for +/- scale (SD) of fitted
#'   bisector regression
#' @param shiny.session session name, for R Shiny to update progress bar. Use
#'   NULL if not using shiny.
#' @param log Boolean. Should log files be generated to a temporary directory?
#'
#' @return A list with the full nested df (nested_out) and a small summary df
#'   (outlier_summary)
#' @export

f_iterate_animals <-
  function(df_in,
           col_animal_id = .data$animal_id,
           col_bin_id = .data$bin_id,
           col_date = .data$date,
           col_start_time = .data$start_time,
           col_intake = .data$corrected_intake,
           col_duration = .data$duration_sec,
           max_duration_min = 60,
           min_intake_rate_kg_min = 0.05,
           max_intake_rate_kg_min = 1.5,
           outlier_exemption_max_duration  = 1,
           outlier_exemption_max_intake = 0.2,
           sd_thresh = Inf,
           shiny.session = NULL,
           log = TRUE,
           verbose = TRUE){

    if(log){
      tmp <- tempfile(pattern = "log_", fileext = ".log")
      logr::log_open(tmp, logdir = FALSE, show_notes = FALSE)
      print(tmp)
      logr::sep(" Data prep for iteration ")
    } else {
      tmp <- NULL # so that the output list can return tmp as NULL if log == FALSE
    }



  df_nested <-  df_in %>%
    dplyr::mutate(animal_id_nest = {{ col_animal_id }} ) %>%
    tidyr::nest(.by = .data$animal_id_nest)

  #get names of columns for log, and returns as a string (as_name())
  c_intake <- rlang::enquo(col_intake) %>% rlang::as_name()
  c_duration <- rlang::enquo(col_duration) %>% rlang::as_name()


  # get number of iterations required:
  max_i <- length(df_nested$animal_id_nest)

  if(log){
    logr::log_print(paste("Number of animals to iterate through:", max_i))

    logr::log_print(paste("User Input: Selected intake column:", c_intake))

    logr::log_print(paste("User Input: Selected duration column:", c_duration))

    logr::log_print(paste("User Input: Maximum allowable duration (min) per feeding event:", max_duration_min))

    logr::log_print(paste("User Input: Minimum allowable rate of intake (kg/min) per feeding event:", min_intake_rate_kg_min))

    logr::log_print(paste("User Input: Maximum allowable rate of intake (kg/min) per feeding event:", max_intake_rate_kg_min))

    logr::log_print(paste("User Input: Ignore fixing outliers with durations (minutes) less than:", outlier_exemption_max_duration,
                          "and with intake (kg) less than:", outlier_exemption_max_intake))

    logr::log_print(paste("User Input: Residual SD threshold value for determining 'outlier' points:", sd_thresh))




    logr::sep("Flag and replace outliers")
  }


  # setup function that can update progress bar with imap()
  .f_outlier_iterate <-  function(.x, idx){

    if(verbose){
      current_animal <- .x %>% dplyr::pull( {{ col_animal_id }}) %>% unique()
      logr::log_print(paste("'By Animal' current iteration = ", current_animal, "Execution time:",format(Sys.time(), "%Y-%m-%d %H:%M:%OS3")))
    }

    out <-
      withCallingHandlers(
        {
                f_flag_and_replace_outliers(.x,
                                       col_intake = {{ col_intake }},
                                       col_duration = {{ col_duration }},
                                       col_animal_id = {{ col_animal_id }},
                                       col_bin_id = {{ col_bin_id }},
                                       col_start_time = {{ col_start_time }},
                                       col_date = {{ col_date }},
                                       max_duration_min = max_duration_min,
                                       min_intake_rate_kg_min = min_intake_rate_kg_min,
                                       max_intake_rate_kg_min = max_intake_rate_kg_min,
                                       outlier_exemption_max_duration  = outlier_exemption_max_duration,
                                       outlier_exemption_max_intake = outlier_exemption_max_intake,
                                       sd_thresh = sd_thresh)
        },
        # catch warnings from f_flag_and_replace_outliers and re-write to the logr file
        warning = function(cnd) {

          #Print warnings to log, but not if it is the warning from lqs.default as this is irrelevant.
          if(log & !any(grepl("lqs.default", conditionCall(cnd)))){
            logr::log_print(conditionMessage(cnd))
          } else {
            print(conditionMessage(cnd))
          }

          }
      )


    if(!is.null(shiny.session)){
    shinybusy::update_modal_progress(value = idx / max_i, session = shiny.session)
    }

    return(out)
  }

  # execute and iterate through
  nested_out <- df_nested %>%
    dplyr::mutate(
      fitted =  purrr::imap(data, ~.f_outlier_iterate(.x, .y))
    )

  outlier_summary <-   nested_out %>%
    dplyr::select(-data) %>%
    tidyr::unnest('fitted') %>%
    dplyr::mutate(dplyr::across('outlier_pos_neg', ~tidyr::replace_na(.x, 'not_error'))) %>%
    f_count_errors(.data$outlier_pos_neg)



  # report values
  if(log){
    logr::log_print("Number of outliers detected:")
    logr::log_print(  outlier_summary )
  }




  # need to return tmp as well
  return(
    list(
      nested_out = nested_out,
      outlier_summary = outlier_summary,
      log_path = tmp,
      # store user selected column names for intake and duration so it can be parsed for plotting
      selected_intake_column = c_intake,
      selected_duration_column = c_duration
    )
  )
  }




#' f_flag_and_replace_outliers
#'
#' This function executes a 3 step process for outlier detection, detailed below.
#'
#' 1) User-defined 'manual outliers': Firstly, individual feeding events that are
#' less than or equal to the `outlier_exemption_max_duration` and
#' `outlier_exemption_max_intake` are classified as 'exempt' from further
#' outlier detection. This is useful as many events occur with low intakes (e.g.
#' < 0.3 kg) and short durations (e.g. < 1 min) that may otherwise be flagged as
#' an outlier when in reality it may be preferable to leave them as-is,
#' particularly as they have a relatively minor influence on total daily
#' intakes. Then, the rates of intake for each point (intake/duration, kg/min)
#' are used to flag biologically unrealistic feeding events by setting a
#' `min_intake_rate_kg_min` and `max_intake_rate_kg_min`. For most users the max
#' intake rate is most important, as events with a higher rate of intake than
#' `max_intake_rate_kg_min` will have a new (lower) intake estimated based on
#' the measured duration. Whereas, events that have a rate of intake lower than
#' the `min_intake_kg_min` will have a new (shorter) duration estimated from the
#' measured feed intake. This change in duration is less relevant for most
#' users, and it is also important to remember that it is theortically possible
#' that animals can eat very slowly, but there will always be an upper limit to how
#' fast they can eat. Finally, some very long durations may also have a high
#' enough intake that they are not flagged by the `min_intake_kg_min` but are
#' likely not a realistic duration. Therefore, any events with a duration that
#' were not previously flagged and have a duration longer than
#' `max_duration_min` are flagged to get a new (lower) duration estimated from
#' the measured intake.
#' Overall, this step provides a way of filtering feeding events based on
#' biologically relevant limitations to what a animal might be expected to normally
#' do.\cr
#'
#' 2) As durations get longer, the range of possible feed intakes increases, so
#' it might be important for some users to still use a residual SD outlier
#' detection method to flag potential outliers from what a animal 'normally' does.
#' This should be done with caution as animals can behave in 'abnormal' ways for
#' very valid reasons. For example, a animal might consume a larger meal at a
#' relatively fast rate of intake if it has been previously been restricted for
#' some reason, such as an experimental treatment. Due to potentially large
#' outliers, a robust linear model is fit first using `MASS::rlm` The
#' `sd_thresh` set in this function is then used to identify points with a
#' residual >=  this threshold and marks them as 'outliers'. \cr
#'
#' 3) To estimate new values for outliers, a bisector regression is fit to the
#' data that does not include the flagged outliers. This is a symmetrical linear
#' model and can be used to predict both the x- and y- axis. Therefore, the
#' outliers points that have a positive residual are re-fitted using this
#' equation to provide a new intake (y value). Likewise, outliers with a
#' negative residual are re-fitted to provide a new duration (x value).
#'
#' @param df_in data frame of one animal to be cleaned.
#' @param col_intake name of column with intake data to use, normally the
#'   corrected intake from 'by bin' functions
#' @param col_duration name of column with duration data to use, normally the
#'   corrected duration from 'by bin' functions. Must be numbers of seconds.
#' @param col_animal_id,col_bin_id,col_start_time,col_date Column names for animal_id,
#'   bin_id, start_time and date from df_in. Used for ID.
#' @param max_duration_min number of minutes. Events with a duration longer than
#'   this will be classed as a 'manual outlier'
#' @param min_intake_rate_kg_min,max_intake_rate_kg_min number (kg/min). Events
#'   with a rate of intake (i.e. intake / duration) greater than the
#'   `max_intake_rate_kg_min` and less than the `min_intake_rate_kg_min` will be
#'   classed as a 'manual outlier'
#' @param outlier_exemption_max_duration,outlier_exemption_max_intake number (kg
#'   or min). Events below this `outlier_exemption_max_intake` (kg) & with a
#'   duration less than `outlier_exemption_max_duration` (min) are exempt from
#'   other outlier detection methods.
#' @param sd_thresh number to use as threshold for +/- scale (SD) of fitted
#'   bisector regression
#'
#'
#' @return A nested data frame with a column called `data` that is the original
#'   data given to equation, and `fitted` which contains the output from the
#'   fitting processes. These can be merged easily into a flat data frame using
#'   `f_merge_corrected_outlier_data()`
#'
#' @export
#'

 f_flag_and_replace_outliers <-
   function(df_in,
            col_intake = .data$corrected_intake_bybin,
            col_duration = .data$corrected_duration_sec, # must be seconds
            col_animal_id = .data$animal_id,
            col_bin_id = .data$bin_id,
            col_start_time = .data$start_time,
            col_date = .data$date,
            max_duration_min = 60,
            min_intake_rate_kg_min = 0.05,
            max_intake_rate_kg_min = 1.5,
            outlier_exemption_max_duration  = 1,
            outlier_exemption_max_intake = 0.2,
            sd_thresh = Inf){

     col_y <- rlang::ensym(col_intake)
     col_x <- rlang::ensym(col_duration)

     c_y <- rlang::enquo(col_intake) %>% rlang::as_name()
     c_x <- rlang::enquo(col_duration) %>% rlang::as_name()

     c_animal_id <- rlang::enquo(col_animal_id) %>% rlang::as_name()
     c_bin_id <- rlang::enquo( col_bin_id ) %>% rlang::as_name()
     c_start_time <- rlang::enquo( col_start_time ) %>% rlang::as_name()
     c_date <- rlang::enquo( col_date ) %>% rlang::as_name()

     # drop NA values - rlm() won't work otherwise
     # There shouldn't be NA value, as errors from first step retained their original intake values
     df_in <-
       df_in %>%
       tidyr::drop_na({{ col_intake }},  {{ col_duration }})

     ############################################# #
     # Check data
     # 1. that only 1 animal of data is entered
     # 2. that more than 3 rows of data are included for regression
     ############################################# #

     n_animals <- df_in %>% dplyr::pull({{ col_animal_id }}) %>% dplyr::n_distinct()
     if(n_animals > 1){
       warning("More than 1 animal detected in df_in. This function requires 1 animal at a time, see f_iterate_animals().")
       return(NULL)
     }

     n_row <- df_in %>% nrow()
     if(n_row < 5){
       warning(cat(
         "Less than 5 events for current animal. Aborting regression fit and keeping original intakes & durations. Animal number:\n",
         df_in %>% dplyr::pull({{ col_animal_id}}) %>% unique(),
         "\nNumber of rows:\n",
         n_row
       ))

       # copy original values to new columns that would be added below
       # keep only minimum amount of columns to allow data to be merged with 'fitted' df's
       out <- df_in %>%
         dplyr::mutate(
           new_x = {{ col_duration }},
           new_y = {{ col_intake }}
         ) %>%
         dplyr::select( tidyselect::all_of(
           #all_of returns an error if some are missing, but requires char vector, hence the enquo(), as_name() + bang-bang
           c(
             !!c_animal_id,
             !!c_bin_id,
             !!c_start_time,
             !!c_date,
             !!c_x,
             !!c_y,
             'new_x',
             'new_y'
           )
         )) %>%
         dplyr::mutate(
           is_outlier = FALSE,
           outlier_pos_neg = 'insufficient_rows_for_regression'
         )

       return(out)
     }


     ############################################# #
     # Fit robust linear model to detect outliers
     ############################################# #

     # Set negative intakes and long durations (> 60 min) as outliers prior to first regression
     # This helps to maintain a more robust linear model, as very negative intakes or very long durations will still
     # skew the rlm().


     df_in <-
       df_in %>%
       dplyr::mutate(instant_rate_of_intake_kg_s = {{ col_intake }} / {{ col_duration }},
                     instant_rate_of_intake_kg_min = .data$instant_rate_of_intake_kg_s * 60) %>%
       dplyr::arrange(dplyr::desc(.data$instant_rate_of_intake_kg_s)) %>%

       # ???
       # dplyr::filter(.data$instant_rate_of_intake_kg_min >= 0) %>%

       dplyr::mutate(
         manual_outlier_classification = dplyr::case_when(
           # flag negatives (via negative rates of intake)
           instant_rate_of_intake_kg_min < 0 ~ 'Negative Intake',

           # ignore <= 60 seconds and < 300 g
           {{ col_duration }} <= outlier_exemption_max_duration*60 &  {{ col_intake }} <= outlier_exemption_max_intake ~ 'Not Outlier',

           instant_rate_of_intake_kg_min > max_intake_rate_kg_min ~ paste0(">", max_intake_rate_kg_min, "kg/min"),
           # instant_rate_of_intake_kg_min > 2 ~ '>2 kg/min',
           instant_rate_of_intake_kg_min < min_intake_rate_kg_min ~ paste0("<", min_intake_rate_kg_min, "kg/min"),
           # instant_rate_of_intake_kg_min < 0.075 ~ '<0.075 kg/min',
           # instant_rate_of_intake_kg_min < 0.1 ~ '<0.100 kg/min',
           {{ col_duration }} > (max_duration_min * 60) ~ paste0("Long Duration (>", max_duration_min, " min)"),

           .default = 'Not Outlier'
         ),
         is_manual_outlier = .data$manual_outlier_classification != 'Not Outlier'
       )

     # split data into 2 dataframes based on initial flag.
     # Data removed here is re-attached to df after rlm() step
     df_in_rlm <- df_in %>% dplyr::filter(!.data$is_manual_outlier)

     df_in_initial_outliers <- df_in %>% dplyr::filter(.data$is_manual_outlier)


     # # INTERCEPT FIXED THROUGH 0:
     rlm_out  <- MASS::rlm(
       rlang::inject(!!col_y ~ 0+!!col_x),
       data = df_in_rlm,
       psi = MASS::psi.huber,
       method = "M",
       init = 'lts',
       maxit = 5000)


     # value from model used as 'sd' for outlier detection
     scale_num <-  rlm_out$s

     resid_df_outliers <-
       broom::augment(rlm_out, newdata = stats::model.frame(rlm_out))  %>%
       dplyr::bind_cols(
         df_in_rlm %>%
           dplyr::select(
             #all_of returns an error if some are missing, but requires char vector, hence the enquo(), as_name() + bang-bang
             tidyselect::all_of(
               c(
                 !!c_animal_id,
                 !!c_bin_id,
                 !!c_start_time,
                 !!c_date,
                 "manual_outlier_classification",
                 "is_manual_outlier",
                 "instant_rate_of_intake_kg_min"
               )
             )
           )
       )  %>%
       # Add initial outliers back in:
       dplyr::bind_rows(
         df_in_initial_outliers %>%
           dplyr::select(
             tidyselect::all_of(
               c(
                 !!c_y,
                 !!c_x,
                 !!c_animal_id,
                 !!c_bin_id,
                 !!c_start_time,
                 !!c_date,
                 "manual_outlier_classification",
                 "is_manual_outlier",
                 "instant_rate_of_intake_kg_min"
               )
             )
           )) %>%
       dtplyr::lazy_dt() %>%
       dplyr::mutate(
         is_outlier = abs(.data$.resid) >= sd_thresh * scale_num | {{ col_intake }} < 0 | .data$is_manual_outlier == TRUE

       ) %>%
       dplyr::as_tibble() # required to collect lazy_dt()


     ##############################################
     # Fit bisector linear model that is symmetrical
     ##############################################

     # This allows for bi-directional prediction of either x or y
     # similar to SMA but easier to implement

     # select only points that were not considered outliers above to fit bisector line to
     # this is only used for calculation, and all data is still returned at end
     df_no_outliers <- resid_df_outliers  %>% dplyr::filter(.data$is_outlier == FALSE)


     # These methods are faster than others for getting values required:
     # uses names that are similar to Isobe 1990 paper

     # mean of x and y
     means <- df_no_outliers %>%
       dplyr::select({{ col_x }}, {{ col_y }}) %>%
       base::colMeans(na.rm=TRUE)

     # sum of squares calculations:
     SS <- df_no_outliers  %>%
       dplyr::select({{ col_x }}, {{ col_y }}) %>%
       dplyr::mutate(
         s_xy = ({{ col_x }} - means[[col_x]] ) *({{ col_y }} - means[[col_y]]),
         s_xx = ({{ col_x }} - means[[col_x]])^2,
         s_yy = ({{ col_y }} - means[[col_y]])^2)

     # Total sum of squares
     TSS <- SS %>% base::colSums()

     # Calculate slope and intercept for bisector model from Isobe 1990:
     b_xy <- TSS[['s_xy']] / TSS[['s_xx']] # normal lm
     b_yx <- TSS[['s_yy']] / TSS[['s_xy']] # inverse - used by bis

     # slope:
     b_bis <-  (b_xy + b_yx)^-1*(b_xy * b_yx - 1 + sqrt((1+b_xy^2)*(1+b_yx^2)))

     # Intercept:
     #b0_bis = means[[col_y]] - b_bis * means[[col_x]]

     # Force intercept to 0 as this is biologically more relevent and prevents estimating negative intakes
     b0_bis = 0

     ####################################################
     # Fit model for outliers:
     ####################################################
     # functions to predict in either direction
     .f_pred_y <- function(x){
       y = b_bis * x  + b0_bis
       return(y)
     }

     .f_pred_x <- function(y){
       x = (y - b0_bis) / b_bis
       return(x)
     }


     predicted_and_corrected <-
       resid_df_outliers %>%
       dtplyr::lazy_dt() %>%
       dplyr::mutate(
         # calculate predicted values for plotting regression
         predicted_y_bisector = b_bis * {{ col_x }} + b0_bis,
         residual_y_bisector = {{ col_intake }} - .data$predicted_y_bisector,

         # classify if outlier should be corrected along x or y axis:
         outlier_pos_neg = dplyr::case_when(.data$is_outlier & {{ col_intake }} < 0 ~ "neg_intake",
                                            .data$is_outlier & {{ col_duration }} < 0  ~ "neg_duration",
                                            .data$is_outlier & .data$residual_y_bisector < 0 ~ "neg",
                                            .data$is_outlier & .data$residual_y_bisector > 0  ~ "pos"),

         # New x and y - with outliers given predicted values:
         new_y = dplyr::case_match(.data$outlier_pos_neg,
                                   'pos' ~ .f_pred_y({{ col_duration }}),
                                   'neg_intake' ~ 0, #set neg intakes to 0
                                   .default =  {{ col_intake }} ),

         new_x = dplyr::case_match(.data$outlier_pos_neg,
                                   'neg' ~ .f_pred_x({{ col_intake }}),
                                   'neg_duration' ~ .f_pred_x({{ col_intake }}),
                                   .default = {{ col_duration }}),
         # store b0 and b_bis in columns to access later for rate of intake
         intercept = b0_bis,
         slope = b_bis,

       ) %>%
       dplyr::as_tibble() %>%  # required to collect lazy_dt()
       dplyr::mutate(
         #add final classification of all outliers
         outlier_classification = dplyr::case_when(
           is_manual_outlier == FALSE & is_outlier == TRUE ~ 'Residual > SD threshold',
           is_manual_outlier == TRUE ~ manual_outlier_classification,
           is_outlier == FALSE ~ "Not Outlier",
           .default = NA_character_
         )
       )



     # return(list(df = predicted_and_corrected, rlm =  rlm_out, df_no_outliers = df_no_outliers))
     return(predicted_and_corrected)
   }


#' Unnest and merge nested df from `f_flag_and_replace_outliers`
#'
#' @param nested_df A data frame nested by 'fitted' and 'data'
#'
#' @return a single, unnested data frame of fitted and original data
#' @export
f_merge_corrected_outlier_data <- function(nested_df){
  raw_data <- nested_df %>%
    dplyr::select(-"fitted") %>%
    tidyr::unnest("data")

  fitted_data <- nested_df %>%
    dplyr::select(-"data") %>%
    tidyr::unnest("fitted")

  merged_data <- dplyr::left_join(raw_data, fitted_data) %>%
    dplyr::select(-"animal_id_nest") %>%
    # copy columns with more meaningful names:
    dplyr::mutate(
      final_intake_kg = .data$new_y,
      final_duration_sec = .data$new_x
    )

  return(merged_data)

}
