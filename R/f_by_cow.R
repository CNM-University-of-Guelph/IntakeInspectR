

#' Function to iterate each cow's data through the 'by cow' cleaning functions
#'
#' This splits the df_in into a list of data frames, which is parsed to
#' [f_flag_and_replace_outliers()] one at a time. Also can create a log.
#'
#' Error types are: pos = new_y (i.e. new intake), neg = new_x (i.e. new duration), and neg_intake = replaced with 0 kg and neg_duration = new duration. See [f_flag_and_replace_outliers()]
#'
#' @param df_in A data frame, typically that has been processed by [f_by_bin_clean()]
#' @param col_cow_id,col_intake,col_duration,col_bin_id,col_date,col_start_time Column names for the columns in df_in that
#' contain cow ID, intake (kg), duration (sec), feed bin ID, date and feeding event start time.
#' @param sd_thresh The threshold of residual SD to use for flagging points as outliers.
#' @param shiny.session session name, for R Shiny to update progress bar. Use NULL if not using shiny.
#' @param log Boolean. Should log files be generated to a temporary directory?

#'
#' @return A list with the full nested df (nested_out) and a small summary df (outlier_summary)
#' @export

f_iterate_cows <-
  function(df_in,
           col_cow_id = .data$cow_id,
           col_bin_id = .data$feed_bin_id,
           col_date = .data$date,
           col_start_time = .data$start_time,
           col_intake = .data$corrected_intake,
           col_duration = .data$feed_duration,
           sd_thresh = 5,
           shiny.session = NULL,
           log = TRUE){

    if(log){
      tmp <- tempfile(pattern = "log_", fileext = ".log")
      logr::log_open(tmp, logdir = FALSE, show_notes = FALSE)
      print(tmp)
      logr::sep(" Data prep for iteration ")
    } else {
      tmp <- NULL # so that the output list can return tmp as NULL if log == FALSE
    }



  df_nested <-  df_in %>%
    dplyr::mutate(cow_id_nest = {{ col_cow_id }} ) %>%
    tidyr::nest(.by = .data$cow_id_nest)


  # get number of iterations required:
  max_i <- length(df_nested$cow_id_nest)

  if(log){
    logr::log_print("Number of cows to iterate through:")
    logr::log_print(max_i)
    logr::log_print("Residual SD threshold value for determining 'outlier' points:")
    logr::log_print(sd_thresh)
    logr::sep("Flag and replace outliers")
  }


  # setup function that can update progress bar with imap()
  .f_outlier_iterate <-  function(.x, idx){

    out <-
      withCallingHandlers(
        {
                f_flag_and_replace_outliers(.x,
                                       col_intake = {{ col_intake }},
                                       col_duration = {{ col_duration }},
                                       col_cow_id = {{ col_cow_id }},
                                       col_date = {{ col_date }},
                                       col_start_time = {{ col_start_time }},
                                       col_bin_id = {{ col_bin_id }},
                                       sd_thresh = sd_thresh)
        },
        # catch warnings from f_flag_and_replace_outliers and re-write to the logr file
        warning = function(cnd) logr::log_print(conditionMessage(cnd))
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
  return(list(nested_out = nested_out,
              outlier_summary = outlier_summary,
              log_path = tmp))
}




#' f_flag_and_replace_outliers
#'
#' This function executes a 2 step process for outlier detection: \cr\cr
#' 1) A robust linear model is fit using `MASS::rlm` which produces a rough regression
#' that is not sensitive to large outliers. The `sd_thresh` set in this function (default of 5 SD)
#' is used to identify points with a residual >= this threshold and marks them as 'outliers'. \cr
#' 2) Then, a bisector regression is fit to the data that does not include the flagged outliers. This is a
#' symmetrical linear model and can be used to predict both the x- and y- axis. Therefore, the outliers points
#' that have a positive residual are re-fitted using this equation to provide a new intake (y value). Likewise,
#' outliers with a negative residual are re-fitted to provide a new duration (x value).
#'
#' @param df_in data frame of one cow to be cleaned.
#' @param col_intake name of column with intake data to use, normally the corrected intake from 'by bin' functions
#' @param col_duration name of column with duration data to use, normally the corrected duration from 'by bin' functions
#' @param col_cow_id,col_bin_id,col_start_time,col_date Column names for cow_id, feed_bin_id, start_time and date from df_in. Used for ID.
#' @param sd_thresh number to use as threshold for +/- scale (SD) of fitted bisector regression
#'
#'
#' @return A nested data frame with a column called `data` that is the original data given to equation, and `fitted` which contains
#' the output from the fitting processes. These can be merged easily into a flat data frame using `f_merge_corrected_outlier_data()`
#'
#' @export
#'


f_flag_and_replace_outliers <-
  function(df_in,
           col_intake,
           col_duration,
           col_cow_id,
           col_bin_id,
           col_start_time,
           col_date,
           sd_thresh = 5){
    #
    col_y <- rlang::ensym(col_intake)
    col_x <- rlang::ensym(col_duration)

    c_y <- rlang::enquo(col_intake) %>% rlang::as_name()
    c_x <- rlang::enquo(col_duration) %>% rlang::as_name()

    c_cow_id <- rlang::enquo(col_cow_id) %>% rlang::as_name()
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
    # 1. that only 1 cow of data is entered
    # 2. that more than 3 rows of data are included for regression
    ############################################# #

    n_cows <- df_in %>% dplyr::pull({{ col_cow_id }}) %>% dplyr::n_distinct()
    if(n_cows > 1){
      warning("More than 1 cow detected in df_in. This function requires 1 cow at a time, see f_iterate_cows().")
      return(NULL)
    }

    n_row <- df_in %>% nrow()
    if(n_row < 5){
      warning(paste0(
        "Less than 5 events for current cow. Aborting regression fit and keeping original intakes & durations. Cow number:\n",
        df_in %>% dplyr::pull({{ col_cow_id}}) %>% unique(),
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
            !!c_cow_id,
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
    df_in <- df_in %>%
      dplyr::mutate(
        initial_outlier_check = {{ col_intake }} < 0
      )

    # split data into 2 dataframes based on initial flag.
    # Data removed here is re-attached to df after rlm() step
    df_in_rlm <- df_in %>% dplyr::filter(!.data$initial_outlier_check)

    df_in_initial_outliers <- df_in %>% dplyr::filter(.data$initial_outlier_check)


    # INTERCEPT FIXED THROUGH 0:
    rlm_out  <- MASS::rlm(
      rlang::inject(!!col_y ~ 0+!!col_x),
      data = df_in_rlm,
      psi = MASS::psi.huber,
      method = "MM",
      maxit = 250)

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
                !!c_cow_id,
                !!c_bin_id,
                !!c_start_time,
                !!c_date
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
                !!c_cow_id,
                !!c_bin_id,
                !!c_start_time,
                !!c_date
              )
            )
          )) |>
      dtplyr::lazy_dt() %>%
      dplyr::mutate(
        is_outlier = abs(.data$.resid) >= sd_thresh * scale_num | {{ col_intake }} < 0,
        rate_g_min = ({{ col_intake }}*1000)/({{ col_duration }}/60),

        # classify if outlier should be corrected along x or y axis:
        outlier_pos_neg = dplyr::case_when(.data$is_outlier & {{ col_intake }} < 0 ~ "neg_intake",
                                           .data$is_outlier & {{ col_duration }} < 0  ~ "neg_duration",
                                           .data$is_outlier & .data$.resid < 0 ~ "neg",
                                           .data$is_outlier & .data$.resid > 0  ~ "pos")) %>%
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
    b0_bis = means[[col_y]] - b_bis * means[[col_x]]


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
        slope = b_bis

      ) %>%
      dplyr::as_tibble() # required to collect lazy_dt()



    return(predicted_and_corrected)
  }



#' Unnest and merge nested df from f_flag_and_replace_outliers
#'
#' @param nested_df A data fram nested by 'fitted' and 'data'
#'
#' @return a merged data frame
#' @export
f_merge_corrected_outlier_data <- function(nested_df){
  raw_data <- nested_df %>%
    dplyr::select(-"fitted") %>%
    tidyr::unnest(.data$data)

  fitted_data <- nested_df %>%
    dplyr::select(-"data") %>%
    tidyr::unnest(.data$fitted)

  merged_data <- dplyr::left_join(raw_data, fitted_data) %>%
    dplyr::select(-"cow_id_nest") %>%
    # copy columns with more meaningful names:
    dplyr::mutate(
      final_intake_kg = .data$new_y,
      final_duration_sec = .data$new_x
    )

  return(merged_data)

}
