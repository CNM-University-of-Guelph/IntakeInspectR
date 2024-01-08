############################################################################ #
#' Internal function to count error groups in df
#'
#' @param .df dataframe to use
#' @param ... Names of columns to group by in `dplyr::count()`
#'
#' @noRd

f_count_errors <- function(.df, ...){
  .df %>%
    dplyr::count(...) %>%
    dplyr::mutate(percent = round(.data$n/sum(.data$n)*100,2))
}
############################################################################ #


#' Cleaning - row-by-row within feed bin
#'
#' This script executes a series of individual functions that check for row-by-row
#' errors in automatically collected feed intake data.
#'
#' Most steps group the data by day and by feed bin ID, meaning that each day
#' and feed bin is analysed separately. The final step checks for incorrectly
#' recorded end times and uses the data grouped by cow ID to evaluate if she
#' visited another feed bin while still being recorded at another feed bin.
#' It also generates a log file, summarising each error by count and %. This is
#' stored in a temporary file path which is returned in the output list.
#'
#'
#' @param df_in Data frame to clean.
#' @param zero_thresh Number. What threshold (+/- ____ kg) should be used to
#'   determine an intake is about 0 kg? Default +/- 0.3 kg. For example, an
#'   intake would be considered 'negative' if it was <= 0.3 kg, rather than <= 0
#'   kg.
#' @param feedout_thresh Number. What threshold (kg) should be used to call an
#'   increase in feed intake a feed out event? Default 10 kg
#' @param col_bin_ID Name of column with feed bin ID.
#' @param col_animal_id Name of column with cow ID.
#' @param col_date Name of column with date (formatted as Date) of feeding event.
#' @param col_start_time,col_end_time Name of columns with start time and end time (formatted as POSIXct
#'   date-time) recorded by feed bin.
#' @param col_start_weight_kg,col_end_weight_kg Name of columns with start and end
#'   weights (in kg) recorded by feed bin.
#' @param col_intake Name of column with the feed intake (kg) for the feeding
#'   event, normally calculated as end_weight_kg - start_weight_kg.
#' @param log Boolean. Should log files be generated?
#'
#' @return A list of data frames: `df_0kg`, `df_step2_errors` and `df_cleaned`.
#' In addition, `log_path` is returned in the list, which is the temporary file path where the log is stored.

#' @export
#'
#'
f_by_bin_clean <- function(
    df_in,
    zero_thresh = 0.3,
    feedout_thresh = 10,
    col_bin_ID = .data$bin_id,
    col_animal_id = .data$animal_id,
    col_date = .data$date,
    col_start_time = .data$start_time,
    col_end_time = .data$end_time,
    col_start_weight_kg = .data$start_weight_kg,
    col_end_weight_kg = .data$end_weight_kg,
    col_intake = .data$intake,
    log = TRUE){

  # Use logr package to store logs.
  # saved in tempfile, with filename stored in output for retrieval later

  if(log){

    tmp <- tempfile(pattern = "log_", fileext = ".log")
    print(tmp)

    logr::log_open(tmp, logdir = FALSE, show_notes = FALSE)
    logr::sep("User Input")
    logr::log_print(paste("zero_threshold set to: ", zero_thresh))
    logr::log_print(paste("feedout_threshold set to: ", feedout_thresh))
    logr::sep(" Start of Step 1 - separate 0 kg rows: ")
  } else {
    tmp <- NULL # so that the output list can return tmp as NULL if log == FALSE
  }

  ####################################### #
  # 1. Separate  all rows where intake == 0. ----
  ####################################### #

  ## count rows with zero intake and report n & % to be removed.
  df <- df_in %>%
    dplyr::mutate(is_0kg = {{ col_intake }} == 0)

  if(log){
    logr::log_print("Number of rows with 0 kg intake values:")
    logr::log_print( df %>% dplyr::count(.data$is_0kg) )
  }


  #separate dfs:
  df_0kg <- df %>% dplyr::filter(.data$is_0kg)

  df_no0 <- df %>% dplyr::filter(!.data$is_0kg)


  ###################################### #
  # 2. Flag errors ----
  ###################################### #

  if(log){
    logr::sep(" Start of Step 2 - flag_errors_between_rows: ")
  }

  step2 <- f_step2(df_in = df_no0,
                   {{ col_bin_ID }},
                   {{ col_date }},
                   {{ col_start_time  }},
                   {{ col_start_weight_kg  }},
                   {{ col_end_weight_kg  }},
                   {{ col_intake  }})

  step2_errors <- step2$step2_errors
  step2_ok <- step2$step2_ok


  if(log){
    logr::log_print("Errors from Step2 are removed in this step, but stored for evaluating errors.")
    logr::log_print("Summary of errors between rows:")
    logr::log_print(f_count_errors(step2$step2_full, .data$check_prev_vs_next))
    logr::log_print("Check intakes of error rows (most should be < |0.2| kg):")
    logr::log_print(f_count_errors(step2$step2_full, .data$check_prev_vs_next, .data$classify_errors))
  }

  ###################################### #
  # 3. Check end weights ----
  # Added in additional check for feed_out_event
  ###################################### #

  if(log){
    logr::sep(" Start of Step 3 - check end weights: ")
  }


  step3 <- f_step3(step2_ok,
                   zero_thresh = {{ zero_thresh }},
                   feedout_thresh = {{ feedout_thresh }},
                   {{ col_bin_ID }},
                   {{ col_date }},
                   {{ col_start_time  }},
                   {{ col_start_weight_kg  }},
                   {{ col_end_weight_kg  }},
                   {{ col_intake  }},
                   col_check_prev_vs_next = .data$check_prev_vs_next
                   )


  if(log){
    logr::log_print("Summary of start weight errors:")
    logr::log_print(f_count_errors(step3, .data$check_end_weight_kgs))
    logr::log_print("Summary of corrected end weights:")
    logr::log_print(f_count_errors(step3, .data$category_end_weight_kg))
  }

  ###################################### #
  # 4. Check start weights ----
  ###################################### #

  if(log){
    logr::sep(" Start of Step 4 - check start weights: ")
  }

  step4 <- f_step4(step3,
          zero_thresh = {{ zero_thresh }},
          {{ col_bin_ID }},
          {{ col_date }},
          {{ col_start_time }},
          {{ col_start_weight_kg }},
          {{ col_end_weight_kg }},
          {{ col_intake }},
          col_check_end_weight_kgs = .data$check_end_weight_kgs,
          col_prevEnd = .data$prevEnd,
          col_prevStart = .data$prevStart,
          col_nextStart = .data$nextStart)

  if(log){
    logr::log_print("Summary of end weight errors:")
    logr::log_print(f_count_errors(step4, .data$check_start_weight_kgs))
  }

  ###################################### #
  # 5. Correct intakes ----
  ###################################### #
  if(log){
    logr::sep(" Start of Step 5 - correct feed intakes (by bin): ")
  }


  step5 <- f_step5_correct_intakes(
    step4,
    col_corrected_start_weight_kg = .data$corrected_start_weight_kg_bybin,
    col_corrected_end_weight_kg = .data$corrected_end_weight_kg_bybin,
    col_intake = {{ col_intake }})


  if(log){
    logr::log_print("Total intakes corrected:")
    logr::log_print(f_count_errors(step5, .data$is_corrected_intake_bybin))
    logr::log_print("NOTE: errors result in NA for `corrected_end_weight_kg_bybin`. \n
                    Therefore, errors return the value from  `col_intake` for `corrected_intake` and are counted as FALSE here.")
  }


  ###################################### #
  # 6. Correct end times ----
  ###################################### #
  if(log){
    logr::sep(" Start of Step 6 - correct end times: ")
  }


  step6 <- f_step6_correct_end_times(
    step5,
    col_animal_id = {{ col_animal_id }},
    col_date = {{ col_date }},
    col_start_time = {{ col_start_time }},
    col_end_time = {{ col_end_time }}
  )

  if(log){
    logr::log_print("Summary of end times corrected:")
    logr::log_print(f_count_errors(step6, .data$is_end_time_overlap_error))
  }


  # Finish ----
  if(log){
    logr::log_close()
  }

  return(list(
    df_0kg = df_0kg,
    df_step2_errors = step2_errors,
    df_cleaned = step6,
    log_path = tmp
  ))

}



