############################################################################ #
# Functions for each step of f_by_bin_clean():

#' By Bin - Step 2 - Check row-by-row errors
#'
#' This function groups data by date and feed bin ID and sorts by start time
#' before adding columns with the previous end weight, previous start weight,
#' next end weight and next start weight. This makes it easy to compare the rows
#' before and after the current row. Then, it checks the difference between the
#' previous end weight and next start weight. If this is 0 kg, then it assumes
#' the current row does not align with the weight data and is therefore an
#' error. The rows flagged with this error are then classified into either:
#' "error type:  intake event <= |0.2|" or "error type:  intake event > |0.2|".
#' This suggests that if the intake is greater than 0.2 or less than -0.2, it
#' might be useful to manually check this classification.
#'
#' This was designed to remove rows where the entry appears to be out of place
#' and typically these entries had a start and end weight that were completely
#' out of line with the surrounding data, as if the entry had been misplaced
#' from another time or feed bin. Without knowing how to deal with this, or if
#' it is even a real event, it is easiest to remove. Doing so also improves the
#' ability of this script to detect row-by-row errors.
#'
#' @param df_in Data frame to clean with the following columns:
#' @param col_bin_ID Name of column with feed bin ID.
#' @param col_date Name of column with date (formatted as Date) of feeding event
#' @param col_start_time Name of column with start time (formatted as POSIXct
#'   date-time) recorded by feed bin.
#' @param col_start_weight_kg,col_end_weight_kg Name of columns with start and end
#'   weights (in kg) recorded by feed bin.
#' @param col_intake Name of column with the feed intake (kg) for the feeding
#'   event, normally calculated as end_weight_kg - start_weight_kg.
#'
#' @return A list of 3 data frames is returned. 1) `step2_full` - with no rows
#'   filtered, 2) The errors are removed and stored in `step2_errors`, and 3)
#'   all rows that are `ok` are returned in `step2_ok` data frame
#'
#'
#' @export
#'

f_step2 <- function(df_in,
                    col_bin_ID,
                    col_date,
                    col_start_time,
                    col_start_weight_kg,
                    col_end_weight_kg,
                    col_intake){

  step2_full <-
    df_in %>%
    dtplyr::lazy_dt() %>%
    dplyr::group_by({{ col_date }}, {{ col_bin_ID }}) %>%
    dplyr::arrange( {{ col_start_time }}) %>%
    dplyr::mutate(
      prevEnd = data.table::shift({{ col_end_weight_kg }}, type = 'lag'),
      prevStart = data.table::shift({{ col_start_weight_kg }}, type = 'lag'),
      nextEnd = data.table::shift({{ col_start_weight_kg }}, type = 'lead'),
      nextStart = data.table::shift({{ col_start_weight_kg}}, type = 'lead'),
      # dplyr versions of lag/lead don't work with lazy_dt()

      # Subtract row below start_weight_kg from row before end_weight_kg (used in check_prev_vs_next)
      prevEnd_vs_nextStart = .data$prevEnd - .data$nextStart,
      check_prev_vs_next = dplyr::case_when(
        # if difference between row above and row below == 0, then current row is error
        prevEnd_vs_nextStart == 0 ~ 'error',

        #Catches all rows that don't meet above conditions (same as  .default = 'ok' which doesn't yet work with dtplyr)
        TRUE ~ 'ok')

    ) %>%
    # quantify errors - most errors should represent rows where |intake| is <0.2 kg
    dplyr::mutate(
      classify_errors = dplyr::case_when(
        check_prev_vs_next == 'error' & abs({{ col_intake }}) <= 0.2 ~ "error type:  intake event <= |0.2|",
        check_prev_vs_next == 'error' & abs({{ col_intake }}) > 0.2 ~ "error type:  intake event > |0.2|",
        TRUE ~ 'ok'
      )
    ) %>%
    tibble::as_tibble() # to collect()


  ############################################## #
  # Separate out check_prev_vs_next errors: ----
  # These need to be removed to re-check row-by-row
  # differences in weights.
  ############################################## #

  # store errors and return in list at end
  df_errors <-
    step2_full %>%
    dplyr::filter(stringr::str_detect(.data$check_prev_vs_next, pattern = "error"))


  df_ok <-
    step2_full %>%
    dplyr::filter(stringr::str_detect(.data$check_prev_vs_next, pattern = "ok"))

  return(list(
    step2_full = step2_full,
    step2_errors = df_errors,
    step2_ok = df_ok
  ))
}




#' By Bin - Step3 - Correct end weights
#'
#' @description This function calculates the various conditions used to check the
#'   original end weights recorded by a feed bin on a single day. This method
#'   chronologically checks weights from the row preceding (above) and following
#'   (below) and compares them to the current row to see if the change in
#'   weights are logical. Based on these check, the function then corrects any
#'   end weights that it can do so safely.
#'
#'   For example, a typical error occurs when the weight recorded at the end of
#'   a feeding event (`end_weight_kg`) is greater than the weight recorded at the
#'   beginning of the feeding event (`start_weight_kg`), resulting in a negative
#'   feed intake. This is likely due to the inability of the sensor (load cell)
#'   to get a stable weight, for example when a cow bumps the feed bin when
#'   exiting. Therefore, if the start weight of the next feeding event
#'   (`nextStart`) is lower than both the `start_weight_kg` and `end_weight_kg`, then
#'   this function flags it as a `replace:` error. This means it is logical to
#'   replace the `end_weight_kg` with `nextStart` as it is assumed that no other
#'   feed was removed between these times as no other feeding events were
#'   recorded for that feed bin.
#'
#'   See details below for full description of other checks.
#'
#'   This function is normally not used by a user, but is used by the
#'   [f_by_bin_clean()] function.
#'
#'
#' @details ## Error categories
#'
#' Firstly, because the errors from [step2] are separated out, the `prevEnd`,
#' `prevStart`, `nextEnd`, `nextStart` are re-calculated without these errors.
#' These errors were flagged for not being consistent with the data above and
#' below them, so removing them improves the ability of this script to detect
#' row-by-row errors.
#'
#' Then, a new column called `check_end_weight_kgs` is added to the original data
#' frame and contains 3 types of categories:
#'
#'    * `error:` - These entries are flagged as errors because they do not make
#' sense when compared to the rows above and below, but can also not be safely
#' replaced using logic. If kept, these intakes can be re-estimated using a
#' regression in `step4`. These entries should be checked manually to determine
#' why these errors occurred. It may be better to exclude them rather than
#' re-estimate them with a regression.
#'    * `keep:` - These entries are considered good and will not be changed.
#' Most entries should be included in this category.
#'    * `replace:` - These entries are flagged to have the `end_weight_kg` replaced
#' with the start weight of the next feeding event (`nextStart`)
#'
#' ## Description of each error type
#'
#' There are various ways that an event can be allocated to a category (`error`,
#' `keep` & `replace`). These are listed in the order they are checked by a call
#' to [dplyr::case_when()].
#'
#' 1. `error: negative with errors`: This error is flagged when the `nextStart`
#' < `end_weight_kg` & `nextStart` > `start_weight_kg`. In this case, the intake is
#' negative but the values are not consistent with the next starting weight and
#' can not be safely replaced.
#'
#' 2. `replace: negative intake`: This is the classic case that is described
#' above in the description of this function where the `end_weight_kg` should be
#' replace by `nextStart`. It is flagged when: `nextStart` < `end_weight_kg` &
#' `nextStart` <= `start_weight_kg` & `intake` <= `-1 * zero_thresh` where
#' `zero_thresh` is the parameter of this function that represents the threshold
#' for determining what should be considered not 0 kg (defaults to 0.3 kg).
#'
#' 3a. `keep: feed removed >= zero_thresh`: This is opposite of `replace:
#' negative intake`, where the `intake` is >= the `zero_thresh`. Then, if the
#' `end_weight_kg` is more than `zero_thresh`  higher than `nextStart` then it indicates
#' feed was removed, and it was more than `zero_thresh`.
#'
#' 3b. `keep: feed removed: minor (< zero_thresh)`: The alternative to 3a is that
#' feed was removed, but that it was basically 0 kg (i.e. < `zero_thresh`)
#'
#' 4a. Feed out:  `error: feed out event with neg intake`:  When the difference
#' between `nextStart` and `end_weight_kg` is greater than the parameter
#' `feedout_thresh`. A reasonable estimate is 10 kg, as it is unlikely that less
#' than 10kg of feed is added, but there are times when this might be different
#' in research. In this case, if the intake is still a negative event
#' (determined using `zero_thresh`) then there's no way to correct it.
#'
#' #' 4b. Feed out: `keep: feed out event`: as above, except when intake is positive.
#'
#' 5. `error: weight increase < feedout_thresh`: This is flagged when the
#' `nextStart` > `end_weight_kg` but the difference is < `feedout_thresh` and >= the
#' inverse of `zero_thresh` (i.e. `-1 * zero_thresh`). The inverse is used because
#' it represents the level of error around 0 kg that can be considered close
#' enough to 0 kg to accept as 'minor'.
#'
#' 6. `replace: minor increase`: As above, except that the difference is between
#' +/- `zero_thresh` and therefore considered 'minor' increase and therefore
#' replacing the `end_weight_kg` with the `nextStart` is more realistic and without
#' significant effects on intake values.
#'
#' 7. `error: negative last row`: The data are stored by day, and the last event
#' for each feed bin on each day can therefore not be checked against the
#' `nextStart` as no data is available. In this case, if the `intake` is
#' negative then it also cannot be corrected.
#'
#' 8. `keep: last row`: When the last row does not have a negative intake, then
#' it is flagged as 'keep'.
#'
#' 9. `keep`: The only rows to remain after all other checks should have an
#' `end_weight_kg` that is equal to `startNext`.
#'
#' 10. `CHECK FUNCTION: no conditions TRUE for check_end_weight_kg` Finally, a
#' default value is included to return this warning for each row that is not
#' caught by any of the previous conditions. This should not be returned, and if
#' it is then it means there is somehow a condition that is unique to the
#' dataset and should be evaluated further.
#'
#' @param df_step2_ok Data frame of `step2_ok` output from [f_step2()]
#' @param zero_thresh Number. What threshold (+/- ____ kg) should be used to
#'   determine an intake is about 0 kg? Default +/- 0.3 kg. For example, an
#'   intake would be considered 'negative' if it was <= 0.3 kg, rather than <= 0
#'   kg.
#' @param feedout_thresh Number. What threshold (kg) should be used to call an
#'   increase in feed intake a feed out event? Default 10 kg
#' @param col_bin_ID Name of column with feed bin ID.
#' @param col_date Name of column with date (formatted as Date) of feeding event
#' @param col_start_time Name of column with start time (formatted as POSIXct
#'   date-time) recorded by feed bin.
#' @param col_start_weight_kg,col_end_weight_kg Name of columns with start and end
#'   weights (in kg) recorded by feed bin.
#' @param col_intake Name of column with the feed intake (kg) for the feeding
#'   event, normally calculated as end_weight_kg - start_weight_kg.
#' @param col_check_prev_vs_next Name of column generated by [f_step2()]
#'   typically called `check_prev_vs_next`
#'
#' @return The original `df_step2` data frame, with the following additional
#'   columns:
#' * prevEnd, prevStart, nextEnd, nextStart - used by check_end_weight_kgs and
#' downstream functions
#' * check_end_weight_kgs - error types as described above
#' * corrected_end_weight_kg_bybin - corrected end weights based on check_end_weight_kgs
#' * category_end_weight_kg - classification of errors (keep, replace, error)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' f_step3(df_step2_ok,
#'   zero_thresh =  -0.3,
#'   feedout_thresh =  10 ,
#'   col_bin_ID = bin_id,
#'   col_date = date,
#'   col_start_time = start_time,
#'   col_start_weight_kg = start_weight_kg,
#'   col_end_weight_kg = end_weight_kg,
#'   col_check_prev_vs_next = check_prev_vs_next
#' )
#' }
#'
f_step3 <-
  function(df_step2_ok,
           zero_thresh = 0.3, # +/- this amount is considered "0 kg"
           feedout_thresh = 10,
           col_bin_ID = .data$bin_id,
           col_date = .data$date,
           col_start_time = .data$start_time,
           col_start_weight_kg = .data$start_weight_kg,
           col_end_weight_kg = .data$end_weight_kg,
           col_intake = .data$intake,
           col_check_prev_vs_next = .data$check_prev_vs_next){

    ############################################## #
    # Execute check_end_rows ----
    ############################################## #

    step3_check <-
      df_step2_ok %>% #this should be a lazy_dt() from above, but collected at end with as_tibble()
      dtplyr::lazy_dt() %>%
      dplyr::group_by({{ col_date }}, {{ col_bin_ID }}) %>%
      dplyr::arrange( {{ col_start_time }}) %>%
      dplyr::mutate(
        # These are repeated as we are using filtered data from step 2, not original data.
        prevEnd = data.table::shift({{ col_end_weight_kg }}, type = 'lag'),
        prevStart = data.table::shift({{ col_start_weight_kg }}, type = 'lag'),
        nextEnd = data.table::shift({{ col_start_weight_kg }}, type = 'lead'),
        nextStart = data.table::shift({{ col_start_weight_kg}}, type = 'lead'),

        check_end_weight_kgs = dplyr::case_when(

          ###############################
          # When nextStart < end_weight_kg:

          # 1. check if start weight of row below is less than end weight but greater than start weight. It should be impossible.
          nextStart < {{ col_end_weight_kg }} &
            nextStart >  {{ col_start_weight_kg }}  ~ 'error: negative with errors',

          # 2. re-check it here to return 'replace_end_weight_kg', as long as 'intake' is negative
          nextStart < {{ col_end_weight_kg }} &
            nextStart <= {{ col_start_weight_kg }} &
            {{ col_intake }} <= (-1 * zero_thresh) ~ 'replace: negative intake',

          # 3a. Opposite of 2 (i.e. >= zero_thresh)
          nextStart < {{ col_end_weight_kg }} &
            nextStart <= {{ col_start_weight_kg }} &
            {{ col_intake }} > (-1 * zero_thresh) &
            # i.e. it wasn't caught above as negative intake, therefore check
            # why nextStart != {{ col_end_weight_kg }}
            # check how much was added:
            ({{ col_end_weight_kg }} - nextStart) >= zero_thresh ~ 'keep: feed removed >= zero_thresh',

          # 3b.
          nextStart < {{ col_end_weight_kg }} &
            nextStart <= {{ col_start_weight_kg }} &
            {{ col_intake }} > (-1 * zero_thresh) &
            # check how much was added:
            ({{ col_end_weight_kg }} - nextStart) < zero_thresh ~ 'keep: feed removed: minor (< zero_thresh)',

          ###############################
          # When nextStart > end_weight_kg:

          # 4a. Define feed out events as an increase in weight of > threshold (default 10 kg)
          nextStart > {{ col_end_weight_kg }} &
            nextStart - {{ col_end_weight_kg }} >= feedout_thresh &
            {{ col_intake }} <= (-1 * zero_thresh) ~ 'error: feed out event with neg intake',

          # 4b. Feed out without negative intake
          nextStart > {{ col_end_weight_kg }} &
            nextStart - {{ col_end_weight_kg }} >= feedout_thresh &
            {{ col_intake }} > (-1 * zero_thresh) ~ 'keep: feed out event',

          # 5. Error when feed value increases between feedout_thresh and zero_thresh:
          nextStart > {{ col_end_weight_kg }} &
            nextStart - {{ col_end_weight_kg }} < feedout_thresh &
            nextStart - {{ col_end_weight_kg }} >= zero_thresh ~ 'error: weight increase < feedout_thresh',


          # 6. if weight is between 0 and 0.3, it's minor error and thus should be replaced :
          nextStart > {{ col_end_weight_kg }} &
            nextStart - {{ col_end_weight_kg }} < zero_thresh &
            nextStart - {{ col_end_weight_kg }} > (-1 * zero_thresh) ~ 'replace: minor increase',


          ###############################
          # Check last row of each day:

          # 7. if last row of the day, then it can be negative but next start weight isn't known
          is.na(nextStart) & {{ col_intake }} < 0 ~ 'error: negative last row',

          # 8. else, if last row and positive - then keep:
          is.na(nextStart) & {{ col_intake }} >= 0 ~ 'keep: last row',

          # 9. finally, if the nextStart == end weight, then keep. Note: dplyr::near is safer than using ==
          dplyr::near(nextStart, {{ col_end_weight_kg }}) ~ 'keep',

          # .default - this value is returned if all other conditions are not TRUE. There shouldn't be any.
          TRUE ~ 'CHECK FUNCTION: no conditions TRUE for check_end_weight_kg')
      ) # return a lazy_dt() which is then collected below.

    ############################################## #
    # Correct end weights ----
    ############################################## #
    step3_corrected <-
      step3_check %>%  # step3_check is already lazy_dt()
      dplyr::mutate(
        corrected_end_weight_kg_bybin = dplyr::case_when(
          stringr::str_detect(check_end_weight_kgs, 'replace') ~ nextStart,
          stringr::str_detect(check_end_weight_kgs, 'keep') ~ {{ col_end_weight_kg }},
          stringr::str_detect(check_end_weight_kgs, 'error') ~ NA_real_,
          stringr::str_detect(check_end_weight_kgs, 'CHECK') ~ NA_real_,

          TRUE ~ -2000 # defaults to an extreme value for easy detection of algorithm.

        ),
        # used for counting and summarising output:
        category_end_weight_kg = dplyr::case_when(
          dplyr::near(.data$corrected_end_weight_kg_bybin, {{ col_end_weight_kg }}) ~ 'keep',
          !dplyr::near(.data$corrected_end_weight_kg_bybin, {{ col_end_weight_kg }}) ~ 'replace',
          is.na(.data$corrected_end_weight_kg_bybin) ~ 'error',
          TRUE ~ 'CHECK'
        )
      ) %>%
      tibble::as_tibble()

    return(step3_corrected)

  }




#' By Bin - Step4 - Correct start weights
#'
#' Occasionally the start weights are recorded incorrectly and can be determined
#' using similar logic to [f_step3()].
#'
#' If there is an abnormality, the column check_start_weight_kgs will classify as
#' either: `'keep: WARNING: start and end weight > prevEnd'` or
#' `'replace: start weight too high'`. Both of these flags check that
#' `prevEnd - start_weight_kg is < 10 kg and > 0.3kg`, based on [f_step2()] thresholds.
#'  Then, they also both check that the end weight is ~ equal to following start
#'  weight (using the `zero_thresh` to determine equality). Therefore, the difference
#'  between the 'keep' and the 'replace' is based on the check of the end weight
#'  with the previous end weight. If the end weight is lower than the previous end
#'  weight, it is assumed that the high start weight is an error that can be replaced
#'  by taking the previous end weight as the new start weight. If this is not true, it
#'  gives the warning: 'start and end weight > prevEnd'.
#'
#'
#'
#' @param df_step3 Data frame returned from [f_step3()]
#' @param zero_thresh Number. What threshold (+/- ____ kg) should be used to
#'   determine an intake is about 0 kg? Default +/- 0.3 kg. For example, an
#'   intake would be considered 'negative' if it was <= 0.3 kg, rather than <= 0
#'   kg.
#' @param col_bin_ID Name of column with feed bin ID.
#' @param col_date Name of column with date (formatted as Date) of feeding event
#' @param col_start_time Name of column with start time (formatted as POSIXct
#'   date-time) recorded by feed bin.
#' @param col_start_weight_kg,col_end_weight_kg Name of columns with start and end
#'   weights (in kg) recorded by feed bin.
#' @param col_intake Name of column with the feed intake (kg) for the feeding
#'   event, normally calculated as end_weight_kg - start_weight_kg.
#' @param col_check_end_weight_kgs,col_prevEnd,col_prevStart,col_nextStart Names of
#'   columns computed by [f_step3()]
#'
#' @return The original data frame with the additional columns:
#'  * prev_step3_check
#'  * check_start_weight_kgs
#'  * corrected_start_weight_kg_bybin
#' @export
#'
f_step4 <-
  function(df_step3,
           zero_thresh = 0.3,
           col_bin_ID = .data$bin_id,
           col_date = .data$date,
           col_start_time = .data$start_time,
           col_start_weight_kg = .data$start_weight_kg,
           col_end_weight_kg = .data$end_weight_kg,
           col_intake = .data$intake,
           col_check_end_weight_kgs = .data$check_end_weight_kgs,
           col_prevEnd,
           col_prevStart,
           col_nextStart){

    step4 <-
      df_step3 %>%
      dtplyr::lazy_dt() %>%
      dplyr::group_by({{ col_date }}, {{ col_bin_ID }}) %>%
      dplyr::arrange( {{ col_start_time }}) %>%
      dplyr::mutate(
        # get the error for the row above from step3
        prev_step3_check = data.table::shift({{ col_check_end_weight_kgs }}, type = 'lag'),

        check_start_weight_kgs = dplyr::case_when(

          stringr::str_detect(.data$prev_step3_check, 'error: weight increase < feedout_thresh') & # same as checking prevEnd - start_weight_kg is < 10 kg and > 0.3kg
            abs({{ col_end_weight_kg }} - {{ col_nextStart }}) <= zero_thresh & # check that the end weight is ~ equal to following start weight (confirming the entry is not completely wrong)
            {{ col_end_weight_kg }} > {{ col_prevEnd }}  ~ 'keep: WARNING: start and end weight > prevEnd', #note: start was checked by first check

          stringr::str_detect(.data$prev_step3_check, 'error: weight increase < feedout_thresh') & # same as checking prevEnd - start_weight_kg is < 10 kg and > 0.3kg
            abs({{ col_end_weight_kg }} - {{ col_nextStart }}) <= zero_thresh & # check that the end weight is ~ equal to following start weight (confirming the entry is not completely wrong)
            {{ col_end_weight_kg }} <= {{ col_prevEnd }}  ~ 'replace: start weight too high',

          TRUE ~ 'keep'
        ),

        corrected_start_weight_kg_bybin = dplyr::case_when(
          stringr::str_detect(.data$check_start_weight_kgs, 'replace') ~ {{ col_prevEnd }},
          stringr::str_detect(.data$check_start_weight_kgs, 'keep') ~ {{ col_start_weight_kg }},
          TRUE ~ -2000 # return extreme default for fault finding if case_when doesn't catch all cases
        )
      ) %>% tibble::as_tibble()

    return(step4)
  }




#' By Bin - Step 5 - Correct intakes
#'
#' This function calculates a corrected intake as corrected_start_weight_kg_bybin - corrected_end_weight_kg_bybin.
#' It also handles the situation where errors are produced by [f_step3()] which means a corrected
#' intake cannot be calculated, and therefore it returns the original intake.
#'
#' @param df_step4 Data frame produced by [f_step4()]
#' @param col_corrected_end_weight_kg Name of column with corrected end weight (normally produced by [f_step3()])
#' @param col_corrected_start_weight_kg Name of column with corrected start weight (normally produced by [f_step4()])
#' @param col_intake Name of column with the feed intake (kg) for the feeding
#'   event, normally calculated as end_weight_kg - start_weight_kg.
#'
#' @return data frame of df_step4 with the following additional columns:
#' * corrected_intake_bybin = NOTE: col_intake returned when NA (from error)
#' * is_corrected_intake_bybin = boolean indicating if intake was corrected at all
#' @export
#'
f_step5_correct_intakes <-
  function(df_step4,
           col_corrected_end_weight_kg,
           col_corrected_start_weight_kg,
           col_intake){

    df_step5 <-
      df_step4 %>%
      dtplyr::lazy_dt() %>%
      dplyr::mutate(
        corrected_intake_bybin = dplyr::if_else(
          is.na({{ col_corrected_end_weight_kg }}), #if NA it means it's an error
          true = {{ col_intake }}, # if error (i.e. not 'replace'), return original intake
          false = {{ col_corrected_start_weight_kg }} - {{ col_corrected_end_weight_kg }}),

        is_corrected_intake_bybin = !dplyr::near(.data$corrected_intake_bybin, {{ col_intake }} )
      ) %>% tibble::as_tibble()

  }


#' By Bin - Step 6 - Correct end times
#'
#' This function checks for errors in the time recorded by the feed bin without making
#' any assumptions about a biologically possible rate of intake. This is technically
#' a 'by cow' step, as it compares start and end times within a cow (across feed bins)
#' to see if the cow visited another feed bin while still being recorded as present
#' at a feed bin (i.e. overlapping events). If this is found, it will trim the first
#' event to end 10 sec before the next recorded feeding event.
#'
#' @param df_step5 Data frame produced by [f_step5()]
#' @param col_animal_id Name of column with cow ID.
#' @param col_date Name of column with date (formatted as Date) of feeding event
#' @param col_start_time,col_end_time Name of column with start and end time (formatted as POSIXct
#'   date-time) recorded by feed bin.

#' @return The original data frame with the following additional columns:
#' nextStartTime, is_end_time_overlap_error, corrected_end_time, corrected_duration_sec_diff, corrected_duration_sec_seconds
#' @export
#'
f_step6_correct_end_times <-
  function(df_step5,
           col_animal_id = .data$animal_id,
           col_date = .data$date,
           col_start_time = .data$start_time,
           col_end_time = .data$end_time){

    df_step5 %>%
      dtplyr::lazy_dt() %>%
      dplyr::group_by({{ col_date }}, {{ col_animal_id }}) %>%
      dplyr::arrange( {{ col_start_time }}) %>%
      dplyr::mutate(
        nextStartTime = data.table::shift({{ col_start_time}}, type = 'lead'),
        # is the end time later in time than the time of the next feeding event by the cow
        is_end_time_overlap_error = dplyr::if_else(
          is.na(.data$nextStartTime), # check if NA - NA due to last event of day
          true = FALSE, # if last event of day, assume no error (i.e. FALSE)
          false = {{ col_end_time }} > .data$nextStartTime),

        corrected_end_time = dplyr::if_else(
          .data$is_end_time_overlap_error,
          true = .data$nextStartTime - lubridate::seconds(10),
          false = {{ col_end_time }}),

        corrected_duration_sec_diff = .data$corrected_end_time - {{ col_start_time }},
        corrected_duration_sec_seconds = as.numeric(.data$corrected_duration_sec_diff)
      ) %>% tibble::as_tibble()
  }
