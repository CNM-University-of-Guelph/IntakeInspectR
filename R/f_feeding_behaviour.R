## Feeding Behaviour Analysis ##
# Developed based on Katie Wood lab's template (M.S Williams, 2022)


#' Feeding Behaviour Daily Summary
#'
#' Data grouped by date and animal id, then summary behaviours calculated.
#'
#' Calculated behaviour values, for each day:
#'
#' * intake (kg/d)
#' * time at feeder (min/d)
#' * number visits to feeder per d
#' * mean time per visit (min)
#' * mean visit size (g)
#' * mean eating rate (g/min)
#'
#' Note: The mean eating rate is the mean of each individual event's rate of intake (g/min). This is considered more correct than using the daily mean visit size / mean duration.
#'
#'
#' @param df_in A data frame, typically the final output from cleaning pipeline, using `selected_final_duration_sec` and `selected_final_intake_kg`
#' @param col_animal_id,col_date,col_intake,col_duration,col_rate_of_intake
#'   Column names for the columns in df_in that should be used. Defaults to column names used in Shiny pipeline.

#' @return A dataframe with a row for each date and animal, and columns with new calculated summary values
#' @export


f_behaviour_daily_summary <-
  function(df_in,
           col_animal_id = .data$animal_id,
           col_date = .data$date,
           col_intake = .data$selected_final_intake_kg,
           col_duration = .data$selected_final_duration_sec
           ){

    daily_summary <-
      df_in %>%
      dplyr::mutate(
        # recalculate with selected cols:
        instant_rate_of_intake_kg_min = {{ col_intake }} / {{ col_duration }}*60
      ) %>%
      dplyr::group_by({{ col_date }}, {{ col_animal_id }}) %>%
      dplyr::summarise(
        daily_intake_kg_d = sum({{ col_intake }}, na.rm = TRUE),
        daily_time_at_feeder_min_d = sum(.data$instant_rate_of_intake_kg_min, na.rm = TRUE) ,
        daily_visits_to_feeder_n_d = dplyr::n(),
        daily_mean_time_per_visit_min = .data$daily_time_at_feeder_min_d / .data$daily_visits_to_feeder_n_d,
        daily_mean_visit_size_g = mean({{ col_intake }}, na.rm = TRUE) * 1000,
        daily_mean_eating_rate_g_min = mean(.data$instant_rate_of_intake_kg_min, na.rm = TRUE)*1000,
        # Note: this is not considered correct:
        # daily_mean_eating_rate_incorrect_method = .data$daily_mean_visit_size_g / .data$daily_mean_time_per_visit_min
      )

    return(daily_summary)

  }

#' Combined Meal Analysis
#'
#' Performs analysis by defining meals based on time intervals between feeding events and then summarizing these meals.
#' The identification of meals is based on a specified interval criteria, distinguishing separate meals by the time lapse between feeding events.
#' The `meal_interval_criterion_min` defaults to 5 minutes, meaning that if the interval between a feeding event and the previous event is > 5 minutes it will be defined as a new 'meal'.
#' Then, all feeding events within the same meal are summarised together in the calculated table.
#'
#' Calculated meal values include:
#'
#' * Number of feeding events per meal
#' * Total meal duration (min)
#' * Meal size (g)
#' * Mean eating rate (g/min)
#' * Meal start and end times
#' * Total meal duration (sec and min)
#' * Meal start date
#'
#' @param df_in A data frame, typically the final output from cleaning pipeline, using `selected_final_duration_sec` and `selected_final_intake_kg`, and corrected times.
#' @param meal_interval_criteria Numeric value defining the time interval (in minutes) to distinguish separate meals based on time lapse between feeding events.
#' @param col_animal_id,col_intake,col_duration
#'   Column names for the columns in df_in that should be used. Defaults to column names used in Shiny pipeline.
#' @param col_start_time Column with start time. NOTE: The end_time is calculated based on the `col_duration` provided so that the corrected duration can be used.
#'
#' @return A dataframe with each row as a 'meal' (aggregate of all feeding events within that meal)
#' @export

f_combined_meal_analysis <- function(df_in,
                                     meal_interval_criteria_min = 5,
                                     col_animal_id = .data$animal_id,
                                     col_start_time = .data$start_time,
                                     col_intake = .data$selected_final_intake_kg,
                                     col_duration = .data$selected_final_duration_sec) {

  df_out <-
    df_in %>%
    dplyr::mutate(
      selected_end_time = {{ col_start_time }} + {{ col_duration }},
      # recalculate with selected cols:
      instant_rate_of_intake_kg_min = {{ col_intake }} / {{ col_duration }}
    ) %>%
    # Define meals
    dplyr::group_by({{ col_animal_id }}) %>%
    dplyr::arrange({{ col_start_time }}, .by_group = TRUE) %>%
    dplyr::mutate(
      time_lapse_sec = {{ col_start_time }} - dplyr::lag(.data$selected_end_time, default = dplyr::first({{ col_start_time }})),
      time_lapse_min = as.numeric(.data$time_lapse_sec) / 60,
      new_meal = .data$time_lapse_min > meal_interval_criteria_min,
      meal_group = cumsum(.data$new_meal)
    ) %>%
    dplyr::group_by({{ col_animal_id }}, .data$meal_group) %>%
    dplyr::mutate(
      cumulative_meal_intake_kg = cumsum({{ col_intake }})
    ) %>%

    #  Summarizing individual meals
    dplyr::group_by({{ col_animal_id }}, .data$meal_group) %>%
    dplyr::summarise(
      feeding_events_per_meal_n = dplyr::n(),
      meal_duration_at_bin_min = sum({{ col_duration }}, na.rm = TRUE) / 60,
      meal_size_g = sum({{ col_intake }}, na.rm = TRUE) * 1000,
      meal_mean_eating_rate_g_min = mean(.data$instant_rate_of_intake_kg_min, na.rm = TRUE) * 1000,
      meal_start_time = min({{ col_start_time }}, na.rm = TRUE),
      meal_end_time = max(.data$selected_end_time, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    dplyr::mutate(
      meal_total_duration_sec = .data$meal_end_time - .data$meal_start_time,
      meal_total_duration_min = as.numeric(.data$meal_total_duration_sec) / 60,
      meal_start_date = lubridate::date(.data$meal_start_time),
      meal_start_time_simple = format(.data$meal_start_time, "%H:%M:%S"),
      meal_end_time_simple = format(.data$meal_end_time, "%H:%M:%S")
    )

  return(df_out)
}


#' Daily Meal Summaries
#'
#' Summarizes meal data on a daily basis for each animal, aggregating meal data by animal ID and meal start date.
#'
#' Calculated daily values include:
#'
#' * Number of meals per day
#' * Average meal size (g/day)
#' * Average meal duration at bin (min/day)
#' * Standard deviation of meal duration at bin
#' * Average total meal duration (min/day)
#' * Standard deviation of total meal duration
#' * Average meal rate of intake (g/min)
#' * Standard deviation of meal rate of intake
#'
#' These summaries provide an overview of feeding behavior, highlighting variations in meal size, duration, and intake rates across different days.
#'
#' @param df_in A data frame containing individual meal data.
#' @param col_animal_id,col_meal_start_date,col_meal_size_g,col_meal_duration_min,col_meal_total_duration_min,col_meal_mean_eating_rate_g_min Column names for the
#'  columns in df_in that should be used. Defaults to column names used in Shiny pipeline.
#'
#' @return A dataframe with aggregated daily meal summaries for each animal, detailing the number of meals, and the average and variability of meal sizes, durations, and eating rates.
#' @export

f_daily_meal_summaries <- function(df_in,
                                   col_animal_id = .data$animal_id,
                                   col_meal_start_date = .data$meal_start_date,
                                   col_meal_size_g = .data$meal_size_g,
                                   col_meal_duration_min = .data$meal_duration_at_bin_min,
                                   col_meal_total_duration_min = .data$meal_total_duration_min,
                                   col_meal_mean_eating_rate_g_min = .data$meal_mean_eating_rate_g_min) {

  df_out <- df_in %>%
    dplyr::group_by({{ col_animal_id }}, {{ col_meal_start_date }}) %>%
    dplyr::summarise(
      n_meal_per_date = dplyr::n(),
      mean_meal_size_g_per_date = mean({{ col_meal_size_g }}, na.rm = TRUE),
      mean_duration_at_bin_during_meal_min_per_date = mean({{ col_meal_duration_min }}, na.rm = TRUE),
      sd_duration_at_bin_during_meal_min_per_date = sd({{ col_meal_duration_min }}, na.rm = TRUE),
      mean_total_meal_duration_min_per_date = mean({{ col_meal_total_duration_min }}, na.rm = TRUE),
      sd_total_meal_duration_min_per_date = sd({{ col_meal_total_duration_min }}, na.rm = TRUE),
      mean_meal_rate_of_intake_g_min_per_date = mean({{ col_meal_mean_eating_rate_g_min }}, na.rm = TRUE),
      sd_meal_rate_of_intake_g_min_per_date = sd({{ col_meal_mean_eating_rate_g_min }}, na.rm = TRUE),
      .groups = 'drop'
    )

  return(df_out)
}


#' Final Daily Summary
#'
#' Merges daily behavioral summaries with daily meal summaries into a single
#' comprehensive dataset.
#'
#' @param df_daily_summary A dataframe containing daily behavioral summary data for each animal. Normally calculated by [f_behaviour_daily_summary()]
#' @param df_daily_meal_summaries A dataframe containing daily meal summary data for each animal. Normally calculated by [f_daily_meal_summaries()]
#'
#' @return A dataframe that combines the daily behavioral summaries with daily meal summaries for each animal, matched by animal ID and date.
#' @export
#'
f_final_daily_summary <- function(df_daily_summary,
                                  df_daily_meal_summaries) {

  final_daily_summary <-
    dplyr::full_join(
      df_daily_summary,
      df_daily_meal_summaries,
      by = c("animal_id" = "animal_id",
             "date" = "meal_start_date")) %>%
    dplyr::ungroup()

  return(final_daily_summary)
}

#' Weekly Behaviour Summary
#'
#' Computes weekly summaries from daily data by grouping observations by animal ID and the start of each week.
#' If `reference_date` is not provided, the earliest date in `df_in` is used as the starting point for week calculation.
#' This function aggregates daily measures into weekly averages and totals.
#'
#' @param df_in A data frame containing daily observations.
#' @param reference_date Optional; a Date object to specify the reference start date for week calculations.
#'        If NULL, the minimum date from `df_in` is used.
#' @param col_date,col_animal_id The name of the column in `df_in` that contains date and animal_id information.
#'
#' @return A dataframe with weekly aggregated summaries, including metrics for meal size, duration, and total number
#'         of meals, adjusted to a weekly basis.
#' @export
#'

f_behaviour_summary_weekly <- function(df_in,
                                       reference_date = NULL,
                                       col_date = .data$date,
                                       col_animal_id = .data$animal_id){

  # Checking for a reference date and assigning the earliest date from df_in if NULL
  if(is.null(reference_date)){
    reference_date <- df_in %>% dplyr::pull({{ col_date }}) %>% min(na.rm = TRUE)
  }

  weekly_summary <-
    df_in %>%
    dplyr::mutate(week_starting = lubridate::floor_date({{ col_date }}, unit = "week", week_start = lubridate::wday(reference_date, week_start = 1))) %>%
    dplyr::group_by({{ col_animal_id }}, .data$week_starting) %>%
    dplyr::summarise(
      dplyr::across(dplyr::ends_with('_d'), ~mean(.x, na.rm = TRUE), .names = "{stringr::str_replace(.col, 'daily', 'weekly_mean')}"),
      dplyr::across(dplyr::starts_with('daily_mean'), ~mean(.x, na.rm = TRUE), .names = "{stringr::str_replace(.col, 'daily_mean', 'weekly_mean')}"),
      dplyr::across(tidyselect::all_of(c("mean_meal_size_g_per_date","mean_duration_at_bin_during_meal_min_per_date", "n_meal_per_date")),
             ~mean(.x, na.rm = TRUE),
             .names = "weekly_{stringr::str_replace(.col, 'per_date', 'per_d')}"),
      total_meals_per_week = sum(.data$n_meal_per_date, na.rm = TRUE),
      .groups = 'drop'
    )

  return(weekly_summary)
}

