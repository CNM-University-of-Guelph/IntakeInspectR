# Note: This code example expects the .rds file that can be downloaded straight after upload
# Note: change the file paths for first and last step as desired.

# IntakeInspectR and its dependencies must be installed. Check version in log file.
# The required packages don't need to be loaded with library() as they are called directly
# for each function, e.g. IntakeInspectR::f_by_bin_clean() means R will use the f_by_bin_clean()
# function from the IntakeInspectR package.

df_uploaded <- readRDS('path/to/file_downloaded_from_upload_page.rds')

# By Bin cleaning:
list_cleaned <-
  IntakeInspectR::f_by_bin_clean(
    df_uploaded,
    zero_thresh = zero_threshold,
    feedout_thresh = feedout_threshold,
    col_bin_ID = bin_id,
    col_animal_id = animal_id,
    col_start_time = start_time,
    col_end_time = end_time,
    col_start_weight_kg = start_weight_kg,
    col_end_weight_kg = end_weight_kg,
    col_intake = intake,
    log = FALSE
  )

# By Animal cleaning:
by_animal_list_out <-
  IntakeInspectR::f_iterate_animals(
    list_cleaned$df_cleaned,
    col_animal_id = animal_id,
    col_bin_id = bin_id,
    col_date = date,
    col_start_time =  start_time,
    # Note, this !!rlang::sym() is only required because we stored the column name as a string above
    col_intake =  !!rlang::sym(user_selected_intake_col_bybin),
    col_duration = !!rlang::sym(user_selected_duration_col_bybin),
    sd_thresh = user_sd_threshold,
    max_duration_min = user_max_duration_min,
    min_intake_rate_kg_min = user_min_intake_rate_kg_min,
    max_intake_rate_kg_min = user_max_intake_rate_kg_min,
    outlier_exemption_max_duration = user_outlier_exemption_max_duration,
    outlier_exemption_max_intake =  user_outlier_exemption_max_intake,
    shiny.session = NULL,
    log = FALSE,
    verbose = FALSE
  )

# Merge the output from By Animal:
merged_by_animal <-
  by_animal_list_out$nested_out %>%
  IntakeInspectR::f_merge_corrected_outlier_data()



# Format the final output to match user selections:
df_final_selections  <-
  merged_by_animal %>%
  # remove prev and next columns
  dplyr::select( !tidyselect::starts_with(c("prev", "next"))) %>%
  dplyr::mutate(
    selected_final_intake_kg = !!rlang::sym(user_selected_final_intake),
    selected_final_duration_sec = !!rlang::sym(user_selected_final_duration)
  )

# Calculate daily intakes (kg/d)
df_daily_intakes <-
  df_final_selections %>%
  dplyr::group_by( animal_id, date ) %>%
  dplyr::summarise(
    daily_intake_kg_asfed = sum(selected_final_intake_kg , na.rm=TRUE)
  )

print(df_daily_intakes)


# Save
readr::write_csv(df_final_selections, file = "path/to/df_final_selections.csv")
readr::write_csv(df_daily_intakes, file = "path/to/df_final_selections.csv")

# END #####################################################################################

