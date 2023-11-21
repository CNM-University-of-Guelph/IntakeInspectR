#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  ## Set maximum file size for upload to 100MB
  options(shiny.maxRequestSize = 250*1024^2)

  mod_welcome_server("welcome_1", parent_session = session) # parent_session used for nav button

  df_uploaded <- mod_uploads_server("uploads_1")

  bybin_out <- mod_by_bin_clean_server("by_bin_clean_1", df = df_uploaded)

  mod_by_bin_vis_server(id = "by_bin_vis_1", df_list = bybin_out)

  bycow_out <-  mod_by_cow_clean_server("by_cow_clean_1", df_list = bybin_out)

  mod_by_cow_vis_server("by_cow_vis_1", df_list = bycow_out)

  mod_final_summary_server("final_summary_1", df_list_bybin = bybin_out, df_list_bycow = bycow_out)



}
