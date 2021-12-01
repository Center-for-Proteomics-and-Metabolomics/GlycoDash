#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  mod_data_import_server("data_import_ui_1")
  mod_spectra_curation_server("spectra_curation_ui_1")
  mod_analyte_curation_server("analyte_curation_ui_1")
  mod_normalization_server("normalization_ui_1")
}
