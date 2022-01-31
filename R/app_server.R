#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  results_data_import <- mod_data_import_server("data_import_ui_1")
  mod_spectra_curation_server(id = "spectra_curation_ui_1", 
                              results_data_import = results_data_import)
  mod_analyte_curation_server("analyte_curation_ui_1")
  mod_normalization_server("normalization_ui_1")
  mod_derived_traits_server("derived_traits_ui_1")
}
