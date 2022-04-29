
#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  # Increasing the maximum size of files that can be uploaded:
  options(shiny.maxRequestSize=30*1024^2)
  
  results_data_import <- mod_data_import_server("data_import_ui_1")
  
  results_spectra_curation <- mod_spectra_curation_server(
    id = "spectra_curation_ui_1", 
    results_data_import = results_data_import)
  
  results_analyte_curation <- mod_analyte_curation_server(
    id = "analyte_curation_ui_1",
    results_spectra_curation = results_spectra_curation)
  
  results_normalization <- mod_normalization_server(
    id = "normalization_ui_1",
    results_analyte_curation = results_analyte_curation)
  
  results_derived_traits <- mod_derived_traits_server(
    id = "derived_traits_ui_1",
    results_normalization = results_normalization)
  
  results_repeatability <- mod_repeatability_server(
    id = "repeatability_ui_1",
    results_normalization = results_normalization,
    results_data_import = results_data_import
  )
  
  results_data_exploration <- mod_data_exploration_server(
    id = "data_exploration_ui_1",
    results_derived_traits = results_derived_traits)
  
  mod_export_server(
    id = "export_ui_1",
    results_derived_traits = results_derived_traits,
    results_data_import = results_data_import,
    results_spectra_curation = results_spectra_curation,
    results_analyte_curation = results_analyte_curation,
    results_repeatability = results_repeatability,
    results_data_exploration = results_data_exploration
    )
  
}
