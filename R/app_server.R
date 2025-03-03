
#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  # Increasing the maximum size of files that can be uploaded to 200 MB:
  options(shiny.maxRequestSize=200*1024^2)
  
  # Download changelog
  output$download_changelog <- downloadHandler(
    filename = function() {
      "GlycoDash_changelog.html"
    },
    content = function(file) {
      # Convert the md_content to HTML
      md_file <- system.file("app", "www", "NEWS.md", package = "GlycoDash")
      html_content <- markdown::markdownToHTML(readLines(md_file), title = "GlycoDash changelog")
      
      # Write the HTML content to the output_file.html
      writeLines(html_content, file)
    }
  )
  
  # Download manual
  output$download_manual <- downloadHandler(
    filename = "GlycoDash_manual.pdf",
    content = function(file) {
      path <- system.file("app", "www", "GlycoDash_manual.pdf", package = "GlycoDash")
      file.copy(path, file)
    }
  )
  
    
  results_data_import <- mod_data_import_server("data_import_ui_1")
  
  results_spectra_curation <- mod_spectra_curation_server(
    id = "spectra_curation_ui_1", 
    results_data_import = results_data_import)
  
  results_analyte_curation <- mod_analyte_curation_server(
    id = "analyte_curation_ui_1",
    results_spectra_curation = results_spectra_curation,
    biogroup_cols = results_data_import$biogroup_cols,
    data_type = results_data_import$data_type)
  
  results_normalization <- mod_normalization_server(
    id = "normalization_ui_1",
    results_analyte_curation = results_analyte_curation,
    merged_metadata = results_data_import$merged_metadata,
    data_type = results_data_import$data_type)
  
  results_quantitation <- mod_quantitation_server(
    id = "quantitation_ui_1",
    quantitation_clusters = results_data_import$quantitation_clusters,
    LaCyTools_summary = results_data_import$LaCyTools_summary,
    keyword_specific = results_data_import$keyword_specific,
    data_type = results_data_import$data_type,
    analyte_curated_data = results_analyte_curation$analyte_curated_data,
    results_normalization = results_normalization
  )
  
  results_derived_traits <- mod_derived_traits_server(
    id = "derived_traits_ui_1",
    results_normalization = results_normalization,
    results_quantitation = results_quantitation)
  
  results_site_occupancy <- mod_site_occupancy_server(
    id = "site_occupancy_ui_1"
  )
  
  results_repeatability <- mod_repeatability_server(
    id = "repeatability_ui_1",
    results_normalization = results_normalization,
    results_data_import = results_data_import
  )
  
  results_data_exploration <- mod_data_exploration_server(
    id = "data_exploration_ui_1",
    results_derived_traits = results_derived_traits,
    results_quantitation = results_quantitation,
    results_normalization = results_normalization)
  
  
  mod_export_server(
    id = "export_ui_1",
    results_derived_traits = results_derived_traits,
    results_quantitation = results_quantitation,
    results_data_import = results_data_import,
    results_spectra_curation = results_spectra_curation,
    results_analyte_curation = results_analyte_curation,
    results_normalization = results_normalization,
    results_repeatability = results_repeatability,
    results_data_exploration = results_data_exploration
    )
  
}
