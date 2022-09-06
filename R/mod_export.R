#' export UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_export_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        h1("Export results")
      ),
      fluidRow(
        column(
          width = 6,
          shinydashboard::box(
            title = "Download the processed data",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            shinyWidgets::awesomeRadio(ns("download_format"),
                                       "Choose a file format:",
                                       choices = c("Excel file", "R object")),
            downloadButton(ns("download"), 
                           "Download processed data"),
            br(),
            br(),
            downloadButton(ns("report"),
                           "Generate report")
          )
        )
      )
    )
 
  )
}
    
#' export Server Functions
#'
#' @noRd 
mod_export_server <- function(id, 
                              results_derived_traits,
                              results_data_import,
                              results_spectra_curation,
                              results_analyte_curation,
                              results_normalization,
                              results_repeatability,
                              results_data_exploration){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    x <- reactiveValues()
    
    # If data_with_derived_traits exists it is assigned to x$data, otherwise
    # normalized_data is assigned to x$data:
    observe({
      if (is_truthy(results_derived_traits$data_with_derived_traits())){
        x$data <- results_derived_traits$data_with_derived_traits()
      } 
      else { if (is_truthy(results_normalization$normalized_data_wide())){
        x$data <- results_normalization$normalized_data_wide()
      }
      }
    })
    
    output$download <- downloadHandler(
      filename = function() {
        todays_date <- paste0(stringr::str_replace_all(Sys.Date(),
                                                       pattern = "-",
                                                       replacement = ""))
        switch(input$download_format,
               "R object" = paste0(todays_date, "_processed_data.rds"),
               "Excel file" = paste0(todays_date, "_processed_data.xlsx"))
      },
      content = function(file) {
        data_to_download <- x$data
        switch(input$download_format,
               "R object" = save(data_to_download, 
                                 file = file),
               "Excel file" = writexl::write_xlsx(data_to_download, 
                                                  path = file))
      }
    )
    
    output$report <- downloadHandler(
      filename = function() {
        todays_date <- paste0(stringr::str_replace_all(Sys.Date(),
                                                       pattern = "-",
                                                       replacement = ""))
        paste0(todays_date, "_data_processing_report.html")
      },
      content = function(file) {
        
        # The contents of the spectra curation tabs need to be retrieved:
        # results_spectra_curation$tab_contents() is a reactive expression
        # containing a list. In that list there is one list for each tab in
        # which the objects are stored as reactive expressions. 
        spectra_curation_tab_contents <- tryCatch(
          expr = {
            # We map the list that contains one list for each tab:
            purrr::map(results_spectra_curation$tab_contents(),
                       function(list_of_objects) {
                         # Then we map each list containing objects:
                         purrr::map(
                           list_of_objects,
                           # Every reactive expression containing an object in
                           # the list is called to retrieve the objects:
                           ~ do.call(.x,
                                     args = list()))
                       })
          },
          error = function(e) {
            NULL
          }
        ) # We end up with the same list as at the start, but now all the 
        # reactive expressions within it have been called, so the list just
        # contains normal objects (plots and tables).
        
        # We do the same thing as above for the analyte curation tabs:
        analyte_curation_tab_contents <- tryCatch(
          expr = {
            purrr::map(results_analyte_curation$objects(),
                       function(list_of_objects) {
                         purrr::map(
                           list_of_objects,
                           ~ do.call(.x,
                                     args = list()))
                       })
          },
          error = function(e) {
            NULL
          }
        )
        
        repeatability_tab_contents <- purrr::map(
          # Mapping (or looping) a reactiveValues list is not possible. You need
          # to convert it to a regular list first:
          shiny::reactiveValuesToList(results_repeatability$tab_results),
          function(list_of_objects) {
            lapply(list_of_objects,
                   function(x) {
                     # When samples are not grouped by plate there is no table
                     # and trying to call it results in an error, so I'm using
                     # tryCatch:
                     tryCatch(
                       expr = {
                         do.call(x,
                                 args = list())
                       },
                       error = function(e) {
                         NULL
                       })
                   })
          })
        
        data_exploration_tab_contents <- purrr::map(
          # Mapping (or looping) a reactiveValues list is not possible. You need
          # to convert it to a regular list first:
          shiny::reactiveValuesToList(results_data_exploration$tab_results),
          function(list_of_objects) {
            lapply(list_of_objects,
                   function(x) {
                     tryCatch(
                       expr = {
                         do.call(x,
                                 args = list())
                       },
                       error = function(e) {
                         NULL
                       })
                   })
          })
        
        # We prepare a list of parameters with all of the plots, tables and
        # other information from the dashboard to pass along to the Report.Rmd
        # markdown file:
        params <- list(
          lacytools_summary = results_data_import$filename_summary(),
          plate_design = try_call(results_data_import$filenames_plate_design), # trycall not needed?
          sample_list = try_call(results_data_import$filename_sample_list), # trycall not needed?
          metadata = try_call(results_data_import$metadata), # trycall not needed?
          sample_types_method = results_data_import$sample_types_method(),
          filename_sample_types = try_call(results_data_import$filename_sample_types),
          mass_acc = results_spectra_curation$mass_acc(),
          ipq = results_spectra_curation$ipq(),
          sn = results_spectra_curation$sn(),
          spectra_curation_tab_contents = spectra_curation_tab_contents,
          curated_spectra_plot = results_spectra_curation$plot(),
          analyte_curation_method = results_analyte_curation$method(),
          ignore_samples = results_analyte_curation$ignore_samples(), # test if empty
          cut_off_percentage = results_analyte_curation$cut_off(),
          analyte_list = results_analyte_curation$analyte_list(),
          analyte_curation_tab_contents = analyte_curation_tab_contents,
          derived_traits = try_call(results_derived_traits$derived_traits),
          formulas = try_call(results_derived_traits$formulas),
          repeatability = repeatability_tab_contents,
          data_exploration = data_exploration_tab_contents
        )
        
        # Create a temporary file with a unique name per session to prevent
        # overwriting the file when there are simultaneous users:
        temp_report <- file.path(tempdir(), paste0(session$token, 
                                                   "Report.Rmd"))
        report_file <- system.file("app",
                                   "www", 
                                   "Report2.Rmd",
                                   package = "glycodash")
        
        # Copy the original Report.Rmd file to the temporary file location:
        file.copy(report_file, 
                  temp_report, 
                  overwrite = TRUE)
        
        # Render the parameterized report:
        rmarkdown::render(
          input = temp_report,
          output_format = "html_document",
          output_file = file,
          envir = new.env(parent = globalenv()),
          params = params # We're passing along the params that are listed above
        )
      }
    )
    
  })
}
    
## To be copied in the UI
# mod_export_ui("export_ui_1")
    
## To be copied in the server
# mod_export_server("export_ui_1")
