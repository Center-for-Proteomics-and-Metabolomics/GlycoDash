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
                              results_repeatability,
                              results_data_exploration){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    x <- reactiveValues()
    
    # If data_with_derived_traits exists it is assigned to x$data, otherwise
    # normalized_data is assigned to x$data:
    observe({
      if (isTruthy(results_derived_traits$data_with_derived_traits())){
        x$data <- results_derived_traits$data_with_derived_traits()
      } 
      else { if (isTruthy(results_derived_traits$normalized_data())){
        x$data <- results_derived_traits$normalized_data()
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
        
        called_analyte_curation_objects <- tryCatch(
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
                                                
        rep1_plot_table <- tryCatch(
          expr = {
            purrr::map(results_repeatability$first_tab(),
                       ~ do.call(.x,
                                 args = list()))
          },
          error = function(e) {
            NULL
          })
        
        if (!is.null(results_repeatability$second_tab())) {
          rep2_plot_table <- tryCatch(
            expr = {
              purrr::map(results_repeatability$second_tab(),
                         ~ do.call(.x,
                                   args = list()))
            },
            error = function(e) {
              NULL
            })
        } else {
          rep2_plot_table <- NULL
        }
        
        data_exploration_plot <- tryCatch(
          expr = {
          results_data_exploration$plot()
            },
          error = function(e){
            NULL
          })
        
        derived_traits <- tryCatch(
          expr = {
            results_derived_traits$derived_traits()
          },
          error = function(e){
            NULL
          })
        
        # plate_design <- purrr::map(results_data_import$plate_design,
        #                            ~ do.call(.x,
        #                                      args = list()))
        
        #plate_design[sapply(plate_design, is.null)] <- NULL
      
        params <- list(lacytools_summary = results_data_import$filename_summary(),
                       plate_design = results_data_import$filenames_plate_design(),
                       metadata = results_data_import$metadata(),
                       manual_sample_types = results_data_import$manual_sample_types(),
                       sample_types_file = results_data_import$sample_types_file(),
                       mass_acc = results_spectra_curation$mass_acc(),
                       ipq = results_spectra_curation$ipq(),
                       sn = results_spectra_curation$sn(),
                       spectra_curation_cut_off = results_spectra_curation$cut_off(),
                       spectra_curation_plot = results_spectra_curation$plot(),
                       analyte_curation_method = results_analyte_curation$method(),
                       ignore_samples = results_analyte_curation$ignore_samples(),
                       cut_off_percentage = results_analyte_curation$cut_off(),
                       analyte_list = results_analyte_curation$analyte_list(),
                       analyte_curation_objects = called_analyte_curation_objects,
                       derived_traits = derived_traits,
                       repeatability_1 = rep1_plot_table,
                       repeatability_2 = rep2_plot_table,
                       data_exploration_plot = data_exploration_plot
                       )
        
        temp_report <- file.path(tempdir(), paste0(session$token, 
                                                   "Report.Rmd"))
        report_file <- system.file("app",
                                   "www", 
                                   "Report.Rmd",
                                   package = "glycodash")
        
        file.copy(report_file, 
                  temp_report, 
                  overwrite = TRUE)
        
        rmarkdown::render(input = temp_report,
                          output_file = file,
                          envir = new.env(parent = globalenv()),
                          params = params
        )
      }
    )
    
  })
}
    
## To be copied in the UI
# mod_export_ui("export_ui_1")
    
## To be copied in the server
# mod_export_server("export_ui_1")
