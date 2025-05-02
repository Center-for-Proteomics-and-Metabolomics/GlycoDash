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
            radioButtons(ns("download_format"),
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
      ),
      fluidRow(
        shinydashboard::box(
          title = "View the processed data",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          DT::dataTableOutput(ns("data_table"))
        )
      )
    )
 
  )
}
    
#' export Server Functions
#'
#' @noRd 
mod_export_server <- function(id, 
                              results_site_occupancy,
                              results_derived_traits,
                              results_data_import,
                              results_spectra_curation,
                              results_analyte_curation,
                              results_normalization,
                              results_repeatability,
                              results_data_exploration) {
  moduleServer( id, function(input, output, session) {
    ns <- session$ns
    
    x <- reactiveValues()
    
    observe({
      req(results_normalization$normalized_data_wide())
      if (is_truthy(results_site_occupancy$site_occupancy_data())) {
        x$data <- results_site_occupancy$site_occupancy_data()
      } else if (is_truthy(results_derived_traits$data_with_traits())) {
        x$data <- results_derived_traits$data_with_traits()
      } else if (is_truthy(results_quantitation$data_with_quantities())) {
        x$data <- results_quantitation$data_with_quantities()
      } else {
        x$data <- results_normalization$normalized_data_wide()
      }
    })
    
    # Disable the "Download processed data" button until normalized data is available
    # Also the Generate report button
    observe({
      shinyjs::toggleState("download", is_truthy(x$data))
      shinyjs::toggleState("report", is_truthy(x$data))
    })
    
    # Display the final data table
    output$data_table <- DT::renderDT({
      req(x$data)
      DT::datatable(data = x$data %>% 
                      dplyr::mutate_if(is.numeric, ~format(round(., 2), nsmall = 2)),
                    options = list(
                      scrollX = TRUE,
                      pageLength = 6,
                      columnDefs = list(list(className = "dt-center", targets = "_all"))
                    ), filter = "top")
    })
    
    observe({
      if (is_truthy(results_normalization$notes())) {
        updateRadioButtons(
          inputId = "download_format",
          choices = c("Excel file (data and notes)", "R object (data only)")
        )
      }
    })
    
    
    # Download processed data
    output$download <- downloadHandler(
      filename = function() {
        current_datetime <- paste0(format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M"))
        if (grepl("R object", input$download_format)) {
          paste0(current_datetime, "_normalized_data.rds")
        } else {
          paste0(current_datetime, "_normalized_data.xlsx")
        }
      },
      content = function(file) {
        if (grepl("R object", input$download_format)) {
          save(x$data, file = file)
        } else if (is_truthy(results_normalization$notes())) {
          data_list <- list("Data" = x$data, "Notes" = results_normalization$notes())
          writexl::write_xlsx(data_list, path = file)
        } else {
          writexl::write_xlsx(x$data, path = file)
        }
      }
    )
    
    
    output$report <- downloadHandler(
      filename = function() {
        current_datetime <- paste0(format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M"))  # Thx ChatGPT
        paste0(current_datetime, "_data_processing_report.html")
      },
      content = function(file) {
        
        shinybusy::show_modal_spinner(
          spin = "cube-grid", color = "#0275D8",
          text = HTML("<br/><strong>Generating HTML report...")
        )
        
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
        
        curated_spectra_plots <- tryCatch(
          expr = {
            purrr::map(
              results_spectra_curation$curated_spectra_plots(),
                       function(curated_spectra_plot) {
                         do.call(curated_spectra_plot,
                                 args = list())
                       })
          },
          error = function(e) {
            NULL
          }
        )
        
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
        
        
        # Repeatability
        # Mapping (or looping) a reactiveValues list is not possible. You need
        # to convert it to a regular list first. 
        repeatability_list <- shiny::reactiveValuesToList(results_repeatability$tab_results)
        # Remove potential NULL values from the list (happens when tabs are deleted)
        repeatability_list_clean <- repeatability_list[!sapply(repeatability_list, is.null)]
        # Loop over the tabs in the list
        repeatability_tab_contents <- purrr::map(
          repeatability_list_clean,
          function(list_of_objects) {
            # Remove NULL tabs from the list (happens when tabs are deleted)
            
            plot <- try_call(list_of_objects$plot)
            plots <- purrr::map(list_of_objects$plots(),
                                ~ try_call(.x))
            table <- try_call(list_of_objects$table)
            title <- do.call(list_of_objects$title_for_report,
                             args = list())
            return(list(
              plot = plot,
              plots = plots,
              table = table,
              title = title
            ))
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
          data_type = results_data_import$data_type(),
          summary_filenames = results_data_import$summary_filenames(),
          plate_design = try_call(results_data_import$filenames_plate_design), 
          sample_list = try_call(results_data_import$filename_sample_list), 
          filenames_metadata = try_call(results_data_import$filenames_metadata), 
          sample_types_method = results_data_import$sample_types_method(),
          filename_sample_types = try_call(results_data_import$filename_sample_types),
          mass_acc = results_spectra_curation$mass_acc(),
          ipq = results_spectra_curation$ipq(),
          sn = results_spectra_curation$sn(),
          idp = results_spectra_curation$idp(),
          total_area = results_spectra_curation$total_area(),
          included_qc_spectra = results_spectra_curation$included_qc(),
          spectra_curation_tab_contents = spectra_curation_tab_contents,
          curated_spectra_plots = curated_spectra_plots,
          analyte_curation_method = results_analyte_curation$method(),
          included_qc_analytes = results_analyte_curation$included_qc(),
          analyte_curation_choice = results_analyte_curation$curation_method(),
          groups_to_ignore = results_analyte_curation$groups_to_ignore(),
          ignore_samples = results_analyte_curation$ignore_samples(),
          cut_offs = results_analyte_curation$cut_offs(),
          analyte_list = results_analyte_curation$analyte_list(),
          analyte_curation_tab_contents = analyte_curation_tab_contents,
          heatmaps = results_normalization$heatmaps(),
          heatmaps_excluded_sample_types = results_normalization$heatmaps_excluded_sample_types(),
          derived_traits = try_call(results_derived_traits$derived_traits),
          formulas = try_call(results_derived_traits$formulas),
          custom_formulas = try_call(results_derived_traits$custom_formulas),
          intensity_plots = results_derived_traits$intensity_plots(),
          site_occupancy_quality_plot = try_call(results_site_occupancy$quality_plot()),
          site_occupancy_boxplot = try_call(results_site_occupancy$occupancy_plot()),
          site_occupancy_mass_error = try_call(results_site_occupancy$mass_accuracy()),
          site_occupancy_excluded_peptides = try_call(results_site_occupancy$exclude_peptides()),
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
                                   package = "GlycoDash")
        
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
        
        shinybusy::remove_modal_spinner()
      }
    )
    
  })
}
    
## To be copied in the UI
# mod_export_ui("export_ui_1")
    
## To be copied in the server
# mod_export_server("export_ui_1")
