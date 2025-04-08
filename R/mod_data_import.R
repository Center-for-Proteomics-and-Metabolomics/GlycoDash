#' data_import UI Function
#'
#' @description This module contains the first tab 'Data Import' of the GlycoDash app.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_data_import_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        h1("Data Import")
      ),
      fluidRow(
        column(
          width = 6,
          mod_read_lacytools_ui(ns("read_lacytools_ui_1")),
          mod_add_sample_ids_ui(ns("add_sample_ids_ui_1")),
          mod_add_sample_types_ui(ns("add_sample_types_ui_1")),
          mod_clusters_ui(ns("clusters_ui_1")),
          mod_add_metadata_ui(ns("add_metadata_ui_1"))
        ),
        column(
          width = 6,
          shinydashboard::box(
            title = "View the converted data",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            shinycssloaders::withSpinner(DT::DTOutput(ns("data_table")))
          ),
          shinydashboard::box(
            title = "Export results",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            radioButtons(ns("download_format"),
                         "Choose a file format:",
                         choices = c("Excel file", "R object")),
            downloadButton(ns("download"), 
                           "Download data")
          )
        )
      )
    )
  )
}
    
#' data_import Server Functions
#'
#' @noRd 
mod_data_import_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    LaCyTools_summary <- mod_read_lacytools_server("read_lacytools_ui_1")
    
    data_incl_sample_ids <- mod_add_sample_ids_server("add_sample_ids_ui_1",
                                                      keyword_specific = LaCyTools_summary$keyword_specific,
                                                      keyword_total = LaCyTools_summary$keyword_total,
                                                      contains_total_and_specific_samples = LaCyTools_summary$contains_total_and_specific_samples,
                                                      LaCyTools_summary = LaCyTools_summary$data,
                                                      summary_filenames = LaCyTools_summary$summary_filenames)
    
    data_incl_sample_types <- mod_add_sample_types_server("add_sample_types_ui_1",
                                                          LaCyTools_summary = data_incl_sample_ids$data)
    
    data_incl_clusters <- mod_clusters_server("clusters_ui_1",
                                              LaCyTools_summary = data_incl_sample_types$data)
    
    data_incl_metadata <- mod_add_metadata_server("add_metadata_ui_1",
                                                  LaCyTools_summary = data_incl_clusters$data)
    
    
    
    # Update the data shown in the datatable according to the steps that have
    # been completed by the user
    show_in_table <- reactive({
      req(LaCyTools_summary$data())

      if (is_truthy(data_incl_metadata$data())) {
        show_in_table <- data_incl_metadata$data()
      } else if (is_truthy(data_incl_clusters$data())) {
        show_in_table <- data_incl_clusters$data()
      } else if (is_truthy(data_incl_sample_types$data())) {
          show_in_table <- data_incl_sample_types$data()
      } else if (is_truthy(data_incl_sample_ids$data())) {
        show_in_table <- data_incl_sample_ids$data()
      } else if (is_truthy(LaCyTools_summary$data())) {
          show_in_table <- LaCyTools_summary$data()
      }
      return(show_in_table)
    })
    
    
    
    # When the LaCyTools summary has been read in, the converted data is shown
    # in the data table
    output$data_table <- DT::renderDT({
      req(show_in_table())
      DT::datatable(show_in_table() %>% # Round numbers to 2 decimals
                      dplyr::mutate_if(is.numeric, ~format(round(., 2), nsmall = 2)),
                    options = list(
                      scrollX = TRUE,
                      pageLength = 6,  # Shows 5 rows
                      columnDefs = list(list(className = "dt-center", targets = "_all"))
                    ),
                    filter = "top")
    })
    
    
    # The data to pass along to the next module:
    to_return <- reactive({
      if (is_truthy(data_incl_metadata$data())) {
        data_incl_metadata$data()
      } else {
        if (is_truthy(data_incl_clusters$data())) {
          data_incl_clusters$data()
        } else { 
          NULL
        }
      }
    })
    
    

    # Create a vector with column names, from which a column can later be
    # chosen as the column that contains the biological groups.
    biogroup_cols <- reactive({
      req(data_incl_clusters$data())
      if (is_truthy(data_incl_metadata$data())) {
        c("sample_id", "sample_type", colnames(data_incl_metadata$merged_metadata()))
      } else if (is_truthy(data_incl_clusters$data())) {
          c("sample_id", "sample_type")
      } else NULL
    })
    
    
    # The download button is disabled until data has been loaded:
    observe({
      shinyjs::toggleState("download",
                           condition = is_truthy(show_in_table()))
    })
    
    output$download <- downloadHandler(
      filename = function() {
        current_datetime <- paste0(format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M"))
        switch(input$download_format,
               "R object" = paste0(current_datetime, "_data.rds"),
               "Excel file" = paste0(current_datetime, "_data.xlsx"))
      },
      content = function(file) {
        data_to_download <- show_in_table()
        
        switch(input$download_format,
               "R object" = save(data_to_download, 
                                 file = file),
               "Excel file" = writexl::write_xlsx(data_to_download, 
                                                  path = file))
      }
    )
    
    
    return(list(
      LaCyTools_summary = to_return,
      data_type = LaCyTools_summary$data_type,
      biogroup_cols = biogroup_cols,
      contains_total_and_specific_samples = LaCyTools_summary$contains_total_and_specific_samples,
      keyword_specific = LaCyTools_summary$keyword_specific,
      keyword_total = LaCyTools_summary$keyword_total,
      summary_filenames = LaCyTools_summary$summary_filenames,
      filenames_plate_design = data_incl_sample_ids$filenames_plate_design,
      filename_sample_list = data_incl_sample_ids$filename_sample_list,
      filenames_metadata = data_incl_metadata$filenames_metadata,
      sample_types_method = data_incl_sample_types$method,
      filename_sample_types = data_incl_sample_types$filename_sample_types,
      colnames_metadata = data_incl_metadata$colnames_metadata,
      merged_metadata = data_incl_metadata$merged_metadata,
      peptides = data_incl_clusters$peptides
    ))
    
  })
}
