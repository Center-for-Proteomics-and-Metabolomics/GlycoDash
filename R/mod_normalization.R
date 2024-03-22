#' normalization UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_normalization_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        h1("Normalized data")
      ),
      fluidRow(
        shinydashboard::box(
          title = "View normalized data",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          DT::dataTableOutput(ns("data_table"))
        )
      ),
      fluidRow(
        shinydashboard::box(
          title = "Export results",
          width = 6,
          solidHeader = TRUE,
          status = "primary",
          radioButtons(ns("download_format"),
                       "Choose a file format:",
                       choices = c("Excel file", "R object")),
          downloadButton(ns("download"), "Download normalized data")
        )
      )
    )
  )
}
    
#' normalization Server Functions
#'
#' @noRd 
mod_normalization_server <- function(id, results_analyte_curation, merged_metadata, data_type) {
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    analyte_curated_data <- reactive({
      # req() considers an empty dataframe Truthy, and because of the way that
      # results_analyte_curation$analyte_curated_data() is created in
      # mod_analyte_curation.R there is a moment that it is an empty dataframe.
      # Solution -> wait until results_analyte_curation$analyte_curated_data()
      # is not empty:
      req(!rlang::is_empty(results_analyte_curation$analyte_curated_data()))
      results_analyte_curation$analyte_curated_data()
    })

    
    total_intensities <- reactive({
      req(analyte_curated_data())
      calculate_total_intensity(data = analyte_curated_data(), data_type = data_type())
    })
    
    
    normalized_data <- reactive({
      req(total_intensities())
      normalize_data(total_intensities = total_intensities())
    })
  
    
    normalized_data_wide <- reactive({
      req(normalized_data())
      
      normalized_data() %>% 
        tidyr::pivot_wider(names_from = cluster, values_from = sum_intensity, 
                           names_glue = "{cluster}_sum_intensity") %>% 
        tidyr::pivot_wider(names_from = analyte, values_from = relative_abundance) %>% 
        dplyr::group_by(sample_name) %>% 
        tidyr::fill(replicates:last_col(), .direction = "downup") %>% 
        dplyr::ungroup() %>% 
        dplyr::distinct() %>% 
        
        { # Combine with metadata if it exists
          if (is_truthy(merged_metadata())) {
            dplyr::left_join(., merged_metadata(), by = "sample_id") %>% 
            dplyr::relocate(colnames(merged_metadata())[-1], .after = sample_id)
          } else .
        }
    })
    
    output$data_table <- DT::renderDT({
      req(normalized_data_wide())
      
      DT::datatable(data = normalized_data_wide(),
                    options = list(scrollX = TRUE))
    })
    
    
    # Download normalized data
    observe({
      shinyjs::toggleState("download", is_truthy(normalized_data_wide()))
    })

    output$download <- downloadHandler(
      filename = function() {
        current_datetime <- paste0(format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M"))
        switch(input$download_format,
               "R object" = paste0(current_datetime, "_normalized_data.rds"),
               "Excel file" = paste0(current_datetime, "_normalized_data.xlsx"))
      },
      content = function(file) {
        data_to_download <- normalized_data_wide()
        switch(input$download_format,
               "R object" = save(data_to_download,
                                 file = file),
               "Excel file" = writexl::write_xlsx(data_to_download,
                                                  path = file))
      }
    )
    
    return(list(
      normalized_data = normalized_data,
      normalized_data_wide = normalized_data_wide
    ))
 
  })
}
