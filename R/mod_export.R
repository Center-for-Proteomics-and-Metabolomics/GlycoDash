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
                           "Download processed data")
          )
        )
      )
    )
 
  )
}
    
#' export Server Functions
#'
#' @noRd 
mod_export_server <- function(id, results_derived_traits){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    x <- reactiveValues()
    
    # If data_incl_metadata exists it is assigned to x$data, otherwise
    # data_incl_plate_design is assigned to x$data. x$data (not the reactives
    # from the previous module) will be used from this point in the module.
    observe({
      if (isTruthy(results_derived_traits$data_with_derived_traits())){
        x$data <- results_derived_traits$data_with_derived_traits()
      } 
      # else { if (isTruthy(results_data_import$data_incl_plate_design())){
      #   x$data <- results_data_import$data_incl_plate_design()
      # } 
      # }
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
    
  })
}
    
## To be copied in the UI
# mod_export_ui("export_ui_1")
    
## To be copied in the server
# mod_export_server("export_ui_1")
