#' data_import UI Function
#'
#' @description A shiny Module.
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
          shinydashboard::box(
            title = "Upload your files",
            width = NULL,
            fileInput(ns("lacytools_summary"), "Upload LacyTools summary.txt file:"),
            fileInput(ns("metadata"), "Upload a metadata file:"),
            fileInput(ns("plate_design"), "Upload a plate design file:")
          ),
          shinydashboard::box(
            title = "Read and convert data",
            width = NULL,
            "If both a metadata file and a plate design file have been supplied, the metadata will be added automatically.",
            br(),
            br(),
            actionButton(ns("read_summary"), "Convert the LacyTools summary file to an R-suitable format")
          )
        ),
        column(
          width = 6,
          shinydashboard::box(
            title = "View the converted data",
            width = NULL,
            DT::dataTableOutput(ns("data_table"))
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
    output$data_table <- DT::renderDT(shinipsum::random_DT(nrow = 100, ncol = 50,
                                                              options = list(scrollX = TRUE)))
  })
}
    
## To be copied in the UI
# mod_data_import_ui("data_import_ui_1")
    
## To be copied in the server
# mod_data_import_server("data_import_ui_1")
