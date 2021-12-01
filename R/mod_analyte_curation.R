#' analyte_curation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_analyte_curation_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        h1("Analyte curation")
      ),
      fluidRow(
        column(
          width = 6,
          shinydashboard::box(
            title = "Method for analyte curation",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            selectInput(ns("method"), 
                        "Choose method for analyte curation:",
                        choices = c("Supply an analyte list", 
                                    "Curate analytes based on data")),
            fileInput(ns("analyte_list"), "Upload file with analyte list"),
            div(
              id = ns("curation_based_on_data"),
              selectInput(ns("ignore_samples"),
                          "Sample types to ignore regarding analyte curation:",
                          choices = c("Total", "Blanks", "Negative controls"),
                          multiple = TRUE),
              numericInput(ns("cut_off"), "Cut-off (%)", value = 25)
              ),
            actionButton(ns("curate_analytes"), 
                         "Perform analyte curation")
          ),
          shinydashboard::box(
            title = "Export results",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            radioButtons(ns("output_format"),
                         "Choose a file format:",
                         choices = c("Excel file", "R object")),
            downloadButton(ns("download"), 
                           "Download analyte-curated data")
          )
        ),
        column(
          width = 6,
          shinydashboard::box(
            title = "Information on analyte curation",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            tableOutput(ns("info_table"))
          )
        )
      )
    )
  )
}
    
#' analyte_curation Server Functions
#'
#' @noRd 
mod_analyte_curation_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observe({
      shinyjs::toggle("analyte_list", 
                      condition = input$method == "Supply an analyte list")
      shinyjs::toggle("curation_based_on_data", 
                      condition = input$method == "Curate analytes based on data")
      shinyjs::toggleState("curate_analytes", 
                           condition = 
                             (input$method == "Supply an analyte list" & 
                             !is.null(input$analyte_list)) | 
                             (input$method == "Curate analytes based on data") &
                             !is.null(input$ignore_samples))
    })
    
    output$info_table <- renderTable({shinipsum::random_table(3, 3)})
 
  })
}
    
## To be copied in the UI
# mod_analyte_curation_ui("analyte_curation_ui_1")
    
## To be copied in the server
# mod_analyte_curation_server("analyte_curation_ui_1")
