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
        h1("Normalization")
      ),
      fluidRow(
        shinydashboard::box(
          title = "Normalization",
          width = 3,
          solidHeader = TRUE,
          status = "primary",
          selectInput(ns("method"),
                      "Choose method for normalization",
                      choices = c("Total Area normalization", 
                                  "other")),
          actionButton(ns("do_normalization"),
                       "Perform normalization")
        )   
      ),
      fluidRow(
        shinydashboard::box(
          title = "View normalized data",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          DT::dataTableOutput(ns("data_table"))
        )
      )
    )
  )
}
    
#' normalization Server Functions
#'
#' @noRd 
mod_normalization_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observe({
      shinyjs::toggleState("do_normalization", 
                           condition = !is.null(input$method))
    })
    
    output$data_table <- DT::renderDT(shinipsum::random_DT(nrow = 100, 
                                                           ncol = 50,
                                                           options = list(scrollX = TRUE)))
 
  })
}
    
## To be copied in the UI
# mod_normalization_ui("normalization_ui_1")
    
## To be copied in the server
# mod_normalization_server("normalization_ui_1")
