#' derived_traits UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_derived_traits_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        h1("Derived traits")
      ),
      fluidRow(
        shinydashboard::box(
          title = "Calculate derived traits",
          width = 3,
          solidHeader = TRUE,
          status = "primary",
          checkboxGroupInput(ns("traits_menu"),
                             "Choose which derived traits should be calculated",
                             choices = c("Fucosylation",
                                         "Bisection",
                                         "Galactosylation",
                                         "Sialylation")),
          actionButton(ns("do_calculation"),
                       "Calculate derived traits")
        )
      )
    )
  )
}
    
#' derived_traits Server Functions
#'
#' @noRd 
mod_derived_traits_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observe({
      shinyjs::toggleState("do_calculation", 
                           condition = !is.null(input$traits_menu))
    })
 
  })
}
    
## To be copied in the UI
# mod_derived_traits_ui("derived_traits_ui_1")
    
## To be copied in the server
# mod_derived_traits_server("derived_traits_ui_1")
