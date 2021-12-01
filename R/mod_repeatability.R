#' repeatability UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_repeatability_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("Welcome to repeatability!")
 
  )
}
    
#' repeatability Server Functions
#'
#' @noRd 
mod_repeatability_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_repeatability_ui("repeatability_ui_1")
    
## To be copied in the server
# mod_repeatability_server("repeatability_ui_1")
