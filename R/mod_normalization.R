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
    h1("Welcome to normalization!")
 
  )
}
    
#' normalization Server Functions
#'
#' @noRd 
mod_normalization_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_normalization_ui("normalization_ui_1")
    
## To be copied in the server
# mod_normalization_server("normalization_ui_1")
