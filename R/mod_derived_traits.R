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
    h1("Welcome to derived traits!")
 
  )
}
    
#' derived_traits Server Functions
#'
#' @noRd 
mod_derived_traits_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_derived_traits_ui("derived_traits_ui_1")
    
## To be copied in the server
# mod_derived_traits_server("derived_traits_ui_1")
