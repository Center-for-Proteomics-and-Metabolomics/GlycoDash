#' site_occupancy UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_site_occupancy_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' site_occupancy Server Functions
#'
#' @noRd 
mod_site_occupancy_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_site_occupancy_ui("site_occupancy_1")
    
## To be copied in the server
# mod_site_occupancy_server("site_occupancy_1")
