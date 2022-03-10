#' data_import_v2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_data_import_v2_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' data_import_v2 Server Functions
#'
#' @noRd 
mod_data_import_v2_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_data_import_v2_ui("data_import_v2_ui_1")
    
## To be copied in the server
# mod_data_import_v2_server("data_import_v2_ui_1")
