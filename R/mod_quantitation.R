#' quantitation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_quantitation_ui <- function(id) {
  ns <- NS(id)
  
  # Code for UI below...
  
}




#' quantitation Server Functions
#'
#' @noRd 
mod_quantitation_server <- function(id, results_analyte_curation, results_normalization) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
	
	# Code for server below...
  
  
  
  })
}