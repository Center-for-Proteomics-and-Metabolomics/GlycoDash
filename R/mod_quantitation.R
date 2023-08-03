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
  
  
  
  
  
  ##### PLACEHOLDER CODE #####
  tagList(
    sliderInput(
      inputId = ns("choice"), 
      label = "Choice",
      min = 1, max = 10, value = 5
    ),
    actionButton(
      inputId = ns("validate"),
      label = "Validate Choice"
    )
  )
  ###########################
  
}




#' quantitation Server Functions
#'
#' @noRd 
mod_quantitation_server <- function(id, quantitation_clusters,
                                    results_analyte_curation) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
	
	  # Code for server below...
  
  
    
    ##### PLACEHOLDER CODE #####
    observeEvent( input$validate , {
      print(input$choice)
    })
    ############################
  
  })
}