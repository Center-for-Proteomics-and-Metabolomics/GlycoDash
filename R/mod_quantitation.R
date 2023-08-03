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
                                    LaCyTools_summary,
                                    analyte_curated_data,
                                    results_normalization) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Write a function: quantify_IgG1() --> place in fct_quantitation.R
    IgG1_quantitation_data <- reactive({
      req(is_truthy(quantitation_clusters()), results_normalization$normalized_data())
      get_IgG1_quantitation_data(LaCyTools_summary(), 
                                 quantitation_clusters(),
                                 analyte_curated_data())
    })
    
    observe({
      req(IgG1_quantitation_data())
      browser()
    })
  
    
    ##### PLACEHOLDER CODE #####
    observeEvent( input$validate , {
      print(input$choice)
    })
    ############################
  
  })
}