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
    
    IgG1_sum_intensities <- reactive({
      req(is_truthy(quantitation_clusters()), results_normalization$normalized_data())
      calculate_IgG1_sum_intensities(LaCyTools_summary(),
                                    quantitation_clusters(),
                                    analyte_curated_data())
    })
    
    IgG1_concentrations <- reactive({
      req(IgG1_sum_intensities())
      calculate_IgG1_concentrations(IgG1_sum_intensities(), quantitation_clusters())
    })
    
    observe({
      req(IgG1_concentrations())
      browser()
      # Still good to check these ratios
    })
    
    
  
    
    ##### PLACEHOLDER CODE #####
    observeEvent( input$validate , {
      print(input$choice)
    })
    ############################
  
  })
}