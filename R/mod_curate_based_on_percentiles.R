#' curate_based_on_percentiles UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_curate_based_on_percentiles_ui <- function(id){
  ns <- NS(id)
  tagList(
    numericInput(
      ns("percentile"),
      "Choose the percentile at which to set the cut-off values:",
      value = 5,
      min = 0,
      max = 100,
      step = 1
    ),
    selectInput(
      ns("exclude_sample_types"),
      "Select sample types to exclude from the cut-off calculation:",
      choices = "",
      multiple = TRUE
    )
  )
}
    
#' curate_based_on_percentiles Server Functions
#'
#' @noRd 
mod_curate_based_on_percentiles_server <- function(id,
                                                   is_Ig_data,
                                                   summarized_checks){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observe({
      req(summarized_checks())
      updateSelectInput(
        inputId = "exclude_sample_types",
        choices = unique(summarized_checks()$sample_type)
      )
    })
    
    cut_offs <- reactive({
      req(summarized_checks(),
          input$percentile)
      
      calculate_cut_offs(
        summarized_checks = summarized_checks(),
        percentile = input$percentile,
        exclude_sample_types = input$exclude_sample_types,
        uncalibrated_as_NA = FALSE
      )
    })
    
    return(cut_offs)
  })
}
    
## To be copied in the UI
# mod_curate_based_on_percentiles_ui("curate_based_on_percentiles_ui_1")
    
## To be copied in the server
# mod_curate_based_on_percentiles_server("curate_based_on_percentiles_ui_1")
