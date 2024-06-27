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
                                                   summarized_checks,
                                                   uncalibrated_as_NA){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Creating a reactiveValue with the sample type options for the selectInput
    # menu to choose samples to exclude:
    r <- reactiveValues()
    
    observe({
      req(summarized_checks)
      r$sample_types <- unique(summarized_checks()$sample_type)
    })
    # I'm using a reactiveValue instead of a reactive expression, because a
    # reactiveValue is not invalidated if its value stays the same. Using a
    # reactiveValue here prevents the observer below (where the menu is updated)
    # from re-executing when it's not needed.
    
    observe({
      req(r$sample_types)
      
      updateSelectInput(
        inputId = "exclude_sample_types",
        choices = unique(r$sample_types)
      )
    })
    
    cut_offs <- reactive({
      req(summarized_checks(),
          input$percentile)
      
      calculate_cut_offs(
        summarized_checks = summarized_checks(),
        percentile = input$percentile,
        exclude_sample_types = input$exclude_sample_types,
        uncalibrated_as_NA = uncalibrated_as_NA()
      ) %>% 
        dplyr::mutate(percentile = input$percentile)
    })
    
    return(cut_offs)
  })
}
