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
  
  tagList(
    fluidPage(
      fluidRow(h1("IgG1 quantitation")),
      fluidRow(
        column(
          width = 6,
          shinydashboard::box(
            title = "IgG1 quantitation using SILuMAb",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            # Text for when there is no SILuMAb
            div(
              id = ns("no_silumab"),
              strong("Your samples do not contain SILuMAb for IgG1 quantitation.\n\n"),
              style = "color:#0021B8; font-size: 16px"
            ),
            # Input for SILuMAb concentration.
            numericInput(
              ns("silumab_concentration"),
              "SILuMAb concentration per sample (ng/mL):",
              value = 50, min = 0, max = NA
            ),
            # Button to calculate IgG1 concentrations
            actionButton(
              ns("calculate_concentrations"),
              "Calculate IgG1 concentrations"
            )
          )
        )
      )
    )
  )
  
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
    
    # Toggle UI elements
    observe({
      shinyjs::toggle(
        id = "no_silumab",
        condition = !is_truthy(quantitation_clusters())
      )
      shinyjs::toggle(
        id = "silumab_concentration",
        condition = is_truthy(quantitation_clusters())
      )
      shinyjs::toggle(
        id = "calculate_concentrations",
        condition = is_truthy(quantitation_clusters())
      )
      shinyjs::toggleState(
        id = "calculate_concentrations",
        condition = all(
          is_truthy(quantitation_clusters()),
          is_truthy(results_normalization$normalized_data())
        )
      )
    })
    
    
    # Calculate ratios of peptides.
    IgG1_ratios <- reactive({
      req(is_truthy(quantitation_clusters()), results_normalization$normalized_data())
      IgG1_sum_intensities <- calculate_IgG1_sum_intensities(
        LaCyTools_summary(), quantitation_clusters(), analyte_curated_data()
      )
      ratios <- calculate_IgG1_ratios(IgG1_sum_intensities, quantitation_clusters())
      return(ratios)
    })
    
    # Calculate IgG1 concentrations
    IgG1_concentrations <- reactive({
      req(IgG1_ratios(), input$silumab_concentration)
      IgG1_ratios() %>% 
        dplyr::mutate(IgG1_concentration = median_ratio * input$silumab_concentration)
    }) %>% 
      bindEvent(input$calculate_concentrations)
    
    observe({
      req(IgG1_ratios())
      browser()
      
    })

  })
}