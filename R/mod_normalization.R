#' normalization UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_normalization_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        h1("Normalization")
      ),
      fluidRow(
        shinydashboard::box(
          title = "View normalized data",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          actionButton(ns("normalization_button"),
                       "Perform total area normalization"),
          br(),
          br(),
          DT::dataTableOutput(ns("data_table")),
          br(),
          br(),
          "To download the normalized data proceed to the 'Export results' tab."
        )
      )
    )
  )
}
    
#' normalization Server Functions
#'
#' @noRd 
mod_normalization_server <- function(id, results_analyte_curation){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    analyte_curated_data <- reactive({
      # req() considers an empty dataframe Truthy, and because of the way that
      # results_analyte_curation$analyte_curated_data() is created in
      # mod_analyte_curation.R there is a moment that it is an empty dataframe.
      # Solution -> wait until results_analyte_curation$analyte_curated_data()
      # is not empty:
      req(!rlang::is_empty(results_analyte_curation$analyte_curated_data()))
      results_analyte_curation$analyte_curated_data()
    })
    
    observe({
      shinyjs::toggleState(id = "normalization_button",
                           condition = is_truthy(analyte_curated_data()))
    })
    
    total_intensities <- reactive({
      req(analyte_curated_data())
      calculate_total_intensity(data = analyte_curated_data())
    }) %>% bindEvent(input$normalization_button)
    
    normalized_data <- reactive({
      req(total_intensities())
      normalize_data(total_intensities = total_intensities())
    })
    
    normalized_data_wide <- reactive({
      req(normalized_data())
      normalized_data() %>% 
        # removing columns with values that differ between clusters:
        dplyr::select(-tidyselect::any_of(c("passing_analyte_percentage", 
                                            "cut_off_passing_analyte_percentage", 
                                            "cut_off_sum_intensity"))) %>% 
        tidyr::pivot_wider(names_from = c(cluster, analyte),
                           names_sep = "_",
                           values_from = relative_abundance)
    })
    
    output$data_table <- DT::renderDT({
      req(normalized_data_wide())
      
      DT::datatable(data = normalized_data_wide(),
                    options = list(scrollX = TRUE))
    })
    
    return(list(
      normalized_data = normalized_data,
      normalized_data_wide = normalized_data_wide
    ))
 
  })
}
