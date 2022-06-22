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
          title = "Normalization",
          width = 3,
          solidHeader = TRUE,
          status = "primary",
          selectInput(ns("method"),
                      "Choose method for normalization",
                      choices = c("Total area normalization")),
          actionButton(ns("do_normalization"),
                       "Perform normalization")
        )   
      ),
      fluidRow(
        shinydashboard::box(
          title = "View normalized data",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          DT::dataTableOutput(ns("data_table"))
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
    
    x <- reactiveValues()
    
    observe({
      req(results_analyte_curation$analyte_curated_data())
      x$data <- results_analyte_curation$analyte_curated_data()
    })
    
    observe({
      req(x$data)
      print("x$data looks like this:")
      print(x$data)
      
      print("colnames(x$data) looks like this:")
      print(colnames(x$data))
    })
    
    observe({
      shinyjs::toggleState("do_normalization", 
                           condition = !is.null(input$method))
    })
    
    observeEvent(input$do_normalization, {
      if (input$method == "Total area normalization") {
        
        total_intensities <- calculate_total_intensity(data = x$data)
        x$normalized_data <- normalize_data(data = total_intensities)
      }
      
    })
    
    
    
    output$data_table <- DT::renderDT({
      req(x$normalized_data)
      
      x$normalized_data_wide <- x$normalized_data %>% 
        # removing columns with values that differ between clusters:
        dplyr::select(-tidyselect::any_of(c("passing_proportion", 
                                            "cut_off_prop", 
                                            "cut_off_sum_int"))) %>% 
        tidyr::pivot_wider(names_from = c(cluster, analyte),
                           names_sep = "_",
                           values_from = relative_abundance)
      
      DT::datatable(data = x$normalized_data_wide,
                    options = list(scrollX = TRUE))
    })
    
    return(list(
      normalized_data = reactive({x$normalized_data}),
      normalized_data_wide = reactive({x$normalized_data_wide})
    ))
 
  })
}
    
## To be copied in the UI
# mod_normalization_ui("normalization_ui_1")
    
## To be copied in the server
# mod_normalization_server("normalization_ui_1")
