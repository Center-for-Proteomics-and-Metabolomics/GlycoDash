#' repeatability UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_repeatability_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidPage(
      fluidRow(
        h1("Repeatability")
      ),
      fluidRow(
        column(
          width = 6,
          shinydashboard::box(
            title = "Assess repeatability",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            uiOutput(ns("standard_menu")),
            actionButton(ns("assess_repeatability"),
                         label = "Assess repeatability")
          )
        )
      ),
      fluidRow(
        column(
          width = 6,
          shinydashboard::box(
            title = "Repeatability plot",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            plotOutput(ns("plot"))
          )
        ),
        column(
          width = 6,
          shinydashboard::box(
            title = "Repeatability table",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            DT::dataTableOutput(ns("table"))
          )
        )
      )
    )
  )
}
    
#' repeatability Server Functions
#'
#' @noRd 
mod_repeatability_server <- function(id, results_normalization, results_data_import){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    x <- reactiveValues()
    
    observe({
      req(results_normalization$normalized_data())
      x$data <- results_normalization$normalized_data()
    })
    
    output$standard_menu <- renderUI({
      req(x$data,
          results_data_import$Ig_data())
      
      sample_type_menu <- selectInput(ns("standard_sample_type"),
                        label = "Choose which standard you want to assess:",
                        choices = unique(x$data$sample_type))
      
      if (results_data_import$Ig_data() == "Yes") {
        groups_menu <- selectInput(ns("standard_group"),
                                   label = "Choose if you want to look at total or specific Ig samples:",
                                   choices = unique(x$data$group))
      }
      
      return(list(sample_type_menu, groups_menu))
    })
    
    observeEvent(input$assess_repeatability, {
      x$repeatability <- calculate_repeatability_stats(data = x$data,
                                                       standard_sample_type = input$standard_sample_type,
                                                       standard_group = input$standard_group)
      x$variation_table <- x$repeatability %>% 
        dplyr::summarise(intra_plate_variation = mean(RSD)) %>% 
        dplyr::summarise(across(),
                         inter_plate_variation = mean(intra_plate_variation, 
                                                      na.rm = TRUE))
    })
    
    output$plot <- renderPlot({
      req(x$repeatability)
      visualize_repeatability(x$repeatability)
    })
    
    output$table <- DT::renderDataTable({
      req(x$variation_table)
      for_table <- x$variation_table %>% 
        dplyr::select(-inter_plate_variation)
      
      DT::datatable(for_table,
                    rownames = FALSE,
                    colnames = c("Plate" = "plate",
                                 "Intra-plate variation" = "intra_plate_variation"))
    })
 
  })
}
