#' tab_repeatability UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tab_repeatability_ui <- function(id){
  ns <- NS(id)
  tagList(
    column(
      width = 6,
      shinydashboard::box(
        title = "Select a standard",
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        uiOutput(ns("standards_menu")),
        actionButton(ns("assess_repeatability"),
                     label = "Assess repeatability")
      ),
      plotOutput(ns("plot"))
    ),
    column(
      width = 6,
      DT::dataTableOutput(ns("table"))
    )
  )
}
    
#' tab_repeatability Server Functions
#'
#' @noRd 
mod_tab_repeatability_server <- function(id, data, Ig_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    x <- reactiveValues()
    
    output$standards_menu <- renderUI({
      req(data())
      req(Ig_data())
      
      sample_type_menu <- selectInput(ns("sample_type"),
                                      label = "Choose which standard you want to assess:",
                                      choices = unique(data()$sample_type))
      
      if (Ig_data() == "Yes") {
        groups_menu <- selectInput(ns("standard_group"),
                                   label = "Choose if you want to look at total or specific Ig samples:",
                                   choices = unique(data()$group))
      }
      
      menu <- list(sample_type_menu, 
                   groups_menu)
      menu[sapply(menu, is.null)] <- NULL
      
      return(menu)
    })
    
    observeEvent(input$assess_repeatability, {
      x$repeatability <- calculate_repeatability_stats(data = data(),
                                                       standard_sample_type = input$standard_sample_type,
                                                       standard_group = input$standard_group)
      x$variation_table <- x$repeatability %>% 
        dplyr::summarise(intra_plate_variation = mean(RSD, na.rm = TRUE))
    })
    
    output$plot <- renderPlot({
      req(x$repeatability)
      visualize_repeatability(x$repeatability)
    })
    
    output$table <- DT::renderDataTable({
      req(x$variation_table)
      
      for_table <- x$variation_table %>% 
        dplyr::mutate(intra_plate_variation = signif(intra_plate_variation,
                                                     digits = 3))
      
      sketch <- htmltools::withTags(table(
        DT::tableHeader(c("Plate", "Intra-plate variation (%)")),
        DT::tableFooter(c("Inter-plate variation (%)", 
                          signif(mean(for_table$intra_plate_variation, 
                                      na.rm = TRUE),
                                 digits = 3)))
      ))
      
      DT::datatable(data = for_table,
                    container = sketch,
                    rownames = FALSE)
    })
    
  })
}
    
## To be copied in the UI
# mod_tab_repeatability_ui("tab_repeatability_ui_1")
    
## To be copied in the server
# mod_tab_repeatability_server("tab_repeatability_ui_1")
