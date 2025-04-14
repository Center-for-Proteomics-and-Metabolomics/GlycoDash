#' tab_quantitation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tab_quantitation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      width = 12,
      br(),
      shinyWidgets::materialSwitch(
        ns("log_scale"),
        HTML("<i style='font-size:15px;'> Plot quantities on logarithmic scale </i>"),
        status = "success",
        right = TRUE,
        value = FALSE
      ),
      shinyjqui::jqui_resizable(plotly::plotlyOutput(ns("plot")))
    )
  )
}
    
#' tab_quantitation Server Functions
#'
#' @noRd 
mod_tab_quantitation_server <- function(id, 
                                        quantities) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    quantities_plot <- reactive({
      req(quantities)
      plot_protein_quantities(quantities, input$log_scale)
    })
    
    output$plot <- plotly::renderPlotly({
      req(quantities_plot())
      plotly::ggplotly(quantities_plot(), tooltip = "text") %>% 
        GlycoDash::hide_outliers(.)
    })
    
    return(list(
      quantities = quantities,
      quantities_plot = quantities_plot
    ))
  })
}
    
