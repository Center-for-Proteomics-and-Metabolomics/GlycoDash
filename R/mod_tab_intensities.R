#' tab_intensities UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tab_intensities_ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      width= 12,
      shinyjqui::jqui_resizable(plotly::plotlyOutput(ns("plot")))
    )
  )
}
    
#' tab_intensities Server Functions
#'
#' @noRd 
mod_tab_intensities_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Example plot
    output$plot <- plotly::renderPlotly(
      ggplot2::ggplot(mtcars, ggplot2::aes(mpg, disp)) +
        ggplot2::geom_point()
    )
    
  })
}
    
## To be copied in the UI
# mod_tab_intensities_ui("tab_intensities_1")
    
## To be copied in the server
# mod_tab_intensities_server("tab_intensities_1")
