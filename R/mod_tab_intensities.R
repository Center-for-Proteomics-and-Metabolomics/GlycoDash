#' mod_tab_intensities UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_mod_tab_intensities_ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      width= 12,
      shinyjqui::jqui_resizable(plotly::plotlyOutput(ns("plot")))
    )
  )
}
    
#' mod_tab_intensities Server Functions
#'
#' @noRd 
mod_mod_tab_intensities_server <- function(id, params) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_mod_tab_intensities_ui("mod_tab_intensities_1")
    
## To be copied in the server
# mod_mod_tab_intensities_server("mod_tab_intensities_1")
