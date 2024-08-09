#' curated_spectra_plots UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tab_curated_spectra_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    column(
      width = 12,
      br(),
      shinyjqui::jqui_resizable(plotly::plotlyOutput(ns("plot")))
    )
  )
}
    
#' curated_spectra_plots Server Functions
#'
#' @noRd 
mod_tab_curated_spectra_plot_server <- function(id,
                                                curated_data,
                                                contains_total_and_specific_samples){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    curated_spectra_plot <- reactive({
      req(curated_data())
      plot_spectra_curation_results(
        curated_data = curated_data(),
        total_and_specific = contains_total_and_specific_samples()
      )
    })
    
    output$plot <- plotly::renderPlotly({
      req(curated_spectra_plot())
      
      plotly_object <- plotly::ggplotly(curated_spectra_plot(), tooltip = "text")
      plotly_object <- facet_strip_bigger(plotly_object)
      plotly_object <- change_axis_title_distance(plotly_object)
      
      return(plotly_object)
    })
    
    return(curated_spectra_plot)
  })
}
    
## To be copied in the UI
# mod_tab_curated_spectra_plot_ui("curated_spectra_plots_1")
    
## To be copied in the server
# mod_tab_curated_spectra_plot_server("curated_spectra_plots_1")
