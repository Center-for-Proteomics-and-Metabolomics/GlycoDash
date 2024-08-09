#' tab_repeatability_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tab_repeatability_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjqui::jqui_resizable(plotly::plotlyOutput(ns("plot")))
  )
}
    
#' tab_repeatability_plot Server Functions
#'
#' @noRd 
mod_tab_repeatability_plot_server <- function(id,
                                              by_plate,
                                              repeatability,
                                              my_data,
                                              selected_sample_id,
                                              selected_group){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    repeatability_plot <- reactive({
      if (is_truthy(by_plate())) {
        req(repeatability())
        req(selected_sample_id())
        
        plot <- tryCatch(
          expr = {
            visualize_repeatability(repeatability(),
                                     selected_group = selected_group(),
                                     selected_sample_id = selected_sample_id())
          },
          all_NAs = function(c) {
            showNotification(c$message,
                             type = "error",
                             duration = NULL)
            NULL
          })
      } else {
        req(my_data())
        req(selected_sample_id())
        
        plot <- tryCatch(
          expr = {
            visualize_repeatability_mean_bars(
              my_data(),
              selected_sample_id = selected_sample_id(),
              selected_group = selected_group()
            ) 
          },
          all_NAs = function(c) {
            showNotification(c$message,
                             type = "error",
                             duration = NULL)
            NULL
          })
      }
      
      return(plot)
    })
    
    output$plot <- plotly::renderPlotly({
      req(repeatability_plot())
      
      plotly_object <- plotly::ggplotly(repeatability_plot(), tooltip = "text")
      
      plotly_object <- change_axis_title_distance(plotly_object, 
                                                  y_distance = 90)
      
      plotly_object
    })
    
    return(repeatability_plot)
    
  })
}
    
## To be copied in the UI
# mod_tab_repeatability_plot_ui("tab_repeatability_plot_1")
    
## To be copied in the server
# mod_tab_repeatability_plot_server("tab_repeatability_plot_1")
