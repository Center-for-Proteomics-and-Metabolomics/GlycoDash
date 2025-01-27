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
      width = 12,
      plotly::plotlyOutput(ns("plot"))
    ),
    # shinyjqui_resizable() does not work here somehow
    # So need this JS code to make resizing work properly
    tags$script(HTML(paste0("
      $(document).ready(function() {
        $('#", ns("plot"), "').resizable({
          handles: 'e, s, se',
          minHeight: 300,
          minWidth: 300
        });
      });
    ")))
  )
}


    
#' tab_intensities Server Functions
#'
#' @noRd 
mod_tab_intensities_server <- function(id, data, traits) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    cluster <- id
    
    data_to_plot <- reactive({
      req(data)
      data_to_plot <- data %>%
        dplyr::select(
          sample_name, sample_type, sample_id, tidyselect::starts_with(paste0(cluster, "_")),
          tidyselect::any_of("group") # specific Ig data
        ) %>% 
        tidyr::pivot_longer(
          cols = tidyselect::matches(paste0(cluster, "_")) & 
                 !tidyselect::matches(paste0(cluster, "_sum_intensity")),
          names_to = "trait", values_to = "relative_abundance"
        ) %>% 
        dplyr::mutate(trait = factor(trait, levels = unique(trait)))
    })
    
    
    intensity_plot <- reactive({
      traits_vs_intensity_plot(data_to_plot(), cluster)
    })
    
    
    output$plot <- plotly::renderPlotly({
      req(intensity_plot())
      plotly::ggplotly(intensity_plot(),  tooltip = "text")
    })
    

    return(list(
      intensity_plot = intensity_plot
    ))
    
  })
}
    
## To be copied in the UI
# mod_tab_intensities_ui("tab_intensities_1")
    
## To be copied in the server
# mod_tab_intensities_server("tab_intensities_1")
