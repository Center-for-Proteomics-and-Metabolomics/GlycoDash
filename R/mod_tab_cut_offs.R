#' tab_cut_offs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tab_cut_offs_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    plotly::plotlyOutput(ns("plot")),
    "Based on the selected samples the cut-off values are:",
    textOutput(ns("sum_int_cut_off")),
    textOutput(ns("prop_cut_off"))
  )
}
    
#' tab_cut_offs Server Functions
#'
#' @noRd 
mod_tab_cut_offs_server <- function(id, cluster, checked_spectra, chosen_cut_offs, cut_off_basis){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    spectra_check_filtered <- reactive({
      req(checked_spectra())
      
      checked_spectra() %>% 
        dplyr::filter(cluster == cluster)
    })
    
    cut_offs_filtered <- reactive({
      req(chosen_cut_offs())
      
      print(chosen_cut_offs())
      chosen_cut_offs() %>% 
        dplyr::filter(cluster == cluster)
    })
    
    my_plot <- reactive({
      req(spectra_check_filtered())
      plot <- create_cut_off_plot2(spectra_check_filtered())
      
      if (is_truthy(cut_offs_filtered())) {
        plot <- plot +
          ggplot2::geom_vline(ggplot2::aes(xintercept = cut_offs_filtered()$cut_off_prop,
                                           text = paste0("Passing proportion cut-off: ",
                                                         cut_offs_filtered()$cut_off_prop)),
                              linetype = "dotted") +
          ggplot2::geom_hline(ggplot2::aes(yintercept = cut_offs_filtered()$cut_off_sum_int,
                                           text = paste0("Sum intensity cut-off: ",
                                                         cut_offs_filtered()$cut_off_prop)),
                              linetype = "dotted")
      }
      return(plot)
    })
    
    output$plot <- plotly::renderPlotly({
      req(my_plot())
      
      plot <- my_plot() +
        ggplot2::theme(axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 20)))
      
      plotly <- plotly::ggplotly(plot, tooltip = "text")
      
      plotly[["x"]][["layout"]][["margin"]][["l"]] <- plotly[["layout"]][["margin"]][["l"]] + 20
      
      plotly <- facet_strip_bigger(plotly)
      
      
      
      return(plotly)
    })
    
    output$sum_int_cut_off <- renderText({
      paste("Sum intensity:",
            ifelse(is_truthy(cut_offs_filtered()), cut_offs_filtered()$cut_off_sum_int, ""))
    })
    
    output$prop_cut_off <- renderText({
      paste("Percentage of passing analytes:",
            ifelse(is_truthy(cut_offs_filtered()), cut_offs_filtered()$cut_off_prop, ""))
    })
    
  })
}
    
## To be copied in the UI
# mod_tab_cut_offs_ui("tab_cut_offs_ui_1")
    
## To be copied in the server
# mod_tab_cut_offs_server("tab_cut_offs_ui_1")
