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
    textOutput(ns("cut_off_basis_text")),
    textOutput(ns("sum_int_cut_off")),
    textOutput(ns("prop_cut_off")),
    shinyjs::hidden(
      div(id = ns("manual_cut_offs_text"),
          "Manual cut-off values will be used instead:",
          br(),
          textOutput(ns("manual_cut_offs_line1")),
          textOutput(ns("manual_cut_offs_line2"))
      )
    )
  )
}
    
#' tab_cut_offs Server Functions
#'
#' @noRd 
mod_tab_cut_offs_server <- function(id, selected_cluster, summarized_checks, 
                                    cut_offs_based_on_samples, cut_off_basis,
                                    manual_cut_offs, switch_to_manual){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # summarized_checks_filtered <- reactive({
    #   req(summarized_checks())
    #   summarized_checks() %>% 
    #     dplyr::filter(cluster == selected_cluster)
    # })
    
    # cut_offs_filtered <- reactive({
    #   req(cut_offs_based_on_samples())
    #   cut_offs_based_on_samples() %>% 
    #     dplyr::filter(cluster == selected_cluster)
    # })
    
    cut_offs_based_on_samples_filtered <- reactive({
      req(cut_offs_based_on_samples())
      cut_offs_based_on_samples() %>% 
        dplyr::filter(cluster == selected_cluster)
    })
    
    cut_offs_to_use <- reactive({
      if (all(is_truthy(switch_to_manual()),
              is_truthy(manual_cut_offs()))) {
        manual_cut_offs()
      } else {
        req(cut_offs_based_on_samples())
        cut_offs_based_on_samples_filtered()
      }
    })
    
    my_plot <- reactive({
      req(summarized_checks())
      plot <- create_cut_off_plot2(summarized_checks())
      
      if (is_truthy(cut_offs_to_use())) {
        plot <- plot +
          ggplot2::geom_vline(ggplot2::aes(xintercept = cut_offs_to_use()$cut_off_prop,
                                           text = paste0("Passing proportion cut-off: ",
                                                         cut_offs_to_use()$cut_off_prop)),
                              linetype = "dotted") +
          ggplot2::geom_hline(ggplot2::aes(yintercept = cut_offs_to_use()$cut_off_sum_int,
                                           text = paste0("Sum intensity cut-off: ",
                                                         cut_offs_to_use()$cut_off_sum_int)),
                              linetype = "dotted")
      }
      return(plot)
    })
    
    output$plot <- plotly::renderPlotly({
      req(my_plot())
      
      plot <- my_plot() +
        ggplot2::theme(axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 20)))
      
      plotly <- plotly::ggplotly(plot, tooltip = "text")
      
      plotly[["x"]][["layout"]][["annotations"]][[2]][["xshift"]] <- -70
      
      plotly[["x"]][["layout"]][["margin"]][["l"]] <- 90
      
      plotly <- facet_strip_bigger(plotly)
      
      
      
      return(plotly)
    })
    
    output$cut_off_basis_text <- renderText({
      req(cut_off_basis())
      
      paste("Based on the", 
            comma_and(cut_off_basis()),
            "the cut-off values are:")
    })
    
    output$sum_int_cut_off <- renderText({
      paste("Sum intensity:",
            ifelse(is_truthy(cut_offs_based_on_samples_filtered()), 
                   cut_offs_based_on_samples_filtered()$cut_off_sum_int, 
                   ""))
    })
    
    output$prop_cut_off <- renderText({
      paste("Percentage of passing analytes:",
            ifelse(is_truthy(cut_offs_based_on_samples_filtered()), 
                   cut_offs_based_on_samples_filtered()$cut_off_prop, ""))
    })
    
    output$manual_cut_offs_line1 <- renderText({
      req(manual_cut_offs())
      paste("Sum intensity:",
            manual_cut_offs()$cut_off_sum_int)
    })
    
    output$manual_cut_offs_line2 <- renderText({
      req(manual_cut_offs())
      paste("Percentage of passing analytes:",
            manual_cut_offs()$cut_off_prop)
    })
    
    observe({
      shinyjs::toggle(id = "manual_cut_offs_text",
                      condition = is_truthy(switch_to_manual()))
    })
    
  })
}
    
## To be copied in the UI
# mod_tab_cut_offs_ui("tab_cut_offs_ui_1")
    
## To be copied in the server
# mod_tab_cut_offs_server("tab_cut_offs_ui_1")
