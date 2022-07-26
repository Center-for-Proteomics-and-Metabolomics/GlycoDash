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
    br(),
    DT::dataTableOutput(ns("table"))
  )
}
    
#' tab_cut_offs Server Functions
#'
#' @noRd 
mod_tab_cut_offs_server <- function(id, selected_cluster, summarized_checks,
                                    switch_to_manual, Ig_data, 
                                    cut_offs_to_use,
                                    cut_offs_based_on_samples,
                                    manual_cut_offs){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    summarized_checks_with_cut_offs <- reactive({
      req(summarized_checks(),
          cut_offs_to_use())
      
      dplyr::full_join(summarized_checks(),
                       cut_offs_to_use())
      
    })
    
    my_plot <- reactive({
      req(summarized_checks())
      plot <- create_cut_off_plot(summarized_checks())
      
      if (is_truthy(summarized_checks_with_cut_offs())) {
        plot <- plot +
          ggplot2::geom_vline(data = summarized_checks_with_cut_offs(),
                              ggplot2::aes(xintercept = cut_off_prop,
                                           text = paste0("Passing proportion cut-off: ",
                                                         cut_off_prop)),
                              linetype = "dotted") +
          ggplot2::geom_hline(data = summarized_checks_with_cut_offs(),
                              ggplot2::aes(yintercept = cut_off_sum_int,
                                           text = paste0("Sum intensity cut-off: ",
                                                         cut_off_sum_int)),
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
    
    cut_off_table <- reactive({
      req(any(is_truthy(cut_offs_based_on_samples()),
              is_truthy(manual_cut_offs())))
      
      cut_off_table <- tryCatch(
        expr = {
          
          dplyr::full_join(cut_offs_based_on_samples(),
                           manual_cut_offs()) %>% 
            dplyr::mutate(`Based on samples` = purrr::map_chr(
              sample_type_list,
              ~ paste(ifelse(rlang::is_empty(.x),"No", "Yes,"),
                      comma_and(.x$sample_type)))) %>% 
            dplyr::ungroup() %>% 
            dplyr::select(-c(cluster,
                             sample_type_list,
                             type)) %>% 
            dplyr::rename("Sum intensity cut-off" = cut_off_sum_int,
                          "Percentage of passing analytes cut-off" = cut_off_prop)
          
        },
        error = function(e) {
          if (is_truthy(cut_offs_based_on_samples())) {
            
            cut_offs_based_on_samples() %>% 
              dplyr::mutate(`Based on sample types` = purrr::map_chr(
                sample_type_list,
                ~ paste("Yes,",
                        comma_and(.x$sample_type)))) %>% 
              dplyr::ungroup() %>% 
              dplyr::select(-c(cluster,
                               sample_type_list,
                               type)) %>% 
              dplyr::rename("Cut-off sum intensity" = cut_off_sum_int,
                            "Cut-off percentage of passing analytes" = cut_off_prop)
            
          } else {
            
            manual_cut_offs() %>% 
              dplyr::mutate(`Based on sample types` = "No") %>% 
              dplyr::ungroup()%>% 
              dplyr::rename("Cut-off sum intensity" = cut_off_sum_int,
                            "Cut-off percentage of passing analytes" = cut_off_prop) %>% 
              dplyr::select(-c(cluster,
                               type))
            
          }
          
        }
      )
      
      return(cut_off_table)
    })
    
    output$table <- DT::renderDT({
      req(cut_off_table())
      
      DT::datatable(cut_off_table(),
                    rownames = FALSE,
                    options = list(searching = FALSE,
                                   paging = FALSE))
      
    })
    
    return(list(
      plot = my_plot,
      table = cut_off_table
    ))
    
  })
}
    
## To be copied in the UI
# mod_tab_cut_offs_ui("tab_cut_offs_ui_1")
    
## To be copied in the server
# mod_tab_cut_offs_server("tab_cut_offs_ui_1")
