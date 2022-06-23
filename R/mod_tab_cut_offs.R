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
    DT::dataTableOutput(ns("table")),
    # br(),
    # uiOutput(ns("text_based_on_samples")),
    # br(),
    # uiOutput(ns("text_manual"))
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
      plot <- create_cut_off_plot2(summarized_checks())
      
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
    
    output$table <- DT::renderDT({
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
                          "Percentage of passing analytes cut-off" = cut_off_prop,
                          "Group" = group)
          
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
                            "Cut-off percentage of passing analytes" = cut_off_prop,
                            "Group" = group)
            
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
      
      DT::datatable(cut_off_table,
                    rownames = FALSE,
                    options = list(searching = FALSE,
                                   paging = FALSE))
      
    })
    

    # text_cut_offs_based_on_samples <- reactive({
    #   req(cut_offs_based_on_samples())
    #   
    #   if ("group" %in% colnames(cut_offs_based_on_samples())) {
    #     purrr::map(unique(cut_offs_based_on_samples()$group),
    #                function(this_group) {
    #                  
    #                  chosen_sample_types <- cut_offs_based_on_samples() %>% 
    #                    dplyr::filter(group == this_group) %>% 
    #                    dplyr::select(sample_type_list) %>% 
    #                    tidyr::unnest() %>% 
    #                    dplyr::pull(sample_type)
    #                  
    #                  values <- cut_offs_based_on_samples() %>% 
    #                    dplyr::filter(group == this_group) %>% 
    #                    dplyr::select(cut_off_prop, cut_off_sum_int)
    #                  
    #                  line1 <- paste("Based on the", 
    #                                  comma_and(paste(this_group,
    #                                                  chosen_sample_types)),
    #                                  "samples the cut-off values are:")
    #                  line2 <- paste("Sum intensity:",
    #                                  values$cut_off_sum_int)
    #                  line3 <- paste("Percentage of passing analytes:",
    #                                  values$cut_off_prop)
    #                  
    #                  paste(line1, line2, line3, sep = '<br/>')
    #                }) %>% 
    #       paste(., collapse = "<br/><br/>")
    #   } else {
    #     chosen_sample_types <- cut_offs_based_on_samples() %>% 
    #       dplyr::select(sample_type_list) %>% 
    #       tidyr::unnest() %>% 
    #       dplyr::pull(sample_type)
    #     
    #     paste("Based on the", 
    #           comma_and(chosen_sample_types),
    #           "samples the cut-off values are:\n",
    #           "Sum intensity:",
    #           cut_offs_based_on_samples()$cut_off_sum_int,
    #           "\nPercentage of passing analytes:",
    #           cut_offs_based_on_samples()$cut_off_prop)
    #   }
    # })
    # 
    # output$text_based_on_samples <- renderUI({
    #   req(text_cut_offs_based_on_samples())
    #   HTML(text_cut_offs_based_on_samples())
    # })
    # 
    # text_manual_cut_offs <- reactive({
    #   req(manual_cut_offs())
    #   
    #   intro_sentence <- "Manual cut-offs will be used instead:"
    #   
    #   if (Ig_data() == "Yes") {
    #     
    #     text <- purrr::map(
    #       unique(manual_cut_offs()$group),
    #       function(this_group) {
    #         
    #         values <- manual_cut_offs() %>% 
    #           dplyr::filter(group == this_group) %>% 
    #           dplyr::select(cut_off_prop, cut_off_sum_int)
    #         
    #         line1 <- paste("Sum intensity cut-off for the",
    #                        this_group,
    #                        "samples:",
    #                        values$cut_off_sum_int)
    #         line2 <- paste("Percentage of passing analytes cut-off for the",
    #                        this_group,
    #                        "samples:",
    #                        values$cut_off_prop)
    #         
    #         paste(line1, line2, sep = "<br/>")
    #         
    #       }) %>% 
    #       paste(., collapse = "<br/><br/>") 
    #   } else {
    #     line1 <- paste("Sum intensity:",
    #                    manual_cut_offs()$cut_off_sum_int)
    #     line2 <- paste("Percentage of passing analytes:",
    #                    manual_cut_offs()$cut_off_prop)
    #     text <- paste(line1, line2, sep = "<br/>")
    #   }
    #   
    #   paste(intro_sentence,
    #         text,
    #         sep = "<br/>")
    # })
    # 
    # output$text_manual <- renderUI({
    #   req(text_manual_cut_offs())
    #   HTML(text_manual_cut_offs())
    # })
    # 
    # observe({
    #   shinyjs::toggle(id = "text_manual",
    #                   condition = isTRUE(switch_to_manual()))
    # })
    
  })
}
    
## To be copied in the UI
# mod_tab_cut_offs_ui("tab_cut_offs_ui_1")
    
## To be copied in the server
# mod_tab_cut_offs_server("tab_cut_offs_ui_1")
