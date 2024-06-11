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
    shinycssloaders::withSpinner(plotly::plotlyOutput(ns("plot"), width = "1350px")),
    br(),
    tags$style(HTML(paste0(
      "#",
      ns("table"),
      ".datatables {width: 50%; margin: auto}"
    ))),
    DT::dataTableOutput(ns("table")),
    br(),
    shinyWidgets::materialSwitch(ns("switch_to_manual"),
                                 HTML("<i style='font-size:15px;'> Choose cut-off values manually instead </i>"),
                                 right = TRUE,
                                 status = "primary"),
    shinyjs::hidden(numericInput(ns("cut_off_sum_intensity"),
                 "Enter a cut-off value for the sum intensity:",
                 value = 0, min = 0)),
    shinyjs::hidden(numericInput(ns("cut_off_passing_analyte_percentage"),
                 "Enter a cut-off value for the percentage of passing analytes:",
                 value = 0 , min = 0)),
    # div() has to be placed around the boxes below for toggle to work properly.
    shinyjs::hidden(div(id = ns("spike_manual_cut_offs"),
      shinydashboardPlus::box(
        title = "Specific Ig manual cut-offs",
        solidHeader = TRUE,
        #status = "primary",
        width = 6,
        numericInput(ns("cut_off_sum_intensity_specific"),
                     "Enter a cut-off value for the sum intensity in the specific Ig samples:",
                     value = 0, min = 0),
        numericInput(ns("cut_off_passing_analyte_percentage_specific"),
                     "Enter a cut-off value for the percentage of passing analytes in the specific Ig samples:",
                     value = 0, min = 0)
      ))),
    shinyjs::hidden(div(id = ns("total_manual_cut_offs"),
      shinydashboardPlus::box(
        title = "Total Ig manual cut-offs",
        solidHeader = TRUE,
        #status = "primary",
        width = 6,
        numericInput(ns("cut_off_sum_intensity_total"),
                     "Enter a cut-off value for the sum intensity in the total Ig samples:",
                     value = 0, min = 0),
        numericInput(ns("cut_off_passing_analyte_percentage_total"),
                     "Enter a cut-off value for the percentage of passing analytes in the total Ig samples:",
                     value = 0, min = 0)
      )))
  )
}
    


#' tab_cut_offs Server Functions
#'
#' @noRd 
mod_tab_cut_offs_server <- function(id, selected_cluster, summarized_checks,
                                    contains_total_and_specific_samples, 
                                    calculated_cut_offs,
                                    keyword_specific, keyword_total,
                                    curation_method) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Show the cut-off numericInputs when manual_cut_off is chosen:
    observe({

      shinyjs::toggle("cut_off_sum_intensity",
                      condition = all(is_truthy(input$switch_to_manual),
                                      contains_total_and_specific_samples() == FALSE))
      shinyjs::toggle("cut_off_passing_analyte_percentage",
                      condition = all(is_truthy(input$switch_to_manual),
                                      contains_total_and_specific_samples() == FALSE))

      shinyjs::toggle("spike_manual_cut_offs",
                      condition = all(is_truthy(input$switch_to_manual),
                                      contains_total_and_specific_samples() == TRUE))

      shinyjs::toggle("total_manual_cut_offs",
                      condition = all(is_truthy(input$switch_to_manual),
                                      contains_total_and_specific_samples() == TRUE))
      
      shinyjs::toggle("switch_to_manual",
                      condition = curation_method() != "Skip spectra curation")
    })
    
    
    # Code below is supposed to set initial manual cut-off values to
    # be equal to calculated cut-off values?
    # For now I set their initial values to 0 by default
    
    # observe({
    #   req(calculated_cut_offs())
    #   req(is_truthy(input$switch_to_manual))
    #   req(!is_truthy(manual_cut_offs()))
    # 
    #   if (contains_total_and_specific_samples() == TRUE) {
    #     calculated_sum_intensity_cut_off_specific <- calculated_cut_offs() %>% 
    #       dplyr::filter(group == keyword_specific()) %>% 
    #       dplyr::pull(cut_off_sum_intensity)
    #     
    #     updateNumericInput(session = session,
    #                        inputId = "cut_off_sum_intensity_specific",
    #                        value = calculated_sum_intensity_cut_off_specific)
    #     
    #     calculated_sum_intensity_cut_off_total <- calculated_cut_offs() %>% 
    #       dplyr::filter(group == keyword_total()) %>% 
    #       dplyr::pull(cut_off_sum_intensity)
    #     
    #     updateNumericInput(session = session,
    #                        inputId = "cut_off_sum_intensity_total",
    #                        value = calculated_sum_intensity_cut_off_total)
    #     
    #     calculated_passing_analyte_percentage_cut_off_specific <- calculated_cut_offs() %>% 
    #       dplyr::filter(group == keyword_specific()) %>% 
    #       dplyr::pull(cut_off_passing_analyte_percentage)
    #     
    #     updateNumericInput(session = session,
    #                        inputId = "cut_off_passing_analyte_percentage_specific",
    #                        value = calculated_passing_analyte_percentage_cut_off_specific)
    #     
    #     calculated_passing_analyte_percentage_cut_off_total <- calculated_cut_offs() %>% 
    #       dplyr::filter(group == keyword_total()) %>% 
    #       dplyr::pull(cut_off_passing_analyte_percentage)
    #     
    #     updateNumericInput(session = session,
    #                        inputId = "cut_off_passing_analyte_percentage_total",
    #                        value = calculated_passing_analyte_percentage_cut_off_total)
    #       
    #   } else if (contains_total_and_specific_samples() == FALSE) {
    #     
    #     calculated_sum_intensity_cut_off <- calculated_cut_offs() %>% 
    #       dplyr::pull(cut_off_sum_intensity)
    #     
    #     updateNumericInput(session = session,
    #                        inputId = "cut_off_sum_intensity",
    #                        value = calculated_sum_intensity_cut_off)
    #     
    #     calculated_passing_analyte_percentage_cut_off <- calculated_cut_offs() %>% 
    #       dplyr::pull(cut_off_passing_analyte_percentage)
    #     
    #     updateNumericInput(session = session,
    #                        inputId = "cut_off_passing_analyte_percentage",
    #                        value = calculated_passing_analyte_percentage_cut_off)
    #   }
    #   
    # })
    
    
    manual_cut_offs <- reactive({
      
      if (contains_total_and_specific_samples() == TRUE) {
        req(input$cut_off_sum_intensity_specific,
            input$cut_off_sum_intensity_total,
            input$cut_off_passing_analyte_percentage_specific,
            input$cut_off_passing_analyte_percentage_total)
        
        specific <- tibble::tibble(
          cut_off_sum_intensity = input$cut_off_sum_intensity_specific,
          cut_off_passing_analyte_percentage = input$cut_off_passing_analyte_percentage_specific,
          group = keyword_specific(),
          curation_method = "manual"
        )
        
        total <- tibble::tibble(
          cut_off_sum_intensity = input$cut_off_sum_intensity_total,
          cut_off_passing_analyte_percentage = input$cut_off_passing_analyte_percentage_total,
          group = keyword_total(),
          curation_method = "manual"
        )
        
        cut_offs <- dplyr::full_join(specific, total) %>% 
          dplyr::mutate(group = as.factor(group))
        
      } else {
        req(input$cut_off_sum_intensity,
            input$cut_off_passing_analyte_percentage)
        
        cut_offs <- data.frame(cut_off_sum_intensity = input$cut_off_sum_intensity,
                               cut_off_passing_analyte_percentage = input$cut_off_passing_analyte_percentage,
                               curation_method = "manual",
                               cluster = selected_cluster)
        
      }
      
      return(cut_offs)
    })
    
    
    # Use either calculated cut-offs, or manual cut-offs based on switch
    cut_offs_to_use <- reactive({
      if (is_truthy(input$switch_to_manual)) {
        req(manual_cut_offs())
      } else {
        req(calculated_cut_offs())
      }
    })
    
    # Combine cut-offs with summarized checks
    summarized_checks_with_cut_offs <- reactive({
      req(summarized_checks(), cut_offs_to_use())
      dplyr::full_join(summarized_checks(), cut_offs_to_use())
    })
    
    # Scatter plot
    my_plot <- reactive({
      req(summarized_checks())
      plot <- create_cut_off_plot(summarized_checks())
      
      if (is_truthy(summarized_checks_with_cut_offs())) {
        plot <- plot +
          ggplot2::geom_vline(data = summarized_checks_with_cut_offs(),
                              ggplot2::aes(xintercept = cut_off_passing_analyte_percentage),
                              linetype = "dotted") +
          ggplot2::geom_hline(data = summarized_checks_with_cut_offs(),
                              ggplot2::aes(yintercept = cut_off_sum_intensity),
                              linetype = "dotted")
        
      }
      return(plot)
    })

    # Render the plot
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
    
    
    # This combined manual cut-offs and calculated cut-offs for some reason,
    # doesn't seem very useful?
    # for_cut_off_table_both <- reactive({
    #   req(calculated_cut_offs(),
    #       manual_cut_offs())
    #   
    #   dplyr::full_join(calculated_cut_offs(),
    #                    manual_cut_offs()) %>% 
    #     dplyr::mutate(
    #       `Based on samples` = purrr::map_chr(
    #         sample_type_list,
    #         ~ paste(ifelse(rlang::is_empty(.x),"No", "Yes,"),
    #                 comma_and(.x$sample_type))
    #       ),
    #       curation_method = firstupper(
    #         stringr::str_replace_all(curation_method,
    #                                  pattern = "_",
    #                                  replacement = " ")
    #       )
    #     ) %>% 
    #     dplyr::ungroup() %>% 
    #     dplyr::select(-c(cluster,
    #                      sample_type_list)) %>% 
    #     dplyr::rename("Sum intensity cut-off" = cut_off_sum_intensity,
    #                   "Percentage of passing analytes cut-off" = cut_off_passing_analyte_percentage,
    #                   "Curation method" = curation_method) %>% 
    #     dplyr::rename_with(firstupper)
    # })
    
    
    # Nicely formatted data for table with calculated cut-offs
    for_cut_off_table_calculated <- reactive({
      req(calculated_cut_offs())
      
      calculated_cut_offs() %>% 
        dplyr::mutate(
          `Based on sample types` = purrr::map_chr(
            sample_type_list,
            ~ paste("Yes,",
                    comma_and(.x$sample_type))),
          curation_method = firstupper(
            stringr::str_replace_all(curation_method,
                                     pattern = "_",
                                     replacement = " "))) %>% 
        dplyr::ungroup() %>% 
        dplyr::select(-c(cluster,
                         sample_type_list)) %>% 
        dplyr::rename("Cut-off sum intensity" = cut_off_sum_intensity,
                      "Cut-off percentage of passing analytes" = cut_off_passing_analyte_percentage,
                      "Curation method" = curation_method) %>% 
        dplyr::rename_with(firstupper)
    })
    
    
    # Nicely formatted data for table with manual cut-offs
    for_cut_off_table_manual <- reactive({
      req(manual_cut_offs())
      
      manual_cut_offs() %>% 
        dplyr::mutate(`Based on sample types` = "No",
                      curation_method = "Manual cut-offs") %>% 
        dplyr::ungroup()%>% 
        dplyr::rename("Cut-off sum intensity" = cut_off_sum_intensity,
                      "Cut-off percentage of passing analytes" = cut_off_passing_analyte_percentage,
                      "Curation method" = curation_method) %>% 
        dplyr::rename_with(firstupper)
    })
    
    
    # show_in_cut_off_table <- reactive({
    #   if (is_truthy(input$switch_to_manual)) {
    #     if (is_truthy(for_cut_off_table_both())) {
    #       for_cut_off_table_both()
    #     } else {
    #       req(for_cut_off_table_manual())
    #     }
    #   } else {
    #     req(for_cut_off_table_calculated())
    #   }
    # })
    
    
    # Show either calculated or manual cut-offs in table, depending on switch
    show_in_cut_off_table <- reactive(
      if (is_truthy(input$switch_to_manual)) {
        for_cut_off_table_manual()
      } else {
        req(for_cut_off_table_calculated())
      }
    )
    
    # Round the numbers in the table
    cut_off_table_rounded <- reactive({
      req(show_in_cut_off_table())
      show_in_cut_off_table() %>% 
        dplyr::mutate(
          `Cut-off sum intensity` = as.character(round(`Cut-off sum intensity`, digits = 0)),
          `Cut-off percentage of passing analytes` = paste0(
            format(round(`Cut-off percentage of passing analytes`, digits = 2), nsmall = 2),
            "%"
          )
        )
    })
    
    # Render the table
    output$table <- DT::renderDT({
      req(cut_off_table_rounded())
  
      DT::datatable(cut_off_table_rounded(),
                    rownames = FALSE,
                    width = "600px",
                    options = list(searching = FALSE,
                                   paging = FALSE,
                                   info = FALSE))
    })
    
    return(list(
      plot = my_plot,
      table = show_in_cut_off_table,
      cut_offs_to_use = cut_offs_to_use
    ))
    
  })
}
    
## To be copied in the UI
# mod_tab_cut_offs_ui("tab_cut_offs_ui_1")
    
## To be copied in the server
# mod_tab_cut_offs_server("tab_cut_offs_ui_1")
