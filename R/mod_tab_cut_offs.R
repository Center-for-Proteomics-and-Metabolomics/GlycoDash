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
    tags$style(HTML(paste0(
      "#",
      ns("table"),
      ".datatables {width: 50%; margin: auto}"
    ))),
    DT::dataTableOutput(ns("table")),
    br(),
    shinyWidgets::materialSwitch(ns("switch_to_manual"),
                                 "Choose cut-off values manually instead",
                                 right = TRUE,
                                 status = "primary"),
    numericInput(ns("cut_off_sum_intensityensity"),
                 "Enter a cut-off value for the sum intensity:",
                 value = ""),
    numericInput(ns("cut_off_passing_analyte_percentage"),
                 "Enter a cut-off value for the percentage of passing analytes:",
                 value = ""),
    shinydashboardPlus::box(
      title = "Specific Ig manual cut-offs",
      id = ns("spike_manual_cut_offs"),
      solidHeader = TRUE,
      #status = "primary",
      width = 6,
      numericInput(ns("cut_off_sum_intensityensity_specific"),
                   "Enter a cut-off value for the sum intensity in the specific Ig samples:",
                   value = ""),
      numericInput(ns("cut_off_passing_analyte_percentage_specific"),
                   "Enter a cut-off value for the percentage of passing analytes in the specific Ig samples:",
                   value = "")
    ),
    shinydashboardPlus::box(
      title = "Total Ig manual cut-offs",
      id = ns("total_manual_cut_offs"),
      solidHeader = TRUE,
      #status = "primary",
      width = 6,
      numericInput(ns("cut_off_sum_intensityensity_total"),
                   "Enter a cut-off value for the sum intensity in the total Ig samples:",
                   value = ""),
      numericInput(ns("cut_off_passing_analyte_percentage_total"),
                   "Enter a cut-off value for the percentage of passing analytes in the total Ig samples:",
                   value = "")
    )
  )
}
    
#' tab_cut_offs Server Functions
#'
#' @noRd 
mod_tab_cut_offs_server <- function(id, selected_cluster, summarized_checks,
                                    #switch_to_manual, cut_offs_to_use, manual_cut_offs,
                                    Ig_data, calculated_cut_offs,
                                    keyword_specific, keyword_total){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Hide the cut_off_basis selectInput when manual_cut_off is chosen:
    observe({
      shinyjs::toggle("cut_off_sum_intensityensity",
                      condition = all(is_truthy(input$switch_to_manual),
                                      Ig_data() == "No"))
      shinyjs::toggle("cut_off_passing_analyte_percentage",
                      condition = all(is_truthy(input$switch_to_manual),
                                      Ig_data() == "No"))
      
      shinyjs::toggle("spike_manual_cut_offs",
                      condition = all(is_truthy(input$switch_to_manual),
                                      Ig_data() == "Yes"))
      
      shinyjs::toggle("total_manual_cut_offs",
                      condition = all(is_truthy(input$switch_to_manual),
                                      Ig_data() == "Yes"))
    })
    
    manual_cut_offs <- reactive({
      
      if (Ig_data() == "Yes") {
        req(input$cut_off_sum_intensityensity_specific,
            input$cut_off_sum_intensityensity_total,
            input$cut_off_passing_analyte_percentage_specific,
            input$cut_off_passing_analyte_percentage_total)
        
        specific <- tibble::tibble(
          cut_off_sum_intensity = input$cut_off_sum_intensityensity_specific,
          cut_off_passing_analyte_percentage = input$cut_off_passing_analyte_percentage_specific,
          group = keyword_specific(),
          curation_method = "manual"
        )
        
        total <- tibble::tibble(
          cut_off_sum_intensity = input$cut_off_sum_intensityensity_total,
          cut_off_passing_analyte_percentage = input$cut_off_passing_analyte_percentage_total,
          group = keyword_total(),
          curation_method = "manual"
        )
        
        cut_offs <- dplyr::full_join(specific, total)
        
        # Multiply rows of combined to get one row for each cluster
        # cut_offs <- purrr::map_dfr(clusters(),
        #                            function(this_cluster) {
        #                              combined %>% 
        #                                dplyr::mutate(cluster = this_cluster)
        #                            })
        
      } else {
        req(input$cut_off_sum_intensityensity,
            input$cut_off_passing_analyte_percentage)
        
        cut_offs <- data.frame(cut_off_sum_intensity = input$cut_off_sum_intensityensity,
                               cut_off_passing_analyte_percentage = input$cut_off_passing_analyte_percentage,
                               curation_method = "manual",
                               cluster = selected_cluster)
        
        # cut_offs <- purrr::map_dfr(clusters(),
        #                            function(this_cluster) {
        #                              cut_offs_wo_cluster %>% 
        #                                dplyr::mutate(cluster = this_cluster)
        #                            })
        
      }
      
      return(cut_offs)
    })
    
    cut_offs_to_use <- reactive({
      if (is_truthy(input$switch_to_manual)) {
        req(manual_cut_offs())
      } else {
        req(calculated_cut_offs())
      }
    })
    
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
                              ggplot2::aes(xintercept = cut_off_passing_analyte_percentage,
                                           text = paste0("Passing analyte percentage cut-off: ",
                                                         cut_off_passing_analyte_percentage)),
                              linetype = "dotted") +
          ggplot2::geom_hline(data = summarized_checks_with_cut_offs(),
                              ggplot2::aes(yintercept = cut_off_sum_intensity,
                                           text = paste0("Sum intensity cut-off: ",
                                                         cut_off_sum_intensity)),
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
      req(any(is_truthy(calculated_cut_offs()),
              is_truthy(manual_cut_offs())))
      
      cut_off_table <- tryCatch(
        expr = {
          dplyr::full_join(calculated_cut_offs(),
                           manual_cut_offs()) %>% 
            dplyr::mutate(`Based on samples` = purrr::map_chr(
              sample_type_list,
              ~ paste(ifelse(rlang::is_empty(.x),"No", "Yes,"),
                      comma_and(.x$sample_type)))) %>% 
            dplyr::ungroup() %>% 
            dplyr::select(-c(cluster,
                             sample_type_list,
                             curation_method)) %>% 
            dplyr::rename("Sum intensity cut-off" = cut_off_sum_intensity,
                          "Percentage of passing analytes cut-off" = cut_off_passing_analyte_percentage) %>% 
            dplyr::rename_with(firstupper)
          
        },
        error = function(e) {
          if (is_truthy(calculated_cut_offs())) {
            
            calculated_cut_offs() %>% 
              dplyr::mutate(`Based on sample types` = purrr::map_chr(
                sample_type_list,
                ~ paste("Yes,",
                        comma_and(.x$sample_type)))) %>% 
              dplyr::ungroup() %>% 
              dplyr::select(-c(cluster,
                               sample_type_list,
                               curation_method)) %>% 
              dplyr::rename("Cut-off sum intensity" = cut_off_sum_intensity,
                            "Cut-off percentage of passing analytes" = cut_off_passing_analyte_percentage) %>% 
              dplyr::rename_with(firstupper)
            
          } else {
            
            manual_cut_offs() %>% 
              dplyr::mutate(`Based on sample types` = "No") %>% 
              dplyr::ungroup()%>% 
              dplyr::rename("Cut-off sum intensity" = cut_off_sum_intensity,
                            "Cut-off percentage of passing analytes" = cut_off_passing_analyte_percentage) %>% 
              dplyr::select(-c(cluster,
                               curation_method)) %>% 
              dplyr::rename_with(firstupper)
            
          }
          
        }
      )
      
      return(cut_off_table)
    })
    
    output$table <- DT::renderDT({
      req(cut_off_table())
      
      DT::datatable(cut_off_table(),
                    rownames = FALSE,
                    width = "600px",
                    options = list(searching = FALSE,
                                   paging = FALSE,
                                   info = FALSE))
      
    })
    
    return(list(
      plot = my_plot,
      table = cut_off_table,
      cut_offs_to_use = cut_offs_to_use
    ))
    
  })
}
    
## To be copied in the UI
# mod_tab_cut_offs_ui("tab_cut_offs_ui_1")
    
## To be copied in the server
# mod_tab_cut_offs_server("tab_cut_offs_ui_1")
