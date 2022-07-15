#' tab_repeatability UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tab_repeatability_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        br(),
        tags$style(
          HTML(paste0("#",
                      ns("assess_repeatability"),
                      ".btn {float: left; padding: 6px 12px; border-color: #ddd; border: 1px solid;}"
          ))
        ),
        shinydashboard::box(
          title = "Select a standard",
          width = 4,
          solidHeader = TRUE,
          status = "primary",
          uiOutput(ns("standards_menu")),
          shinyWidgets::materialSwitch(ns("by_plate"),
                                       "Group samples by plate.",
                                       right = TRUE,
                                       status = "primary"),
          actionButton(ns("assess_repeatability"),
                       label = "Assess repeatability")
        )
      ),
      fluidRow(
        column(
          width = 12,
          shinydashboard::box(
            title = "Results",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            column(
              width = 9,
              plotly::plotlyOutput(ns("plot"))
            ),
            column(
              width = 3,
              br(),
              DT::dataTableOutput(ns("table"))
            )
          )
        )
      )
    )
  )
}

#' tab_repeatability Server Functions
#'
#' @noRd 
mod_tab_repeatability_server <- function(id, my_data, Ig_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    x <- reactiveValues()
    
    # change the renderUI to just updating a selectInput (because now I use only
    # one input regardless of the group being there or not)
    
    output$standards_menu <- renderUI({
      req(my_data())
      #req(Ig_data())
      
      menu <- my_data() %>% 
        dplyr::ungroup() %>% 
        dplyr::select(tidyselect::any_of(c("sample_name", "group", "sample_id"))) %>%
        dplyr::distinct() %>%
        dplyr::group_by(dplyr::across(tidyselect::any_of("group"))) %>% 
        dplyr::add_count(sample_id, name = "number_of_replicates_after_curation") %>% 
        dplyr::mutate(number_of_replicates_after_curation = ifelse(sample_id == "empty cell in plate design",
                                                                   1,
                                                                   number_of_replicates_after_curation)) %>% 
        dplyr::mutate(replicates_after_curation = number_of_replicates_after_curation > 1) %>% 
        dplyr::filter(replicates_after_curation == TRUE) %>% 
        dplyr::distinct(dplyr::across(tidyselect::any_of(c("group", "sample_id")))) %>% 
        purrr::pmap_chr(.,
                        paste)
      
      selectInput(ns("sample_menu"),
                  label = "Choose which samples you want to assess:",
                  choices = menu)
    })
    
    selected_sample_id <- reactive({
      req(input$sample_menu)
      req(my_data())
      stringr::str_extract(input$sample_menu,
                           unique(my_data()$sample_id)) %>% 
        na.omit()
    }) %>% bindEvent(input$assess_repeatability)
    
    selected_group <- reactive({
      req(input$sample_menu)
      req(my_data())
      if (Ig_data() == "Yes") {
        group <- stringr::str_extract(
          string = input$sample_menu,
          pattern = unique(my_data()$group)) %>% 
          na.omit(.)
      } else {
        group <- NULL
      }
      return(group)
    }) %>% bindEvent(input$assess_repeatability)
    
    
    observeEvent(input$assess_repeatability, {
      # Reset x$repeatability and x$variation_table:
      x$repeatability <- NULL
      x$variation_table <- NULL
      
      if (is_truthy(input$by_plate)) {
        # Try to calculate the repeatability stats, but show a notification if
        # there are no samples of the selected sample type and group combination:
        tryCatch(
          expr = {
            x$repeatability <- calculate_repeatability_stats(
              data = my_data(),
              standard_sample_id = selected_sample_id(),
              standard_group = selected_group())
          },
          no_samples = function(c) {
            showNotification(c$message, type = "error")
          }
        )
        
        # Pause until x$repeatability has been calculated:
        req(x$repeatability)
        
        # Calculate the intra-plate variations to show in the table:
        x$variation_table <- x$repeatability %>% 
          dplyr::group_by(plate) %>% 
          dplyr::summarise(intra_plate_variation = mean(RSD, na.rm = TRUE))
      }
    })
    
    plot <- reactive({
      if (is_truthy(input$by_plate)) {
        req(x$repeatability)
        plot <- visualize_repeatability2(x$repeatability)
      } else {
        req(my_data())
        req(selected_sample_id())
        plot <- visualize_repeatability_mean_bars(
          my_data(),
          selected_sample_id = selected_sample_id(),
          selected_group = selected_group()
        )
      }
      
      return(plot)
    })
    
    output$plot <- plotly::renderPlotly({
      req(plot())
      
      plotly_object <- plotly::ggplotly(plot(), tooltip = "text")
      
      plotly_object <- change_axis_title_distance(plotly_object, 
                                                  y_distance = 90)
      
      plotly_object
    })
    
    for_table <- reactive({
      req(x$variation_table)
      x$variation_table %>% 
        dplyr::mutate(intra_plate_variation = signif(intra_plate_variation,
                                                     digits = 3))
    })
    
    output$table <- DT::renderDataTable({
      req(for_table())
      sketch <- htmltools::withTags(table(
        DT::tableHeader(c("Plate", "Intra-plate variation (%)")),
        DT::tableFooter(c("Inter-plate variation (%)", 
                          signif(mean(for_table()$intra_plate_variation, 
                                      na.rm = TRUE),
                                 digits = 3)))
      ))
      
      DT::datatable(data = for_table(),
                    container = sketch,
                    rownames = FALSE,
                    filter = "none",
                    options = list(searching = FALSE,
                                   paging = FALSE))
    })
    
    # title_for_report <- reactive({
    #   paste(input$standard_sample_type,
    #         ifelse(is.null(input$standard_group), "", input$standard_group))
    # })
    
    return(list(
      plot = plot,
      for_table = for_table#,
      #title_for_report = title_for_report
    ))
    
  })
}
    
## To be copied in the UI
# mod_tab_repeatability_ui("tab_repeatability_ui_1")
    
## To be copied in the server
# mod_tab_repeatability_server("tab_repeatability_ui_1")
