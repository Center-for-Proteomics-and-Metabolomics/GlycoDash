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
              shinyjqui::jqui_resizable(plotly::plotlyOutput(ns("plot"))),
              tabsetPanel(id = ns("plot_tabs"))
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
mod_tab_repeatability_server <- function(id, my_data, contains_total_and_specific_samples){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    x <- reactiveValues()
    
    observe({
      req(!is.null(input$by_plate)) # Needed because otherwise this observer
      # runs before input$by_plate is rendered.
      shinyjs::toggle(id = "by_plate",
                      condition = "plate_well" %in% colnames(my_data()))
    })
    
    # change the renderUI to just updating a selectInput (because now I use only
    # one input regardless of the group being there or not)
    
    output$standards_menu <- renderUI({
      req(my_data())
      #req(contains_total_and_specific_samples())
      
      # TODO: convert to function:
      menu_df <- my_data() %>% 
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
        dplyr::distinct(dplyr::across(tidyselect::any_of(c("group", "sample_id")))) 
      
      if ("group" %in% colnames(menu_df)) {
        menu <- purrr::pmap(menu_df,
                            function(group, sample_id) {
                              paste("group:", group, "sample_id:", sample_id)
                            })
      } else {
        menu <- paste("sample_id:", menu_df$sample_id)
      }
      
      selectInput(ns("sample_menu"),
                  label = "Choose which samples you want to assess:",
                  choices = menu)
    })
    
    selected_sample_id <- reactive({
      req(input$sample_menu)
      req(my_data())
      
      stringr::str_extract(input$sample_menu,
                           "(?<=sample_id: ).+$") %>% 
        na.omit()
    }) %>% bindEvent(input$assess_repeatability)
    
    selected_group <- reactive({
      req(input$sample_menu)
      req(my_data())
      if (contains_total_and_specific_samples() == "Yes") {
        group <- stringr::str_extract(
          string = input$sample_menu,
          pattern = "(?<=group: ).+(?= sample_id: .+)") %>% 
          na.omit(.)
      } else {
        group <- NULL
      }
      return(group)
    }) %>% bindEvent(input$assess_repeatability)
    
    
    repeatability <- reactive({
      req(input$by_plate)
      # Try to calculate the repeatability stats, but show a notification if
      # there are no samples of the selected sample type and group combination:
      repeatability <- tryCatch(
        expr = {
          calculate_repeatability_stats(
            data = my_data(),
            standard_sample_id = selected_sample_id(),
            standard_group = selected_group()
          )
        },
        no_samples = function(c) {
          showNotification(c$message, type = "error")
          NULL
        }
      )
      
      return(repeatability)
    }) %>% bindEvent(input$assess_repeatability)
    
    variation_df <- reactive({
      req(repeatability())
      
      # Calculate the intra-plate variations to show in the table:
      repeatability() %>% 
        dplyr::group_by(plate) %>% 
        dplyr::summarise(intra_plate_variation = mean(RSD, na.rm = TRUE))
    })
    
    # observeEvent(input$assess_repeatability, {
    #   # Reset x$repeatability and x$variation_table:
    #   x$repeatability <- NULL
    #   x$variation_table <- NULL
    #   
    #   if (is_truthy(input$by_plate)) {
    #     # Try to calculate the repeatability stats, but show a notification if
    #     # there are no samples of the selected sample type and group combination:
    #     tryCatch(
    #       expr = {
    #         x$repeatability <- calculate_repeatability_stats(
    #           data = my_data(),
    #           standard_sample_id = selected_sample_id(),
    #           standard_group = selected_group())
    #       },
    #       no_samples = function(c) {
    #         showNotification(c$message, type = "error")
    #       }
    #     )
    #     
    #     # Pause until x$repeatability has been calculated:
    #     req(x$repeatability)
    #     
    #     # Calculate the intra-plate variations to show in the table:
    #     x$variation_table <- x$repeatability %>% 
    #       dplyr::group_by(plate) %>% 
    #       dplyr::summarise(intra_plate_variation = mean(RSD, na.rm = TRUE))
    #   }
    # })
    
    plot <- reactive({
      req(length(clusters()) <= 4)
      if (is_truthy(input$by_plate)) {
        req(repeatability())
        plot <- visualize_repeatability2(repeatability())
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
    
    observe({
      req(!is.null(input$by_plate))
      shinyjs::toggle(id = "plot_tabs",
                      condition = length(clusters()) > 4)
      shinyjs::toggle(id = "plot",
                      condition = length(clusters()) <= 4)
    })
    
    clusters <- reactive({
      req(my_data())
      unique(my_data()$cluster)
    })
    
    observe({
      req(clusters())
      req(length(clusters()) > 4)
      req(!is.null(input$by_plate))
      
      # Remove tabs in case they were already there before:
      purrr::map(clusters(),
                 function(current_cluster) {
                   removeTab(inputId = "plot_tabs",
                             target = current_cluster)
                 })
      
      # Create a tab with a plot for each cluster:
      purrr::map(clusters(),
                 function(current_cluster) {
                   appendTab(inputId = "plot_tabs",
                             tabPanel(
                               title = current_cluster,
                               mod_tab_repeatability_plot_ui(ns(paste0(current_cluster,
                                                                      "repeatability_plot")))
                             ))
                 })
      
      # Save the plots on the tabs in the reactiveValues list x in the list plots:
      x$plots <- rlang::set_names(clusters()) %>% 
        purrr::map(.,
                   function(current_cluster) {
                     mod_tab_repeatability_plot_server(
                       id = paste0(current_cluster,
                                   "repeatability_plot"),
                       by_plate = reactive({ input$by_plate }),
                       repeatability = reactive({ repeatability() %>% 
                           dplyr::filter(cluster == current_cluster) }),
                       my_data = reactive({ my_data() %>% 
                           dplyr::filter(cluster == current_cluster) }),
                       selected_sample_id = selected_sample_id,
                       selected_group = selected_group
                     )
                   })
      
    }) %>% bindEvent(input$assess_repeatability) # needed?
    
    output$plot <- plotly::renderPlotly({
      req(plot())
      
      plotly_object <- plotly::ggplotly(plot(), tooltip = "text")
      
      plotly_object <- change_axis_title_distance(plotly_object, 
                                                  y_distance = 90)
      
      plotly_object
    })
    
    for_table <- reactive({
      req(variation_df())
      variation_df() %>% 
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
    
    return(list(
      plot = plot,
      plots = reactive({ x$plots }),
      table = for_table,
      title_for_report = reactive({ input$sample_menu })
    ))
    
  })
}
