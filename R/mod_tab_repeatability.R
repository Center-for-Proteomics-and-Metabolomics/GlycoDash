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
mod_tab_repeatability_server <- function(id, data, Ig_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    x <- reactiveValues()
    
    output$standards_menu <- renderUI({
      req(data())
      req(Ig_data())
      
      sample_type_menu <- selectInput(ns("standard_sample_type"),
                                      label = "Choose which standard you want to assess:",
                                      choices = unique(data()$sample_type))
      
      if (Ig_data() == "Yes") {
        groups_menu <- selectInput(ns("standard_group"),
                                   label = "Choose if you want to look at total or specific Ig samples:",
                                   choices = unique(data()$group))
      }
      
      menu <- list(sample_type_menu, 
                   get0("groups_menu"))
      menu[sapply(menu, is.null)] <- NULL
      
      return(menu)
    })
    
    observeEvent(input$assess_repeatability, {
      # Reset x$repeatability and x$variation_table:
      x$repeatability <- NULL
      x$variation_table <- NULL
      
      # Try to calculate the repeatability stats, but show a notification if
      # there are no samples of the selected sample type and group combination:
      tryCatch(
        expr = {
          x$repeatability <- calculate_repeatability_stats(
            data = data(),
            standard_sample_type = input$standard_sample_type,
            standard_group = input$standard_group)
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
    })
    
    plot <- reactive({
      req(x$repeatability)
      visualize_repeatability(x$repeatability)
    })
    
    output$plot <- plotly::renderPlotly({
      req(plot())
      
      # ay <- list(
      #   overlaying = "y",
      #   side = "right",
      #   title = "RSD (%)"
      # )
      
      plotly_object <- plotly::ggplotly(plot(), tooltip = "text") #%>%
        # plotly::add_lines(x = ~2:4, y = ~1:3, 
        #                   color = "transparent", 
        #                   name = "", yaxis = "y2", hoverinfo='skip', showlegend=FALSE) %>%
        # plotly::layout(yaxis2 = ay)
      
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
    
    title_for_report <- reactive({
      paste(input$standard_sample_type,
            ifelse(is.null(input$standard_group), "", input$standard_group))
    })
    
    return(list(
      plot = plot,
      for_table = for_table,
      title_for_report = title_for_report
    ))
    
  })
}
    
## To be copied in the UI
# mod_tab_repeatability_ui("tab_repeatability_ui_1")
    
## To be copied in the server
# mod_tab_repeatability_server("tab_repeatability_ui_1")
