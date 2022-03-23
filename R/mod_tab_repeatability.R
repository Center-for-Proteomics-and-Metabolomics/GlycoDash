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
              plotOutput(ns("plot"))
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
      x$repeatability <- calculate_repeatability_stats(data = data(),
                                                       standard_sample_type = input$standard_sample_type,
                                                       standard_group = input$standard_group)
      x$variation_table <- x$repeatability %>% 
        dplyr::summarise(intra_plate_variation = mean(RSD, na.rm = TRUE))
    })
    
    output$plot <- renderPlot({
      req(x$repeatability)
      visualize_repeatability(x$repeatability)
    })
    
    output$table <- DT::renderDataTable({
      req(x$variation_table)
      
      for_table <- x$variation_table %>% 
        dplyr::mutate(intra_plate_variation = signif(intra_plate_variation,
                                                     digits = 3))
      
      sketch <- htmltools::withTags(table(
        DT::tableHeader(c("Plate", "Intra-plate variation (%)")),
        DT::tableFooter(c("Inter-plate variation (%)", 
                          signif(mean(for_table$intra_plate_variation, 
                                      na.rm = TRUE),
                                 digits = 3)))
      ))
      
      DT::datatable(data = for_table,
                    container = sketch,
                    rownames = FALSE,
                    filter = "none",
                    options = list(searching = FALSE,
                                   paging = FALSE))
    })
    
    return(list(
      plot = reactive({output$plot}),
      table = reactive({output$table})
    ))
    
  })
}
    
## To be copied in the UI
# mod_tab_repeatability_ui("tab_repeatability_ui_1")
    
## To be copied in the server
# mod_tab_repeatability_server("tab_repeatability_ui_1")
