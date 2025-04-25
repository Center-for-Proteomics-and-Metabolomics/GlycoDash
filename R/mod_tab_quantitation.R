#' tab_quantitation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tab_quantitation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      width = 12,
      br(),
      div(
        id = ns("boxplots_title"),
        strong("Protein quantities per sample type"),
        style = "font-size: 24px"
      ),
      br(),
      shinyWidgets::materialSwitch(
        ns("log_scale_1"),
        HTML("<i style='font-size:15px;'> Plot quantities on logarithmic scale </i>"),
        status = "success",
        right = TRUE,
        value = FALSE
      ),
      shinyjqui::jqui_resizable(plotly::plotlyOutput(ns("boxplots"))),
      br(),
      div(
        id = ns("corplots_title"),
        strong("Correlations between peptides"),
        style = "font-size: 24px"
      ),
      br(),
      shinyWidgets::materialSwitch(
        ns("log_scale_2"),
        HTML("<i style='font-size:15px;'> Plot quantities on logarithmic scale </i>"),
        status = "success",
        right = TRUE,
        value = FALSE
      ),
      br(),
      shinyjqui::jqui_resizable(plotOutput(ns("corplots")))
    )
  )
}
    
#' tab_quantitation Server Functions
#'
#' @noRd 
mod_tab_quantitation_server <- function(id, 
                                        quantities,
                                        protein_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    quantities_plot <- reactive({
      req(quantities)
      plot_protein_quantities(quantities, input$log_scale_1)
    })
    
    output$boxplots <- plotly::renderPlotly({
      req(quantities_plot())
      plotly::ggplotly(quantities_plot(), tooltip = "text") %>% 
        GlycoDash::hide_outliers(.)
    })
    
    correlation_plots <- reactive({
      req(protein_data)
      if (length(unique(protein_data$peptide_pair)) > 1) {
        plot_peptide_correlations(protein_data, input$log_scale_2)
      } else {
        NULL
      }
    })
    
    output$corplots <- renderPlot({
      req(correlation_plots())
      correlation_plots()
    }, res = 96)
    
    
    # Toggle visibility of correlation
    observe({
      if (is_truthy(correlation_plots())) {
        shinyjs::show("corplots_title")
        shinyjs::show("log_scale_2")
      } else {
        shinyjs::hide("corplots_title")
        shinyjs::hide("log_scale_2")
      }
    })
    
    
    return(list(
      quantities = quantities,
      quantities_plot = quantities_plot
    ))
  })
}
    
