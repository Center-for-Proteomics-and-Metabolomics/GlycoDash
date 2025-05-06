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
      # Quantities boxplots
      br(),
      div(
        id = ns("boxplots_div"),
        strong("Protein quantities per sample type"),
        style = "font-size: 24px",
        br(),
        shinyWidgets::materialSwitch(
          ns("log_scale_1"),
          HTML("<i style='font-size:16px;'> Plot quantities on logarithmic scale </i>"),
          status = "success",
          right = TRUE,
          value = FALSE
        ),
        shinyjqui::jqui_resizable(plotly::plotlyOutput(ns("boxplots")))
      ),
      # Correlation plots
      # Need the div to be hidden by default for the toggling to work below
      br(),
      br(),
      shinyjs::hidden(div(
        id = ns("corplots_div"),
        strong("Correlations between peptides"),
        style = "font-size: 24px",
        br(),
        div(
          shinyWidgets::materialSwitch(
            ns("log_scale_2"),
            HTML("<i style='font-size:16px;'> Plot quantities on logarithmic scale </i>"),
            status = "success",
            right = TRUE,
            value = FALSE
          ),
          selectInput(
            ns("correlation_method"),
            "Method for calculating correlations:",
            choices = c("Pearson (linear)", "Spearman (non-parametric)"),
            selected = "Pearson (linear)"
          ),
          selectizeInput(
            ns("exclude_sample_types"),
            "Exclude sample types from correlations:",
            choices = c(""),
            multiple = TRUE
          ),
          style = "font-size: 15px; font-style: italic"
        ),
        shinyjqui::jqui_resizable(plotOutput(ns("corplots")))
      ))
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
    
    # Show boxplots with quantities
    quantities_plot <- reactive({
      req(quantities)
      plot_protein_quantities(quantities, input$log_scale_1)
    })
    
    output$boxplots <- plotly::renderPlotly({
      req(quantities_plot())
      plotly::ggplotly(quantities_plot(), tooltip = "text") %>% 
        GlycoDash::hide_outliers(.)
    })
    
    
    # Correlation plots with option to exclude sample types
    observe({
      req(protein_data)
      sample_types <- levels(protein_data$sample_type)
      updateSelectizeInput(
        inputId = "exclude_sample_types",
        choices = sample_types,
        options = list(maxItems = length(sample_types) - 1)
      )
    })
    
    correlation_plots <- reactive({
      req(protein_data)
      if (length(unique(protein_data$peptide_pair)) > 1) {
        protein_data_filtered <- protein_data %>% 
          dplyr::filter(!sample_type %in% input$exclude_sample_types)
        plot_peptide_correlations(protein_data_filtered, input$log_scale_2, 
                                  input$correlation_method)
      } else {
        NULL
      }
    })
    
    output$corplots <- renderPlot({
      req(correlation_plots())
      correlation_plots()
    }, res = 96)
  
    observe({
      if (is_truthy(correlation_plots())) {
        shinyjs::show("corplots_div")
      } else {
        shinyjs::hide("corplots_div")
      }
    })
    
    
    return(list(
      quantities_plot = quantities_plot,
      correlation_plots = correlation_plots,
      correlation_method = reactive(input$correlation_method),
      exclude_sample_types = reactive(input$exclude_sample_types)
    ))
  })
}
    
