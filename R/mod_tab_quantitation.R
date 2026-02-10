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
          ns("log_scale_quantities"),
          HTML("<i style='font-size:16px;'> Plot quantities on logarithmic scale </i>"),
          status = "success", right = TRUE, value = TRUE
        ),
        shinyjqui::jqui_resizable(plotly::plotlyOutput(ns("boxplots")))
      ),
      # Sum inntensity plots
      br(),
      br(),
      shinyjs::hidden(div(
        id = ns("div_sum_intensities"),
        strong("Natural vs labeled sum intensities"),
        style = "font-size: 24px",
        br(),
        div(
          shinyWidgets::materialSwitch(
            ns("log_scale_sum_intensities"),
            HTML("<i style='font-size:16px;'> Plot intensities on logarithmic scale </i>"),
            status = "success", right = TRUE, value = FALSE
          ),
          style = "font-size: 15px; font-style: italic"
        ),
        shinyjqui::jqui_resizable(plotly::plotlyOutput(ns("sum_intensities")))
      )),
      # Peptide correlation plots
      br(),
      br(),
      shinyjs::hidden(div(
        id = ns("div_peptide_correlations"),
        strong("Correlations between peptides"),
        style = "font-size: 24px",
        br(),
        div(
          shinyWidgets::materialSwitch(
            ns("log_scale_peptides"),
            HTML("<i style='font-size:16px;'> Plot quantities on logarithmic scale </i>"),
            status = "success",
            right = TRUE,
            value = TRUE
          ),
          style = "font-size: 15px; font-style: italic"
        ),
        shinyjqui::jqui_resizable(plotly::plotlyOutput(ns("peptide_correlations")))
      ))
    )
  )
}



#' tab_quantitation Server Functions
#'
#' @noRd 
mod_tab_quantitation_server <- function(id, 
                                        quantities,
                                        protein_data,
                                        intensities) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Show boxplots with quantities
    quantities_plot <- reactive({
      req(quantities)
      plot_protein_quantities(quantities, input$log_scale_quantities)
    })
    
    output$boxplots <- plotly::renderPlotly({
      req(quantities_plot())
      plotly::ggplotly(quantities_plot(), tooltip = "text") %>% 
        GlycoDash::hide_outliers(.)
    })
    
    
    # Correlation plots between peptide pairs
    peptide_correlation_plots <- reactive({
      req(protein_data)
      if (length(unique(protein_data$peptide_pair)) > 1) {
        plot_peptide_correlations(protein_data, input$log_scale_peptides)
      } 
      else {
        NULL
      }
    })
    
    output$peptide_correlations <- plotly::renderPlotly({
      req(peptide_correlation_plots())
      # Create list of interactive plots
      plotly_list <- purrr::map(
        peptide_correlation_plots(), function(plot) {
          plotly::ggplotly(plot, tooltip = "text")
        }
      )
      # Combine into subplot
      plotly::subplot(plotly_list, titleX = TRUE, titleY = TRUE,
                      nrows = ceiling(length(plotly_list) / 2), 
                      margin = 0.05)
    })
    
  
    
    # Sum intensity plots
    sum_intensity_plots <- reactive({
      req(intensities, protein_data)
      plot_sum_intensities(intensities, protein_data, input$log_scale_sum_intensities)
    })
    
    output$sum_intensities <- plotly::renderPlotly({
      req(sum_intensity_plots())
      plotly_list <- purrr::map(
        sum_intensity_plots(), function(plot) {
          plotly::ggplotly(plot, tooltip = "text")
        }
      )
      plotly::subplot(plotly_list, titleX = TRUE, titleY = TRUE,
                      nrows = ceiling(length(plotly_list) / 2), 
                      margin = 0.05)
    })
  
    
    # Set visibility of correlation plots.
    observe({
      if (is_truthy(peptide_correlation_plots())) {
        shinyjs::show("div_peptide_correlations")
      } else {
        shinyjs::hide("div_peptide_correlations")
      }
      
      if (is_truthy(sum_intensity_plots())) {
        shinyjs::show("div_sum_intensities")
      } else {
        shinyjs::hide("div_sum_intensities")
      }
    })
    
    
    return(list(
      quantities_plot = quantities_plot,
      correlation_plots = peptide_correlation_plots  # TODO: Rename
    ))
  })
}
    
