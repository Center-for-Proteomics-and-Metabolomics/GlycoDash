#' quantitation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_quantitation_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidPage(
      fluidRow(h1("IgG1 quantitation")),
      fluidRow(
        column(
          width = 5,
          shinydashboard::box(
            title = "IgG1 quantitation using SILuMAb",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            # Text for when there is no SILuMAb
            div(
              id = ns("no_silumab"),
              strong("Your samples do not contain SILuMAb for IgG1 quantitation.\n\n"),
              style = "color:#0021B8; font-size: 16px"
            ),
            # Input for SILuMAb amounts.
            numericInput(
              ns("silumab_amount"),
              "Amount of SILuMAb per sample (ng):",
              value = 5000, min = 0, max = NA
            ),
            # Button to cquantify IgG1
            actionButton(
              ns("quantify_IgG1"),
              "Quantify IgG1"
            )
          )
        ),
        column(
          width = 7,
          shinydashboard::box(
            title = "Peptide correlations",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            tabsetPanel(id = ns("tabs"),
              tabPanel(
                title = "Glycopeptides vs GPS",
                plotly::plotlyOutput(ns("glyco_vs_GPS"))
              ),
              tabPanel(
                title = "Glycopeptides vs TTP",
                plotly::plotlyOutput(ns("glyco_vs_TTP"))
              ),
              tabPanel(
                title = "GPS vs TTP",
                plotly::plotlyOutput(ns("GPS_vs_TTP"))
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          shinydashboard::box(
            title = "IgG1 quantitation plot",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            plotly::plotlyOutput(ns("quantitation_plot"))
          )
        )
      )
    )
  )
  
}




#' quantitation Server Functions
#'
#' @noRd 
mod_quantitation_server <- function(id, quantitation_clusters,
                                    LaCyTools_summary,
                                    analyte_curated_data,
                                    results_normalization) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Toggle UI elements
    observe({
      shinyjs::toggle(
        id = "no_silumab",
        condition = !is_truthy(quantitation_clusters())
      )
      shinyjs::toggle(
        id = "silumab_amount",
        condition = is_truthy(quantitation_clusters())
      )
      shinyjs::toggle(
        id = "quantify_IgG1",
        condition = is_truthy(quantitation_clusters())
      )
      shinyjs::toggleState(
        id = "quantify_IgG1",
        condition = all(
          is_truthy(quantitation_clusters()),
          is_truthy(results_normalization$normalized_data())
        )
      )
    })
    
    
    # Calculate ratios of peptides.
    IgG1_ratios <- reactive({
      req(is_truthy(quantitation_clusters()), results_normalization$normalized_data())
      IgG1_sum_intensities <- calculate_IgG1_sum_intensities(
        LaCyTools_summary(), quantitation_clusters(), analyte_curated_data()
      )
      ratios <- calculate_IgG1_ratios(IgG1_sum_intensities, quantitation_clusters())
      return(ratios)
    })
    
    # Calculate IgG1 amounts
    IgG1_amounts <- reactive({
      req(IgG1_ratios(), input$silumab_amount)
      IgG1_ratios() %>% 
        dplyr::mutate(
          IgG1_median_amount = median_ratio * input$silumab_amount,
          # Use scientific formatting
          IgG1_median_amount = format(IgG1_median_amount, scientific = TRUE, digits = 2)
        )
    }) %>% bindEvent(input$quantify_IgG1)
  
    
    
    # Create peptide correlation plots
    purrr::map(c("glyco_vs_GPS", "glyco_vs_TTP", "GPS_vs_TTP"), ~ {
      plot <- reactive({
        plot_peptide_correlation()
      })
      output[[.x]] <- plotly::renderPlotly({
        req(plot())
        plotly::ggplotly(plot(), tooltip = "text")
      })
    })
    
    
    # Create a plot with quantitation results
    quantitation_plot <- reactive({
      req(IgG1_amounts())
      create_quantitation_plot(IgG1_amounts())
    })
    
    output$quantitation_plot <- plotly::renderPlotly({
      req(quantitation_plot())
      plotly_object <- plotly::ggplotly(quantitation_plot(), tooltip = "text") %>% 
        # plotly ignores "outlier.shape" so use function from utils
        GlycoDash::hide_outliers(.)
      return(plotly_object)
    })
    

  })
}