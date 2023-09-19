# TODO
# - Make checkboxes work (calculation excluding peptides)
# - In the generated report, mention on which peptides the quantitation was based.
# - Delete already created correlation plot tabs after pushing button.
# - Pass results on to "Traits" and "Export results" tab.
# - Check what happens with missing values (NA) for peptides?
# - Make quantitation work in the case of Total and Specific antibodies.
# - Add info boxes.


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
              value = 5, min = 0, max = NA
            ),
            # Checkboxes to include/exclude peptides
            # TODO: toggle visibiliy, require at least 1 checked for quantitation button
            shinyWidgets::awesomeCheckboxGroup(
              ns("chosen_peptides"),
              "Peptides to include in the calculation:",
              choices = c("Glycopeptides", "GPSVFPLAPSSK", "TTPVLDSDGSFFLYSK"),
              selected = c("Glycopeptides", "GPSVFPLAPSSK", "TTPVLDSDGSFFLYSK")
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
            tabsetPanel(id = ns("tabs"))
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
            shinycssloaders::withSpinner(plotly::plotlyOutput(ns("quantitation_plot")))
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
      shinyjs::toggle(
        id = "chosen_peptides",
        condition = is_truthy(quantitation_clusters())
      )
      shinyjs::toggleState(
        id = "quantify_IgG1",
        condition = all(
          is_truthy(quantitation_clusters()),
          is_truthy(results_normalization$normalized_data()),
          length(input$chosen_peptides) >= 1
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
    
    # Calculate IgG1 amounts based on chosen peptides.
    IgG1_amounts <- reactive({
      req(IgG1_ratios(), input$silumab_amount)
      calculate_IgG1_amounts(IgG1_ratios(), input$chosen_peptides, input$silumab_amount)
    }) %>% bindEvent(input$quantify_IgG1)
  
    
    
    # Create peptide correlation plots.
    # TODO: Remove created plot tabs when amounts are re-calculated.
    r <- reactiveValues(created_tabs = vector("character"))  # To store IDs of created tabs
    observeEvent(IgG1_amounts(), {
      req(length(input$chosen_peptides) > 1)
      
      # Determine tab IDs and titles
      # TODO: make this a function
      peptide_list <- c("Glycopeptides", "GPSVFPLAPSSK", "TTPVLDSDGSFFLYSK")
      id_list <- c("glyco_vs_GPS", "glyco_vs_TTP", "GPS_vs_TTP")
      title_list <- c("Glycopeptides vs GPS", "Glycopeptides vs TTP", "GPS vs TTP")
      
      if (length(input$chosen_peptides) == 3) {
        tab_ids <- id_list
        tab_titles <- title_list
      } else {
        tab_ids <- dplyr::case_when(
          all(input$chosen_peptides == c("Glycopeptides", "GPSVFPLAPSSK")) ~ "glyco_vs_GPS",
          all(input$chosen_peptides == c("Glycopeptides", "TTPVLDSDGSFFLYSK")) ~ "glyco_vs_TTP",
          all(input$chosen_peptides == c("GPSVFPLAPSSK", "TTPVLDSDGSFFLYSK")) ~ "GPS_vs_TTP"
        )
        tab_titles <- dplyr::case_when(
          tab_ids == "glyco_vs_GPS" ~ "Glycopeptides vs GPS",
          tab_ids == "glyco_vs_TTP" ~ "Glycopeptides vs TTP",
          tab_ids == "GPS_vs_TTP" ~ "GPS vs TTP"
        )
      }
      
      # Create tabs and plots.
      purrr::imap(tab_ids, function(tab_id, i) {
        plot <- plot_peptide_correlation(IgG1_amounts(), tab_id)
        output[[tab_id]] <- plotly::renderPlotly(plotly::ggplotly(plot, tooltip = "text"))
        appendTab(
          inputId = "tabs",
          select = TRUE,
          tab = tabPanel(
            title = tab_titles[[i]],
            plotly::plotlyOutput(ns(tab_id))
          )
        )
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