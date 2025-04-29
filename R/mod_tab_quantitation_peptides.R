#' tab_quantitation_peptides UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tab_quantitation_peptides_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjqui::jqui_resizable(plotly::plotlyOutput(ns("plot"))),
    sliderInput(
      ns("mass_accuracy"),
      "Acceptable mass accuracy (ppm)",
      min = -50, max = 50, value = c(-20, 20)
    ),
    selectizeInput(
      ns("exclude_sample_types"),
      "Sample types to exclude from the quality assessment:",
      choices = c(""), 
      multiple = TRUE
    )
  )
}



#' tab_quantitation_peptides Server Functions
#'
#' @noRd 
mod_tab_quantitation_peptides_server <- function(id, 
                                                 peptides_data,
                                                 results_spectra_curation) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Option to exclude sample types
    observeEvent(peptides_data, {
      sample_types <- as.character(unique(peptides_data$sample_type))
      updateSelectizeInput(
        inputId = "exclude_sample_types",
        choices = c(sample_types),
        options = list(maxItems = length(sample_types) - 1)
      )
    })
    
    # Generate quality plot using function from site occupancy
    quality_plot <- reactive({
      req(peptides_data, input$mass_accuracy)
      summary <- summarize_peptides_quality(
        peptides_quality = peptides_data %>% 
          dplyr::filter(!sample_type %in% input$exclude_sample_types),
        ipq = results_spectra_curation$ipq(),
        sn = results_spectra_curation$sn(),
        idp = results_spectra_curation$idp(),
        total_area = results_spectra_curation$total_area(),
        mass_accuracy = input$mass_accuracy
      )
      plot <- peptides_quality_plot(summary)
      return(plot)
    })
    
    output$plot <- plotly::renderPlotly({
      req(quality_plot())
      plotly::ggplotly(quality_plot(), tooltip = "text")
    })
    
    
    return(list(
      quality_plot = quality_plot,
      exclude_sample_types = reactive(input$exclude_sample_types),
      mass_accuracy = reactive(input$mass_accuracy)
    ))
  })
}

