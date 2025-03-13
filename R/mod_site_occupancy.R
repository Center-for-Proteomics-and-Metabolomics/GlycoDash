#' site_occupancy UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_site_occupancy_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(paste0(
      "#", ns("box_header1"), " .awesome-checkbox {padding-top: 7px}",
      "#", ns("box_header1"), " .popover {max-width: 400px !important; color: #333}",
      "#", ns("box_header1"), " .fas {float: right; margin-right: 5px; font-size: 18px}",
      "#", ns("box_header1"), " .direct-chat-contacts {right: 0; background: #222d32!important}",
      "#", ns("box_header1"), " .btn {float: right; border-width: 0px; margin-right: 10px}",
      "#", ns("box_header1"), " .dropdown-menu {background: #333; right: -30px; left: auto; top: 28px;}",
      "#", ns("box1"), " .box-title {width: 100%}",
      "#", ns("box1"), " .dropdown {display: inline-block; float: right; width: 330px}",
      "#", ns("box_header2"), " .awesome-checkbox {padding-top: 7px}",
      "#", ns("box_header2"), " .popover {max-width: 400px !important; color: #333}",
      "#", ns("box_header2"), " .fas {float: right; margin-right: 5px; font-size: 18px}",
      "#", ns("box_header2"), " .direct-chat-contacts {right: 0; background: #222d32!important}",
      "#", ns("box_header2"), " .btn {float: right; border-width: 0px; margin-right: 10px}",
      "#", ns("box_header2"), " .dropdown-menu {background: #333; right: -30px; left: auto; top: 28px;}",
      "#", ns("box2"), " .box-title {width: 100%}",
      "#", ns("box2"), " .dropdown {display: inline-block; float: right; width: 330px}"
    ))),
    fluidPage(
      fluidRow(h1("Site occupancy")),
      fluidRow(
        column(
          width = 6,
          shinydashboardPlus::box(
            id = ns("box1"),
            title = div(
              id = ns("box_header1"),
              "Non-glycosylated peptides",
              icon("info-circle", class = "ml") %>% 
                bsplus::bs_embed_popover(
                  title = "Explanation",
                  content = HTML("
                    When a non-glycosylated peptide for a glycosylation site is
                    present in your data, the corresponding site occupancy can be
                    calculated by dividing the intensity of the glycopeptides
                    by the summed intensity of the both the glycopeptides and the 
                    non-glycosylated peptide, and multiplying by 100%.
                    <br><br>
                    You can choose to exclude non-glycosylated peptide ions from 
                    the site occupancy calculations (e.g., when the quality 
                    of the data is insufficient for a charge state.)
                  "),
                  trigger = "hover",
                  placement = "bottom",
                  html = "true"
                )
            ),
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            # Text for when there are no non-glycosylated peptides
            div(
              id = ns("no_peptides"),
              strong("Your samples do not contain non-glycosylated peptides
                     that can be used to calculate site occupancies.\n\n"),
              style = "color:#0021B8; font-size: 16px"
            ),
            # Detected clusters in table
            div(
              id = ns("info_clusters"),
              HTML("
                <strong> 
                The following non-glycosylated peptide ions were detected
                and can be used to calculate the corresponding site occupancies:
                </strong> 
                <br> <br>
              ")
            ),
            tableOutput(ns("peptides_table")),
            # Option to exclude peptides from calculation
            selectizeInput(
              ns("exclude_peptides"),
              "Peptide ions to exclude from site occupancy calculations:",
              choices = c(""), 
              multiple = TRUE
            )
          )
        ),
        column(
          width = 6,
          shinydashboardPlus::box(
            id = ns("box2"),
            title = div(
              id = ns("box_header2"),
              "Quality check",
              icon("info-circle", class = "ml") %>% 
                bsplus::bs_embed_popover(
                  title = "Explanation",
                  content = HTML("
                  For each non-glycosylated peptide ion in your data,
                  the percentage of samples in which the ion fulfills three
                  quality criteria is plotted. For S/N and IPQ (in the case 
                  of LaCyTools data), or total area and IDP (in the case 
                  of Skyline data), the same quality criteria that were used
                  for spectral and analyte curation are applied.
                  <br> <br>
                  Because non-glycosylated peptides are often integrated without
                  calibration, you may want to be more lenient when it comes to
                  the acceptable mass error. This value can be set below.
                  "),
                  trigger = "hover",
                  placement = "bottom",
                  html = "true"
                )
            ),
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            downloadButton(ns("download"), "Download quality details"),
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
        ),
      ),
      fluidRow(
        shinydashboard::box(
          title = "View data with site occupancies",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          DT::dataTableOutput(ns("data_table"))
        )
      )
    )
  )
}
 

   
#' site_occupancy Server Functions
#'
#' @noRd 
mod_site_occupancy_server <- function(id,
                                      results_spectra_curation,
                                      results_analyte_curation,
                                      results_normalization,
                                      results_quantitation,
                                      results_derived_traits) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Get quality info on peptides
    peptides_quality <- reactive({
      req(results_analyte_curation$analyte_curated_data())
      data <- results_analyte_curation$analyte_curated_data() %>% 
        dplyr::filter(analyte == paste0(cluster, "1")) %>% 
        dplyr::select(sample_name, sample_id, sample_type, cluster, analyte, charge,
                      tidyselect::any_of(c(
                        "group",
                        "absolute_intensity_background_subtracted",
                        "mass_accuracy_ppm",
                        "isotopic_pattern_quality",
                        "sn",
                        "fraction",
                        "total_area",
                        "isotope_dot_product"
                      )))
      if (nrow(data) > 0) {
        data
      } else{
        NULL
      }
    })
    
    # Allow for downloading of peptides quality
    observe({
      shinyjs::toggleState("download", is_truthy(peptides_table()))
    })
    
    output$download <- downloadHandler(
      filename = function() {
        current_datetime <- paste0(format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M"))
        paste0(current_datetime, "_site_occupancy_peptides_quality.xlsx")
      },
      content = function(file) {
        writexl::write_xlsx(
          peptides_quality(),
          path = file
        )
      }
    )
    

    # Generate table with peptides and charge states
    peptides_table <- reactive({
      req(peptides_quality())
      peptides_quality() %>%
        dplyr::select(cluster, charge) %>%
        dplyr::distinct() %>%
        dplyr::rename(Peptide = cluster, Charge = charge)
    })

    output$peptides_table <- renderTable({
      req(peptides_table())
      peptides_table()
    }, striped = TRUE, bordered = TRUE, rownames = TRUE, align = "c")
    
    
    # Option to exclude sample types from quality assessment
    observeEvent(peptides_quality(), {
      sample_types <- as.character(unique(peptides_quality()$sample_type))
      updateSelectizeInput(
        inputId = "exclude_sample_types",
        choices = c(sample_types),
        options = list(maxItems = length(sample_types) - 1)
      )
    })
    
    # Create a quality plot
    quality_plot <- reactive({
      req(peptides_quality())
      summary <- summarize_peptides_quality(
        peptides_quality = peptides_quality() %>% 
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
    
    
    # Create choices for peptide ions that can be excluded
    observeEvent(peptides_table(), {
      choices <- peptides_table() %>% 
        dplyr::mutate(ion = paste0(Peptide, ", ", Charge)) %>% 
        dplyr::pull(ion)
      updateSelectizeInput(
        inputId = "exclude_peptides",
        choices = choices
      )
    })
    
    
    # Calculate intensities of peptides
    peptides_intensities <- reactive({
      req(peptides_quality())
      calculate_peptides_intensities(peptides_quality(), input$exclude_peptides)
    })
    
    
    # Calculate site occupancies
    site_occupancy <- reactive({
      req(peptides_intensities(), results_normalization$normalized_data_wide())
      calculate_site_occupancy(
        peptides_intensities(), 
        results_normalization$normalized_data_wide(),
        peptides_table()
      )
    })
    
  
  
    # Combine data with IgG1 quantities and/or traits
    data_combined <- reactive({
      req(site_occupancy())
      if (is_truthy(results_derived_traits$data_with_traits())) {
        dplyr::left_join(results_derived_traits$data_with_traits(), site_occupancy()) %>% 
          dplyr::relocate(tidyselect::contains("site_occupancy"),
                          .after = tidyselect::contains("sum_intensity"))
      } else if (is_truthy(results_quantitation$quantitation_data())) {
        dplyr::left_join(results_quantitation$quantitation_data(), site_occupancy()) %>% 
          dplyr::relocate(IgG1_quantity_ng, .after = replicates)
      } else {
        site_occupancy()
      }
    })
    
  
    # Show data in table
    output$data_table <- DT::renderDT({
      req(data_combined())
      DT::datatable(data = data_combined() %>% 
                      dplyr::mutate_if(is.numeric, ~format(round(., 2), nsmall = 2)),
                    options = list(
                      scrollX = TRUE,
                      pageLength = 6,
                      columnDefs = list(list(className = "dt-center", targets = "_all"))
                    ), filter = "top")
    })
    
    
    # Toggle UI
    observe({
      if (is_truthy(peptides_quality())) {
        shinyjs::hide("no_peptides")
        shinyjs::show("info_clusters")
        shinyjs::show("exclude_peptides")
        shinyjs::show("plot")
        shinyjs::show("mass_accuracy")
      } else {
        shinyjs::show("no_peptides")
        shinyjs::hide("info_clusters")
        shinyjs::hide("exclude_peptides")
        shinyjs::hide("plot")
        shinyjs::hide("mass_accuracy")
      }
    })
    
    
    return(list(
      site_occupancy_data = data_combined,
      quality_plot = quality_plot,
      mass_accuracy = reactive(input$mass_accuracy),
      exclude_peptides = reactive(input$exclude_peptides)
    ))
    
  })
}




