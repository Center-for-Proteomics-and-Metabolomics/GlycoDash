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
                    calculated by the diving the sum intensity of the glycopeptides
                    by the intensity of the non-glycosylated peptide.
                    <br><br>
                    You can choose to exclude a non-glycosylated peptide from 
                    the site occupancy calculations (e.g., when the quality 
                    of the data is insufficient.)
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
              strong("Your samples do not contain non-glycosylated peptides.\n\n"),
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
                  content = HTML("Text..."),
                  trigger = "hover",
                  placement = "bottom",
                  html = "true"
                )
            ),
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            shinyjqui::jqui_resizable(plotly::plotlyOutput(ns("plot")))
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
      data <- peptides_quality() %>% 
        dplyr::select(sample_name, sample_id, sample_type,
                      cluster, charge, tidyselect::any_of(c(
                        "group",
                        "absolute_intensity_background_subtracted",
                        "fraction",
                        "total_area"
                      ))) %>% 
        # Ignore ions when applicable
        dplyr::mutate(ion = paste0(cluster, ", ", charge), .after = charge) %>% 
        dplyr::filter(!ion %in% input$exclude_peptides)
      
      if (nrow(data) == 0) {
        return(NULL)
      }
      else if ("fraction" %in% colnames(data)) {
        data <- data %>% 
          dplyr::mutate(
            intensity_by_fraction = absolute_intensity_background_subtracted / fraction
          ) %>% 
          dplyr::group_by(sample_name, cluster) %>% 
          dplyr::mutate(total_intensity = sum(intensity_by_fraction)) %>% 
          dplyr::ungroup() %>% 
          dplyr::select(sample_name, sample_id, sample_type, cluster,
                        tidyselect::any_of(c("group")), total_intensity) %>% 
          tidyr::pivot_wider(names_from = "cluster", values_from = "total_intensity")
        
        return(data)
      } 
      else {
        data <- data %>% 
          dplyr::group_by(sample_name, cluster) %>% 
          dplyr::mutate(total_intensity = sum(total_area)) %>% 
          dplyr::ungroup() %>% 
          dplyr::select(sample_name, sample_id, sample_type, cluster,
                        tidyselect::any_of(c("group")), total_intensity) %>% 
          tidyr::pivot_wider(names_from = "cluster", values_from = "total_intensity")
        
        return(data)
      }
    })
    
    
    # Calculate site occupancies
    site_occupancy <- reactive({
      req(peptides_intensities(), results_normalization$normalized_data_wide())
      peptides <- peptides_table()$Peptide
      peptides <- peptides[peptides %in% colnames(peptides_intensities())]
      
      data <- results_normalization$normalized_data_wide() %>% 
        dplyr::left_join(., peptides_intensities())
      
      for (peptide in peptides) {
        formula <- create_expr_ls(paste0(
          peptide, "_site_occupancy = ", peptide, " / ", peptide, "_sum_intensity * 100"
        ))
        data <- data %>% 
          dplyr::mutate(!!! formula, .after = tidyselect::contains("sum_intensity"))
      }
      
      data <- data %>% 
        dplyr::select(-peptides)
      
      return(data)
    })
  
  
    # TODO Combine data with IgG1 quantities and/or traits
    data_combined <- reactive({
      req(site_occupancy())
      site_occupancy()
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
      } else {
        shinyjs::show("no_peptides")
        shinyjs::hide("info_clusters")
        shinyjs::hide("exclude_peptides")
        shinyjs::hide("plot")
      }
    })
    
    
  })
}




